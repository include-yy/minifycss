;; minifycss.el --- Minifying CSS file -*- lexical-binding: t; -*-
;;; https://www.w3.org/TR/css-syntax-3/

;; Copyright (C) 2025 include-yy

;; Author: include-yy <yy@egh0bww1.com>
;; Maintainer: include-yy <yy@egh0bww1.com>
;; Version: 0.1
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package uses peg.el to parse and minify CSS files or text
;; strings. The main referenced CSS standards are as follows:
;;
;; - https://www.w3.org/TR/2021/CRD-css-syntax-3-20211224/
;; - https://www.w3.org/TR/2022/WD-selectors-4-20221111/
;; - https://www.w3.org/TR/2024/WD-css-values-4-20240312/

;;; Code:

(require 'peg)
;; bug#76555
(setf (get 'define-peg-rule 'lisp-indent-function) 2)

;;; Comments, escapes and whitespaces.

;; https://stackoverflow.com/a/13014995
;; "/\\*.*?\\*/"
;; or /\*\(?:[^*]|\*+[^*/]\)*\*+/
(defvar t--rx-cmt "/\\*\\(?:.\\|\n\\)*?\\*/")
;; comment
(define-peg-rule t--cmt () "/*" (* (not "*/") (any)) "*/")
;; whitespaces
(define-peg-rule t--nl  () (or "\n" "\r\n" "\r" "\f"))
(define-peg-rule t--ws  () (or [" \t"] t--nl))
(define-peg-rule t--ws* () (* t--ws))
(define-peg-rule t--ws+ () (+ t--ws))
(define-peg-rule t--wsn () (+ t--ws) `(-- 'ws))
;; symbols
(define-peg-rule t--comma () t--ws* "," t--ws*)
;; hex character
(define-peg-rule t--hex () [0-9 a-f A-F])
;; escape
(define-peg-rule t--es0 ()
  "\\" (or (and (not (or t--hex t--nl)) (any))
	   (and t--hex (opt t--hex) (opt t--hex)
		(opt t--hex) (opt t--hex) (opt t--hex)
		(opt t--ws))))
(define-peg-rule t--es ()
  "\\" (or (and (not (or t--hex t--nl)) (any))
	   (and t--hex (opt t--hex) (opt t--hex)
		(opt t--hex) (opt t--hex) (opt t--hex)
		(opt (replace t--ws " ")))))
;; string
(define-peg-rule t--str ()
  (substring
   (or
    (and "\""
	 (* (or (and (not (or ["\"\\"] t--nl)) (any))
		(and "\\" t--nl) t--es0))
	 "\"")
    (and "'"
	 (* (or (and (not (or ["'\\"] t--nl)) (any))
		(and "\\" t--nl) t--es0))
	 "'")))
  `(str -- (list 'str str)))

(defun t--preprocess ()
  "Remove all comments."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward minifycss--rx-cmt nil t)
      (replace-match ""))))

(define-peg-rule t--non-ascii ()
  (or (char #x00B7) (range #x00C0 #x00D6) (range #x00D8 #x00F6)
      (range #x00F8 #x037D) (range #x037F #x1FFF)
      (char #x200C) (char #x200D) (char #x203F) (char #x2040)
      (range #x2070 #x218F) (range #x2C00 #x2FEF)
      (range #x3001 #xD7FF) (range #xF900 #xFDCF)
      (range #xFDF0 #xFFFD) (range #x10000 #x10FFFF)))
(define-peg-rule t--non-print ()
  (or (range #x0000 #x0008) (char #x000B)
      (range #x000E #x001F) (char #x007F)))
(define-peg-rule t--ident ()
  (substring
   (or "--"
       (and (opt "-")
	    (or [a-z A-Z "_"] t--non-ascii t--es)))
   (* (or t--es [a-z A-Z 0-9 "-_"] t--non-ascii))))
(define-peg-rule t--ident2 ()
  t--ident `(id -- (list 'id id)))
(define-peg-rule t--func () t--ident "(")
(define-peg-rule t--at () "@" t--ident)
(define-peg-rule t--hash ()
  "#" (substring (+ (or t--es
			[a-z A-Z 0-9 "-_"]
			t--non-ascii)))
  `(it -- (list 'hash it)))
(define-peg-rule t--url ()
  "url(" t--ws*
  (substring (* (not (or ["\"'()"] t--ws t--non-print))
		(any)))
  t--ws* ")"
  `(url -- (list 'url (format "url(%s)" url))))
(define-peg-rule t--number ()
  (substring
   (opt ["+-"])
   (or (and (+ [digit]) "." (+ [digit]))
       (+ [digit])
       (and "." (+ [digit])))
   (opt ["eE"] (opt ["+-"]) (+ [digit]))))
(define-peg-rule t--number2 ()
  t--number `(num -- (list 'num num)))
(define-peg-rule t--dimension ()
  t--number t--ident `(num tok -- (list 'dim num tok)))
(define-peg-rule t--percent ()
  t--number "%" `(num -- (list '% num)))

(define-peg-rule t--stylesheet ()
  (bob) (list (* (or t--ws+ t--at-rule t--qu-rule))) (eob))
(define-peg-rule t--at-rule ()
  t--at t--ws*
  (list (* t--component)) (or t--block (substring ";"))
  `(at items last -- (list 'at at items last)))
(define-peg-rule t--qu-rule ()
  (list (opt (* (not t--block) t--component))) t--block
  `(items block -- (list 'qu items block)))
(define-peg-rule t--block ()
  "{" t--ws*
  (list (and (* (or t--at-rule t--qu-rule
		    (and t--decl ";"))
		t--ws*)
	     ;; last decl can omit semicolon
	     (opt t--decl)))
  t--ws* "}"
  `(items -- (cons 'b items)))
(define-peg-rule t--decl ()
  t--ident t--ws* ":" t--ws*
  (list (* t--component)) (list (opt t--imp))
  `(id items imp -- (list 'decl id items (if imp t nil))))
(define-peg-rule t--imp ()
  "!" t--ws* "important" t--ws* `(-- t))
(define-peg-rule t--preserved ()
  (or t--dimension t--hash t--percent t--number2
      t--ident2 t--str t--wsn (substring [",.:=>|+-*"])))
(define-peg-rule t--component ()
  (or t--url t--function-block t--preserved
      t--simple-block))
(define-peg-rule t--m-block ()
  "[" (list (* t--component)) "]"
  `(items -- (list 'sb items)))
(define-peg-rule t--s-block ()
  "(" (list (* t--component)) ")"
  `(items -- (list 'mb items)))
(define-peg-rule t--simple-block ()
  (or t--m-block t--s-block))
(define-peg-rule t--function-block ()
  t--func (list (* t--component)) ")"
  `(fun items -- (list 'fb fun items)))

(defun t--parse-buffer (&optional buffer)
  "Delete all comment and parse CSS buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (t--preprocess)
      (goto-char (point-min))
      (when-let* ((result (peg-run (peg t--stylesheet))))
	(cons 'css (car result))))))

(defun t-parse (buffer &optional outbuf)
  "Parse BUFFER and output result."
  (with-work-buffer
    (insert-buffer buffer)
    (when-let* ((res (t--parse-buffer )))
      (when outbuf (pp res outbuf))
      res)))

(defun t-parse-string (str &optional outbuf)
  (with-work-buffer
    (insert str)
    (when-let* ((res (t--parse-buffer)))
      (when outbuf (pp res outbuf))
      res)))

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "minifycss-"))
;; coding: utf-8-unix
;; fill-column: 72
;; End:
