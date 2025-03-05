;; -*- lexical-binding: t; -*-

(defmacro t--parse-str (rule str &optional fail-fn)
  `(with-work-buffer
     (insert ,str)
     (goto-char (point-min))
     (peg-run (peg (and (bob) ,rule (eob))) ,fail-fn)))

(defmacro t--parse-str2 (rule str &optional fail-fn)
  `(with-work-buffer
     (insert ,str)
     (goto-char (point-min))
     (when (peg-run (peg (and (bob) ,rule (eob))) ,fail-fn)
       (buffer-substring-no-properties (point-min) (point-max)))))

(defmacro t--parse-str3 (rule str &optional fail-fn)
  `(with-work-buffer
     (insert ,str)
     (goto-char (point-min))
     (t--preprocess)
     (goto-char (point-min))
     (peg-run (peg (and (bob) ,rule (eob))) ,fail-fn)))

(ert-deftest t--cmt ()
  (dolist (s '("/**/" "/*123*/" "/*****/"
	       "/* 这是一段中文注释 */"
	       "/* //// ***** /*/"))
    (should (t--parse-str t--cmt s))))

(ert-deftest t--hex ()
  (dolist (s '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
	       "A" "B" "C" "D" "E" "F"
	       "a" "b" "c" "d" "e" "f"))
    (should (t--parse-str t--hex s)))
  (dolist (s '("bad" "cab" "bed" "feed" "deaf""ace"
	       "facade" "faced"))
    (should (t--parse-str (+ t--hex) s)))
  (dolist (s '("G" "H" "☉"))
    (should-not (t--parse-str t--hex s))))

(ert-deftest t--nl ()
  (dolist (s '("\n" "\r\n" "\r" "\f"))
    (should (t--parse-str t--nl s)))
  (dolist (s '("
" "" ""))
    (should (t--parse-str t--nl s))))

(ert-deftest t--es ()
  (let* ((fn (lambda (s) (t--parse-str2 t--es s)))
	 (ls '("\\a " "\\a\t" "\\a\n" "\\a\r\n" "\\a\r"))
	 (res (mapcar fn ls)))
    (should (equal res '("\\a " "\\a " "\\a " "\\a " "\\a ")))))

(ert-deftest t--es0 ()
  (dolist (s '("\\1" "\\11" "\\111" "\\1111" "\\11111" "\\10FFFF"
	       "\\11 " "\\111 " "\\1111 " "\\11111 " "\\10FFFF "))
    (should (t--parse-str t--es0 s)))
  (dolist (s '("\\ " "\\z" "\\你" "\\の"))
    (should (t--parse-str t--es0 s)))
  (dolist (s '("\\ " "\\a" "\\c" "\\m" "\\a\\c\\m"
	       "\\30 \\31 \\32 \\33" "\\6211\\6D4B\\4F60\\7801"
	       "\\003042\\003052\\00304A\\003081"))
    (should (t--parse-str (+ t--es0) s)))
  (dolist (s '("" "\\coffee" "\\1234567" "\\12  "))
    (should-not (t--parse-str t--es0 s))))

(ert-deftest t--str ()
  (dolist (s '("\"\"" "''" "\"123\"" "\"456\"" "'123'"
	       "'123\\\n456'" "\"123\\FACE \\\n456\""
	       "'这是一个简单的单引号字符串'"))
    (should (t--parse-str t--str s))))

(ert-deftest t--ident ()
  (dolist (s '("a" "b" "z" "F" "_" "\\30" "\\30 " "--" "我" "-我"
		 "-a" "-b" "-z" "-F" "_" "-\\30" "-\\30 "))
    (should (t--parse-str t--ident s)))
  (dolist (s '("-" ""))
    (should-not (t--parse-str t--ident s)))
  (dolist (s '("index_1" "ount_2" "total_sum" "value_1" "data_3"
	       "result_42" "temp_value" "flag_status" "user_name"
	       "item-1" "max_value" "min_value" "x_coordinate"
	       "y-coordinate" "z_axis" "input_data" "output_buffer"
	       "start_time" "end_time" "buffer_size" "counter_5"
	       "temp-flag" "user_id_123" "file-name" "user-list"
	       "connection-1" "max-limit" "timeout_30"
	       "response_code" "retry_count_3" "\\1 \\2\\3"
	       "a\\ strange\\ token"))
    (should (t--parse-str t--ident s)))
  (dolist (s '("\\1 \\2 \\3" "真的有人用中文标识符吗"
	       "λϕχ" "😇😡😊🥵"))
    (should (t--parse-str t--ident s))))

(ert-deftest t--func ()
  (dolist (s '("acos(" "asin(" "atan(" "atan2(" "attr(" "blur("
	       "brightness(" "calc(" "circle(" "clamp(" "color("
	       "color-mix(" "conic-gradient(" "contrast(" "cos("
	       "counter(" "counters(" "cubic-bezier("
	       "drop-shadow(" "ellipse(" "exp(" "fit-content("
	       "grayscale(" "hsl(" "hue-rotate(" "hwb(" "hypot("
	       "inset(" "invert(" "lab(" "lch(" "light-dark("
	       "linear-gradient(" "log(" "matrix(" "matrix3d("
	       "max(" "min(" "minmax(" "mod(" "oklab(" "oklch("
	       "opacity(" "perspective(" "polygon(" "pow("
	       "radial-gradient(" "ray(" "rem(" "repeat("
	       "repeating-conic-gradient("
	       "repeating-linear-gradient("
	       "repeating-radial-gradient(" "rgb(" "rotate("
	       "rotate3d(" "rotateX(" "rotateY(" "rotateZ("
	       "round(" "saturate(" "scale(" "scale3d(" "scaleX("
	       "scaleY(" "sepia(" "sin(" "skew(" "skewX(" "skewY("
	       "sqrt(" "steps(" "tan(" "translate(" "translateX("
	       "translateY(" "url(" "var("))
    (should (t--parse-str t--func s))))

(ert-deftest t--at ()
  (dolist (s '("@charset" "@container" "@counter-style"
	       "@font-face" "@font-palette-values" "@import"
	       "@keyframes" "@layer" "@media" "@namespace"
	       "@page" "@property" "@scope" "@starting-style"
	       "@supports"))
    (should (t--parse-str t--at s))))

(ert-deftest t--hash ()
  (dolist (s '("#123" "#234" "#abc" "#_a-b-c" "#\\ space"
	       "#人" "#ひと" "#사람" "#people" "#إنسان" "#🧑"))
    (should (t--parse-str t--hash s))))

(ert-deftest t--url ()
  (dolist (s '("url(1)"
	       "url(../../img/1.jpg)"
	       "url( https://www.w3schools.com/cssref/ )"))
    (should (t--parse-str t--url s))))

(ert-deftest t--number ()
  (dolist (s '("1" "2" "3" "314" "114514"
	       "1.1" "2.2" "3.3" "3.14" "114.514"
	       "+1.1" "-2.2" "+3.3" "-3.14" "+19.19"))
    (should (t--parse-str t--number s)))
  (dolist (s '("10e3" "10e+3" "10e-3" "10E3"))
    (should (t--parse-str t--number s))))

(ert-deftest t--dimension ()
  (dolist (s '("1cm" "1mm" "3Q" "5in" "3pc" "2pt"
	       "2px" "3rem" "1vh" "9vw"))
    (should (t--parse-str t--dimension s))))

(ert-deftest t--percent ()
  (dolist (s '("1%" "3%" "5%" "99%"))
    (should (t--parse-str t--percent s))))
	  


;; Local Variables:
;; read-symbol-shorthands: (("t-" . "minifycss-"))
;; coding: utf-8-unix
;; End:
