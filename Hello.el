(defun Hello (a &optional b &rest c)
  "A function to print 'Hello, World!'"
  nil
  (print "Hello, World!")
  )

(defun Pyramid (numLayers)
  "Output a pyramid with `numLayers` layers"
  (dotimes (i numLayers)
    (progn
      (princ (concat (make-string (- numLayers i) 32) (make-string (+ 1 (* 2 i)) ?*)))
      (terpri)
      )
    )
  )

(defun Integral (a b c func)
  "Calculate the approximate definite integral from lower bound a to upper bound b of func"
  (if (fboundp func)
      (let* ((n 0))
	(dotimes (i c (progn (princ n) (terpri)))
	  (setq n (+ n (* (funcall func (+ a (/ (float (* (- b a) i)) c))) (/ (float (- b a)) c))))
	  )
	)
    (print "error: func not a function")
    )
  )

(require 'seq) ;; required for accessing sequence functions when invoking script
(defun Portmanteus(word1 word2)
  "Find all valid portmanteus of the two given words
   according to https://codegolf.stackexchange.com/questions/167760/pleasanortmanteaus/167842#167842"
  (dotimes (i (1- (length word2)))
    (dotimes (j (1- (length word1)))
      (progn
	(setq w (substring word1 0 (1+ j)) w2 (substring word2 (1+ i)) comb (concat w w2))
	(defun isVowel (c) (seq-contains "aeiou" (elt c 0) 'char-equal))
	(if (not (or (string-prefix-p word1 comb) (string-suffix-p word2 comb)))
	  (if (isVowel (substring w -1))
	    (if (not (isVowel w2))
	      (princ (format "%s\n" comb))
	    )
	    (if (isVowel w2)
	      (princ (format "%s\n" comb))
	    )
	  )
	)
      )
    )
  )
)

;; Golfed and anonymous Portmanteus function
(lambda(a b)(dotimes(i(1-(length b)))(dotimes(j(1-(length a)))(progn(setq w(substring a 0(1+ j))x(substring b(1+ i))c(concat w x))(defun V(c)(seq-contains"aeiou"(elt c 0)'char-equal))(if(not(or(string-prefix-p a c)(string-suffix-p b c)))(if(V(substring w -1))(if(not(V x))(print c))(if(V x)(print c))))))))

(Pyramid 10)
(Integral 2 9 1000 (defun f (a) (/ 1 a)))
(Portmanteus "tiger" "lion")
;(P "harry" "ginny")
;(substring-no-properties "hello" 0 1)
