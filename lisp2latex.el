;; converts valid mathematical lisp expressions to LaTeX snippets

;; improved from https://emacs.stackexchange.com/a/70360
(defvar lisp2latex-functions '(sin cos tan))
(defvar lisp2latex-maybe-enclose? nil)

(defun lisp2latex-maybe-enclose (form)
  (let* ((lisp2latex-maybe-enclose? nil)
         (latex (lisp2latex form)))
    (if lisp2latex-maybe-enclose?
        (format "(%s)" latex)
      latex)))

(defun lisp2latex-all (form)
  "Converts given lisp expression to latex equivalent"
  (pcase form
    ;; basic operators
    (`(+ . ,args)
     (setf latex-maybe-enclose t)
     (mapconcat #'lisp2latex-all args " + "))
    
    (`(* . ,args)
     (setf latex-maybe-enclose t)
     (with-output-to-string
       (loop for (me next . rest) on args do
             (if (numberp next)
                 (princ (format "%s \\times " (lisp2latex-maybe-enclose me)))
               (princ (format "%s " (lisp2latex-maybe-enclose me)))))))     
    
    (`(/ ,a1 . ,args)
     (if args
         (format "\\frac{%s}{%s}" (lisp2latex-all a1)
                 (lisp2latex-all (cons '* args)))
       (format "\\frac1{%s}" (lisp2latex-all a1))))
    
    (`(- ,a1 . ,args)
     (if args
         (format "%s - %s" (lisp2latex-all a1)
                 (mapconcat #'lisp2latex-all args " - "))
       (format "- %s" (lisp2latex-all a1))))

    (`(expt ,base ,power)
     (if (listp base)
         (format "(%s)^{%s}" (lisp2latex-all base) (lisp2latex-all power))
       (format "%s^{%s}"  (lisp2latex-all base) (lisp2latex-all power))))

    ;; assignment operator
    (`(setq . ,args)
     (with-output-to-string 
       (loop for (a b . rest) on args by #'cddr do
             (princ (format "%s = %s" (lisp2latex-all a) (lisp2latex-all b)))
             (when rest (princ "; ")))))
    
    ;; other operators
    (`(1+ ,arg) (concat "1 + " (lisp2latex-all arg) ))
    (`(sqrt ,arg) (format "\\sqrt{%s}" (lisp2latex-all arg)))

    
    ;; named functions
    (`(,func . ,args)
     (let* ((known? (find func lisp2latex-functions))
            (enclose? (or (not known?)
                          (> (length args) 1)
                          (listp (first args))))
            (format-string (concat (if known? "\\%s" "\\mathrm{%s}")
                                   (if enclose?  "(%s)" " %s"))))
       (format format-string func (mapconcat #'lisp2latex-all args ","))))
    
    (_ (prin1-to-string form) )))



(defun contains-variables (expression)
  "gives a string from expression substituting the values."
  (if (functionp expression)
      nil
  (if (symbolp expression)
	t
      (if (consp expression)
	  (cl-some #'contains-variables expression)
      nil))))


(defun substitute-values (expression)
  "gives a string from expression substituting the values."
  (if (functionp expression)
      (format "%s" expression)
    (if (symbolp expression)
	(format "%s" (eval expression))
      (if (consp expression)
	  (format "(%s)"
		  (mapconcat #'substitute-values expression " "))
	(prin1-to-string expression)))))


(defun solve-single-step (expression)
  (let
      ((form-str (prin1-to-string expression)))
    (read
     (replace-regexp-in-string "\\(([^(^)]+)\\)"
			       (lambda (s) (format "%s" (eval (read s))))
			       form-str)))
  )



(defun lisp2latex-solve-in-steps (form)
  (let
      ((solution (list (prin1-to-string form)))) ;given expression
    (if
	(contains-variables form)
      (setq solution
	    (append solution
		    (list
		     (prin1-to-string
				  (setq form
					(read
					 (substitute-values form))))))))

  (while (consp form)
    (setq solution
	  (append solution
		  (list (prin1-to-string
			 (setq form
			       (solve-single-step form)))))))
  solution))

(provide 'lisp2latex)

