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

(defun lisp2latex (form)
  "Converts given lisp expression to latex equivalent"
  (pcase form
    ;; basic operators
    (`(+ . ,args)
     (setf latex-maybe-enclose t)
     (mapconcat #'lisp2latex args " + "))
    
    (`(* . ,args)
     (setf latex-maybe-enclose t)
     (with-output-to-string
       (loop for (me next . rest) on args do
             (if (numberp next)
                 (princ (format "%s \\times " (lisp2latex-maybe-enclose me)))
               (princ (format "%s " (lisp2latex-maybe-enclose me)))))))     
    
    (`(/ ,a1 . ,args)
     (if args
         (format "\\frac{%s}{%s}" (lisp2latex a1)
                 (lisp2latex (cons '* args)))
       (format "\\frac1{%s}" (lisp2latex a1))))
    
    (`(- ,a1 . ,args)
     (if args
         (format "%s - %s" (lisp2latex a1)
                 (mapconcat #'lisp2latex args " - "))
       (format "- %s" (lisp2latex a1))))

    (`(expt ,base ,power)
     (if (listp base)
         (format "(%s)^{%s}" (lisp2latex base) (lisp2latex power))
       (format "%s^{%s}"  (lisp2latex base) (lisp2latex power))))

    ;; assignment operator
    (`(setq . ,args)
     (with-output-to-string 
       (loop for (a b . rest) on args by #'cddr do
             (princ (format "%s = %s" (lisp2latex a) (lisp2latex b)))
             (when rest (princ "; ")))))
    
    ;; other operators
    (`(1+ ,arg) (concat "1 + " (lisp2latex arg) ))
    
    (`(,func . ,args)
     (let* ((known? (find func lisp2latex-functions))
            (enclose? (or (not known?)
                          (> (length args) 1)
                          (listp (first args))))
            (format-string (concat (if known? "\\%s" "\\mathrm{%s}")
                                   (if enclose?  "(%s)" " %s"))))
       (format format-string func (mapconcat #'lisp2latex args ","))))
    
    (_ (prin1-to-string form) )))

(defun lisp-to-latex ()
  (interactive)
  (backward-kill-sexp)
  (insert (lisp2latex (read (current-kill 0)))))
