;;; calt-mode.el --- Minor mode for calculation based templating.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; Version: 0.1
;; Keywords: calculator, templating, LaTeX

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(setq calt-subprocess-engine "python")
(setq calt-subprocess-arg "-i")

(setq calt-format-string "%.2f")

(defun calt-filter-func (proc string)
  (internal-default-process-filter proc string)
  (setq string (if (string-match ">>>" string)
		   (string-trim-right string "[ >\n]*")
		 (string-trim-right string)))
  (setq calt-last-eval-string
	 (concat calt-last-eval-string string)))


(defun calt-start-process ()
  (interactive)
  (calt-clear-last-eval-string)
  (setq calt-subprocess-process
	(start-process
	 (concat "calt-" calt-subprocess-engine)
	 (concat "*calt-" calt-subprocess-engine "*")
	 calt-subprocess-engine
	 calt-subprocess-arg
	 ))
  (set-process-query-on-exit-flag calt-subprocess-process nil)
  (set-process-filter calt-subprocess-process 'calt-filter-func)
  (message "%s process started" calt-subprocess-engine)
  )

(defun calt-clear-last-eval-string ()
  (setq calt-last-eval-string ""))


(defun calt-eval-string (string)
  (calt-clear-last-eval-string)
  (calt-send-string-to-buffer string)
  (process-send-string calt-subprocess-process (concat string "\n"))
  (accept-process-output calt-subprocess-process 2)
  calt-last-eval-string
  )

(defun calt-send-string-to-buffer (string)
    (with-current-buffer (process-buffer calt-subprocess-process)
      (goto-char (process-mark calt-subprocess-process))
      (insert (concat string "\n"))
      (set-marker (process-mark calt-subprocess-process) (point))
      (goto-char (process-mark calt-subprocess-process))
      )
    )


(defun calt-eval-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (calt-clear-last-eval-string)
  (message "Evaluating Expression. C-g to cancel.")
  (calt-send-string-to-buffer (buffer-substring-no-properties beg end))
  (process-send-region calt-subprocess-process beg end)
  (process-send-string calt-subprocess-process "\n")
  (accept-process-output calt-subprocess-process 2)
  (message calt-last-eval-string)
  )

(defun calt-eval-and-replace-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (calt-eval-region beg end)
  (kill-region beg end)
  (insert calt-last-eval-string)
  )

(defun calt-parse-template (string)
    (if (match-string 2 string)
      (let* ((expression (match-string 1 string))
	(res (calt-eval-string expression))
	(fmt (match-string 2 string)))
	(if
	    (string-match-p "=" expression)
	    ;; (member (string-to-char "=") (string-to-list expression))
	    (setq res (calt-eval-string
		       (first (split-string expression "=")))))
	(if (string-match-p "[0-9.]*[dfex]" fmt)
	    (setq res (string-to-number res))
	  (setq res (replace-quote res)))
	
	;; (message "rest: %s" res)
	;; (message "fmt: %s" fmt)
	(format (concat "%" fmt) res))
      (replace-quote (calt-eval-string (match-string 1 string)))))

(defun calt-eval-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'line)))
		   (list (first bnd) (rest bnd)))))
  (let ((region (buffer-substring-no-properties beg end)) newln)
    (setq newln (replace-regexp-in-string
		 "<<\\(.*?\\);\\([0-9.a-zA-Z]+\\)?>>"
		 'calt-parse-template region))
    ;; kill-new is new entry on kill menu
    (kill-new (message "%s" newln))
  ))

(defun calt-eval-and-insert-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'line)))
		   (list (first bnd) (rest bnd)))))
  (save-excursion
    (let ((newln (calt-eval-template beg end)) tmp)
      (when (> (length (string-trim newln)) 0)
	(goto-char (max beg end))
	(setq tmp (point))
	(insert newln)
	(uncomment-region tmp (point))))
    ))

(defun calt-stop-process ()
  (interactive)
  (kill-process (concat "calt-" calt-subprocess-engine))
  )

(defun calt-exp-to-latex (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'symbol)))
		   (list (first bnd) (rest bnd)))))
  (save-excursion
    (goto-char beg)
    (when (re-search-forward
     "\\([0-9.+-]+\\)e\\([0-9.+-]+\\)"
     end)
      (replace-match "\\1×10^{\\2}")
      )))

(defun calt-format-region-last (beg end)
      (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'symbol)))
		   (list (first bnd) (rest bnd)))))
      (let ((text (buffer-substring-no-properties beg end)))
      (if (string-match-p "%[0-9.]*[dfex]" calt-format-string)
	    (setq text (string-to-number text)))
      (kill-region beg end)
      (insert (format calt-format-string text))))

(defun calt-format-region (beg end)
    (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'symbol)))
		   (list (first bnd) (rest bnd)))))
    (let ((fmt  (read-string "Enter format string:"
			     calt-format-string)))
      (setq calt-format-string fmt)
      (calt-format-region-last beg end)))

(defun calt-increment-number (step)
  (interactive "P")
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (word (buffer-substring-no-properties
		(car bounds)
		(cdr bounds)))
	 (step (or step 1)))
    (when bounds
      (let ((ma (string-match
		 "\\(.*[^0-9]\\)\\([0-9]+\\)\\([/_.,-\"']\\)?"
		 word)))
	(when ma
	  (delete-region (car bounds) (cdr bounds))
	  (insert (concat
		   (match-string 1 word)
		   (number-to-string (+ step (string-to-number
					      (match-string 2 word))))
		   (match-string 3 word))
		  ))))))

(defun calt-eval-elisp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(setq calt-key-map (make-sparse-keymap))
(define-key calt-key-map (kbd "p") 'calt-start-process)
(define-key calt-key-map (kbd "q") 'calt-stop-process)
(define-key calt-key-map (kbd "T") 'calt-eval-template)
(define-key calt-key-map (kbd "t") 'calt-eval-and-insert-template)
(define-key calt-key-map (kbd "r") 'calt-eval-and-replace-region)
(define-key calt-key-map (kbd "R") 'calt-eval-regio)
(define-key calt-key-map (kbd "F") 'calt-format-region)
(define-key calt-key-map (kbd "f") 'calt-format-region-last)
(define-key calt-key-map (kbd "e") 'calt-eval-elisp-and-replace)
(define-key calt-key-map (kbd "+") 'calt-increment-number)
(define-key calt-key-map (kbd "l") 'calt-exp-to-latex)


(define-minor-mode calt-mode
  "Minor mode for Calculation based templating."
  :lighter " Calt"
  ;; :keymap `((,(kbd "C-e") . calt-key-map))
  )

(provide 'calt-mode)
;;; calt-mode.el ends here
