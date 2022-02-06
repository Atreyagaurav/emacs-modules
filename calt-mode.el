;;; calt-mode.el --- Minor mode for calculation based templating.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; Version: 0.2
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

(defun calt-eval-and-insert-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (calt-eval-region beg end)
  (goto-char end)
  (insert " = ")
  (insert calt-last-eval-string)
  )

(defun calt-parse-template (string)
  (save-match-data
    ;; string-match-p shouldn't modify match data but it was being
    ;; modified somewhere, so this will prevent the bug.
    (if (match-string 2 string)
      (let* ((expression (match-string 1 string))
	(res (calt-eval-string expression))
	(fmt (match-string 2 string)))
	(if
	    (string-match-p "=" expression)
	    (setq res (calt-eval-string ; gets the value of last assigned variable.
		       (cl-first (split-string expression "=")))))
	(if (string-match-p "[0-9.]*[dfex]" fmt)
	    (setq res (string-to-number res)) ;if the format is number
					      ;we need to convert it.
	  (setq res (replace-quote res)))
	(format (concat "%" fmt) res))
      (replace-quote (calt-eval-string (match-string 1 string))))))

(defun calt-eval-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'line)))
		   (list (cl-first bnd) (cl-rest bnd)))))
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
		   (list (cl-first bnd) (cl-rest bnd)))))
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


(defun calt-xx-string (beg end)
  (interactive "r")
  (delete-region beg end)
  (goto-char beg)
  (loop repeat (- end beg) do (insert "X"))
  )


(setq calt-key-map (make-sparse-keymap))
(define-key calt-key-map (kbd "p") 'calt-start-process)
(define-key calt-key-map (kbd "q") 'calt-stop-process)
(define-key calt-key-map (kbd "T") 'calt-eval-template)
(define-key calt-key-map (kbd "t") 'calt-eval-and-insert-template)
(define-key calt-key-map (kbd "R") 'calt-eval-and-replace-region)
(define-key calt-key-map (kbd "r") 'calt-eval-and-insert-region)
(define-key calt-key-map (kbd "x") 'calt-xx-string)

;; things it modifies for the current mode


(define-minor-mode calt-mode
  "Minor mode for Calculation based templating."
  :lighter " Calt"
  ;; :keymap `((,(kbd "C-e") . calt-key-map))
  )

(provide 'calt-mode)
;;; calt-mode.el ends here

