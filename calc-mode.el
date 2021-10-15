
(setq calc-subprocess-engine "python")
(setq calc-subprocess-arg "-i")

(defun calc-filter-func (proc string)
  (internal-default-process-filter proc string)
  (setq string (if (string-match ">>>" string)
		   (string-trim-right string "[ >\n]*")
		 (string-trim-right string)))
  (setq calc-last-eval-string
	 (concat calc-last-eval-string string)))


(defun calc-start-process ()
  (interactive)
  (calc-clear-last-eval-string)
  (setq calc-subprocess-process (start-process
				 (concat "calc-" calc-subprocess-engine)
				 (concat "*calc-" calc-subprocess-engine "*")
				 calc-subprocess-engine
				 calc-subprocess-arg
				 ))
  
  (set-process-filter calc-subprocess-process 'calc-filter-func)
  ;; (process-send-string calc-subprocess-process "import sys\nsys.ps1='>>>'\n")
  )

(defun calc-clear-last-eval-string ()
  (setq calc-last-eval-string ""))


(defun calc-eval-string (string)
  (calc-clear-last-eval-string)
  (calc-send-string-to-buffer string)
  (process-send-string calc-subprocess-process (concat string "\n"))
  (accept-process-output calc-subprocess-process 2)
  calc-last-eval-string
  )

(defun calc-send-string-to-buffer (string)
    (with-current-buffer (process-buffer calc-subprocess-process)
      (goto-char (process-mark calc-subprocess-process))
      (insert (concat string "\n"))
      (set-marker (process-mark calc-subprocess-process) (point))
      (goto-char (process-mark calc-subprocess-process))
      )
    )


(defun calc-eval-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (calc-clear-last-eval-string)
  (message "Evaluating Expression. C-g to cancel.")
  (calc-send-string-to-buffer (buffer-substring-no-properties beg end))
  (process-send-region calc-subprocess-process beg end)
  (process-send-string calc-subprocess-process "\n")
  (accept-process-output calc-subprocess-process 2)
  (message calc-last-eval-string)
  )

(defun calc-eval-and-replace-region (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (calc-eval-region beg end)
  (kill-region beg end)
  (insert calc-last-eval-string)
  )

(defun calc-parse-template (string)
  (let ((res "") (expression "") (fmt "%s"))
    (if (match-string 2 string)
      (let* ((expression (match-string 1 string))
	(res (calc-eval-string expression))
	(fmt (match-string 2 string)))
	(if (string-match-p "[0-9.]*[df]" fmt)
	    (setq res (string-to-number res)))
	(format (concat "%" fmt) res))
      (calc-eval-string (match-string 1 string)))))

(defun calc-eval-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (let ((region (buffer-substring-no-properties beg end)) newln)
    (setq newln (replace-regexp-in-string
		 "<<\\(.*?\\);\\([0-9.a-zA-Z]+\\)?>>"
		 'calc-parse-template region))
    (message "%s" newln)
  ))

(defun calc-eval-and-insert-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (save-excursion
    (let ((newln (calc-eval-template beg end)) tmp)
      (when (> (length (string-trim newln)) 0)
      (insert "\n")
      (setq tmp (point))
      (insert newln)
      (uncomment-region tmp (point))))
    ))

(defun calc-kill-process ()
  (interactive)
  (kill-process (concat "calc-" calc-subprocess-engine))
  )

(global-set-key (kbd "C-e") 'calc-eval-and-replace-region)
(global-set-key (kbd "C-S-e") 'calc-eval-region)

