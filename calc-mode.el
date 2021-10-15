
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
  (let ((res "") (expression ""))
    (when (string-match "<<\\(.*\\)>>" string)
      (setq expression (match-string 1 string))
      (setq res (calc-eval-string expression)))
  res))

(defun calc-eval-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (let ((region (buffer-substring-no-properties beg end)) newln)
    (setq newln (replace-regexp-in-string "<<\\(.*?\\)>>"
					  'calc-parse-template region))
    (message "%s" newln)
  ))

(defun calc-eval-and-insert-template (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-min))))
  (let ((region (buffer-substring-no-properties beg end)) beg)
    (setq newln (replace-regexp-in-string "<<\\(.*?\\)>>"
					  'calc-parse-template region))
    (when (> (length (string-trim newln)) 0)
      (goto-char end)
      (insert "\n")
      (setq beg (point))
      (insert newln)
      (uncomment-region beg (point)))
    ))

(defun calc-kill-process ()
  (interactive)
  (kill-process (concat "calc-" calc-subprocess-engine))
  )

(global-set-key (kbd "C-e") 'calc-eval-and-replace-region)
(global-set-key (kbd "C-S-e") 'calc-eval-region)

