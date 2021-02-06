;;; mtl-edit-mode.el --- Minor mode to edit MTL generated so that it's easier to insert/change common mistakes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; Version: 0.1
;; Keywords: docs, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'help-macro)

(setq pronouns (list
		'("she" "her" "her" "herself")
		'("he" "him" "his" "himself")
		'("i" "me" "my" "myself")
		'("we" "us" "our" "ourselves")
		'("they" "them" "their" "themselves")
		'("it" "it" "its" "itself")
		'("you" "you" "your" "yourself")))

(setq mtl-chara-names (list "???"))
(setq mtl-honorifics (list "kun" "sama" "san" "chan" "dono"))


(setq mtl-pronouns-regex
      (concat "\\b\\(" (mapconcat 'identity
		 (mapcar #'(lambda (list)
			     (concat "\\(" (mapconcat 'identity list "\\)\\|\\(") "\\)"))
			 pronouns) "\\|") "\\)\\b"))

(defun pronoun-change-horizontal (word &optional step)
  (let* ((g (find-if #'(lambda (list)
			 (member word list))
		     pronouns))
	 (i (position word g :test #'string-equal)))
    (when i
      (let* ((step (or step 0))
	    (len (length g))
	    (next (mod (+ step i) len)))
	(loop while (string= word (nth next g))
	      do (setq next (mod (+ step next) len)))
	(nth next g)))))


(defun pronoun-change-vertical (word &optional step)
  (let* ((n (position-if #'(lambda (list)
			     (member word list))
			 pronouns)))
    (when n
      (let
	 ((group (nth n pronouns))
	 (next-group (nth (mod (+ (or step 0) n) (length pronouns))
			  pronouns)))
	(nth (position word group :test #'string-equal) next-group)))))


(defun map-current-word (change-func &rest args)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text (downcase (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (when bounds
      (let ((subs (apply change-func text args)))
	(when subs
	  (delete-region (car bounds) (cdr bounds))
	  (insert subs))))))

;; INTERACTIVE functions from here onwards

(defun seperate-lines ()
  "Remove extra nextline characters."
  (interactive)
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n\n" nil nil )))


(defun mtl-add-chara-name (name)
  "Add the character name to the local list."
  (interactive (list
		    (let* ((bounds
			    (if (use-region-p)
				(cons (region-beginning) (region-end))
			      (bounds-of-thing-at-point 'symbol))))
		      (downcase (buffer-substring-no-properties
				 (car bounds) (cdr bounds))))))
    (if (= (length (member name mtl-chara-names)) 0)
	(message "Characters: %s" (setq mtl-chara-names (cons name mtl-chara-names)))))


(defun mtl-insert-chara-name (name end)
  "Insert character's name from the list."
  (interactive (list (ido-completing-read "Insert Character: " mtl-chara-names)
		     (ido-completing-read "End>" '("" ": " " " "'s"))))
  (mtl-add-chara-name name)
  (insert (concat (capitalize name) end)))


(defun mtl-remove-chara-name (name)
  "Remove character's name from the list."
  (interactive (list (ido-completing-read "Select Character: " mtl-chara-names)))
  (message "Characters: %s" (setq mtl-chara-names (remove name mtl-chara-names))))


(defun mtl-insert-honorifics (hon)
  "Insert honorifics (sama, kun, san, etc) in point."
  (interactive (list (ido-completing-read "Honorific: " mtl-honorifics)))
  (insert (concat "-" hon)))


(defun mtl-goto-next-pronoun ()
  "Move the cursor to the next pronoun in the text."
  (interactive)
  (re-search-forward mtl-pronouns-regex nil t 1)
  ;; (backward-char 1)
  )

(defun mtl-goto-prev-pronoun ()
  "Move the cursor to the previous pronoun in the text."
  (interactive)
  (re-search-backward mtl-pronouns-regex nil t 1)
  ;; (forward-char 1)
  )

(defun mtl-insert-text (text)
  (interactive "sEnter Text:")
  (insert text))

(defun pronoun-next-vt ()
  "Change pronoun to next vertically."
  (interactive)
  (map-current-word #'pronoun-change-vertical 1))

(defun pronoun-next-hz ()
  "Change pronoun to next horizontally."
  (interactive)
  (map-current-word #'pronoun-change-horizontal 1))

(defun pronoun-prev-vt ()
  "Change pronoun to previous vertically."
  (interactive)
  (map-current-word #'pronoun-change-vertical -1))

(defun pronoun-prev-hz ()
  "Change pronoun to previous horizontally."
  (interactive)
  (map-current-word #'pronoun-change-horizontal -1))

(defun mtl-upcase ()
  "upcase current word."
  (interactive)
  (map-current-word #'upcase))

(defun mtl-downcase ()
  "downcase current word."
  (interactive)
  (map-current-word #'downcase))

(defun mtl-capitalize ()
  "capitalize current word."
  (interactive)
  (map-current-word #'capitalize))


(define-minor-mode mtl-edit-mode
  "Mode to edit MTLs without significant effort."
  :lighter " MTL"
  :keymap `((,(kbd "l") . pronoun-next-hz)
	    (,(kbd "k") . pronoun-next-vt)
	    (,(kbd "h") . pronoun-prev-hz)
	    (,(kbd "j") . pronoun-prev-vt)
	    (,(kbd "n") . mtl-goto-next-pronoun)
	    (,(kbd "p") . mtl-goto-prev-pronoun)
	    (,(kbd ";") . seperate-lines)
	    (,(kbd "s") . save-buffer)
	    (,(kbd "a") . mtl-add-chara-name)
	    (,(kbd "i") . mtl-insert-chara-name)
	    (,(kbd "-") . mtl-insert-honorifics)
	    (,(kbd "c") . mtl-capitalize)
	    (,(kbd "u") . mtl-upcase)
	    (,(kbd "d") . mtl-downcase)
	    (,(kbd "r") . mtl-remove-chara-name)
	    (,(kbd "t") . mtl-insert-text)
	    (,(kbd "?") . mtl-edit-help)
	    )
  )

(defun mtl-edit-help ()
  (interactive)
  (with-temp-buffer
    (map-keymap (lambda (kb f)
		  (princ (key-description (where-is-internal f mtl-edit-mode-map t)))
		  (princ "  -> ")(princ f)(princ "\n")
		  ;; (princ "\t(")
		  ;; (princ (documentation f))
		  ;; (princ ")\n")
		  ) mtl-edit-mode-map)))


(provide 'mtl-edit-mode)
;;; mtl-edit-mode.el ends here
