;;; mtl-edit-mode.el --- Minor mode to edit MTL generated so that it's easier to insert/change common mistakes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  <allmanpride@gmail.com>
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


(defun pronoun-substitute (change-func &optional step)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text (downcase (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (when bounds
      (let ((subs (funcall change-func text step)))
	(when subs
	  (delete-region (car bounds) (cdr bounds))
	  (insert subs))))))

(defun pronoun-next-vt ()
  (interactive)
  (pronoun-substitute #'pronoun-change-vertical 1))

(defun pronoun-next-hz ()
  (interactive)
  (pronoun-substitute #'pronoun-change-horizontal 1))

(defun pronoun-prev-vt ()
  (interactive)
  (pronoun-substitute #'pronoun-change-vertical -1))

(defun pronoun-prev-hz ()
  (interactive)
  (pronoun-substitute #'pronoun-change-horizontal -1))

(defun seperate-lines ()
  (interactive)
  (while (re-search-forward "\n+" nil t)
    (replace-match "\n\n" nil nil )))

(defun mtl-add-chara-name ()
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (name (downcase (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (if (= (length (member name mtl-chara-names)) 0)
	(setq mtl-chara-names (cons name mtl-chara-names)))))

(defun mtl-insert-chara-name (name end)
  (interactive (list (ido-completing-read "Select Character: " mtl-chara-names)
		     (read-string "End>" ": ")))
  (insert (concat (capitalize name) end)))

(defun mtl-insert-honorifics (hon)
  (interactive (list (ido-completing-read "Honorific: " mtl-honorifics)))
  (insert (concat "-" hon)))


(define-minor-mode mtl-edit-mode
  "Mode to edit MTLs without significant effort."
  :lighter " MTL"
  :keymap `((,(kbd "l") . pronoun-next-hz)
	    (,(kbd "k") . pronoun-next-vt)
	    (,(kbd "h") . pronoun-prev-hz)
	    (,(kbd "j") . pronoun-prev-vt)
	    (,(kbd ";") . seperate-lines)
	    (,(kbd "s") . save-buffer)
	    (,(kbd "a") . mtl-add-chara-name)
	    (,(kbd "i") . mtl-insert-chara-name)
	    (,(kbd "-") . mtl-insert-honorifics)
	    ))


(provide 'mtl-edit-mode)
;;; mtl-edit-mode.el ends here
