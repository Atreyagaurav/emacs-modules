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
(setq mtl-insert-chara-end "")


(setq mtl-pronouns-regex
      (concat "\\b\\(" (mapconcat 'identity
		 (mapcar #'(lambda (list)
			     (concat "\\(" (mapconcat 'identity list "\\)\\|\\(") "\\)"))
			 pronouns) "\\|") "\\)\\b"))

;; Converstion to past tense rewritten from : https://github.com/kaitlynnhetzel/past_tense_generator
(setq mtl-verbs-irregular #s(hash-table test equal data ("go" "went" "buy" "bought" "break" "broke" "sit" "sat" "come""came" "eat" "ate" "sleep" "slept" "see" "saw" "pay" "paid" "sing" "sang" "tell" "told" "get" "got" "teach" "taught" "feel" "felt" "hear" "heard" "understand" "understood" "is" "was" "are" "were" "make" "made" "lose" "lost" "speak" "spoke" "say" "said")))

(setq mtl-verbs-single-form '("cut" "put" "let" "hurt" "quit" "read" "broadcast" "hit" "cost" "spread"))

(setq mtl-vowels-list '("a" "e" "i" "o" "u" "y"))

(setq mtl-verbs-end-hushers '("x" "lk" "sh" "ch" "ck" "h" "k" "nt" "lp" "wn" "st"))


(defun add-double-consonant (verb)
  (let ((chars (loop for c across verb collect (string c)))
	(count (apply #'+
		      (loop for c across verb collect
			    (if (member (string c) mtl-vowels-list)
				1 0)))))
    (if (> count 1)
	verb
      (let ((rev-chars (reverse chars)))
	(if (member (first rev-chars) mtl-verbs-end-hushers)
	    verb
	  (if (member (concat (first (rest rev-chars)) (first rev-chars)) mtl-verbs-end-hushers)
	      verb
	    (if (string= (first (rest rev-chars)) (first rev-chars))
		verb
	      (concat verb (first rev-chars))))))
      )))


(defun convert-to-past-tense (verb)
  (let ((from-hash (gethash verb mtl-verbs-irregular)))
    (if from-hash
	from-hash
      (if (member verb mtl-verbs-single-form)
	  verb
	(let* ((word-chars (loop for c across verb collect (string c)))
	       (rev-word-chars (reverse word-chars)))
	  (if (string= (first rev-word-chars) "e")
	      (concat verb "d")
	    (if (string= (first rev-word-chars) "y")
		(if (member (first (rest rev-word-chars)) mtl-vowels-list)
		    (concat (add-double-consonant verb) "ed")
	      (concat (apply #'concat (reverse (rest rev-word-chars))) "ied")))))))))


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


(defun mtl-insert-chara-name (name)
  "Insert character's name from the list."
  (interactive (list (ido-completing-read "Insert Character: " mtl-chara-names)))
  (mtl-add-chara-name name)
  (insert (concat (capitalize name) mtl-insert-chara-end)))


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

(defun mtl-goto-next-dialogue ()
  "FIXME Temp func; Move the cursor to next dialogue."
  (interactive)
  ;; \n\([A-Za-z_?-]+: \)[[] didn't work idk why.
  (when (re-search-forward "\n[[]" nil t 1)
    (move-beginning-of-line nil)))

(defun mtl-goto-prev-dialogue ()
  "FIXME Temp func; Move the cursor to prev dialogue."
  (interactive)
  ;; \n\([A-Za-z_?-]+: \)[[] didn't work idk why.
  ;; (let ((line-num (thing-at-point 'line t)))
  ;;   (message line-num))
  (when (re-search-backward "[]]" nil t 1)
    (move-beginning-of-line nil)))

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

(defun mtl-past-tense ()
  "Convert the current verb to past tense."
  (interactive)
  (map-current-word #'convert-to-past-tense))


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
	    (,(kbd ">") . mtl-goto-next-dialogue)
	    (,(kbd "<") . mtl-goto-prev-dialogue)
	    (,(kbd "v") . mtl-past-tense)
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
