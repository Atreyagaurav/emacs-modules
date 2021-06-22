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
;; Irregular Verb list from https://www.gingersoftware.com/content/grammar-rules/verbs/list-of-irregular-verbs/
(setq mtl-verbs-irregular #s(hash-table test equal data
					("arise"	"arose"
					 "awake"	"awoke"
					 "be"	"was"
					 "is" "was"
					 "are" "were"
					 "has" "had"
					 "bear"	"bore"
					 "beat"	"beat"
					 "become"	"became"
					 "begin"	"began"
					 "bend"	"bent"
					 "bet"	"bet"
					 "bind"	"bound"
					 "bite"	"bit"
					 "bleed"	"bled"
					 "blow"	"blew"
					 "break"	"broke"
					 "breed"	"bred"
					 "bring"	"brought"
					 "broadcast"	"broadcast"
					 "build"	"built"
					 "burst"	"burst"
					 "buy"	"bought"
					 "can"	"could"
					 "catch"	"caught"
					 "choose"	"chose"
					 "cling"	"clung"
					 "come"	"came"
					 "cost"	"cost"
					 "creep"	"crept"
					 "cut"	"cut"
					 "deal"	"dealt"
					 "dig"	"dug"
					 "do"	"did"
					 "does" "did"
					 "draw"	"drew"
					 "drink"	"drank"
					 "drive"	"drove"
					 "eat"	"ate"
					 "fall"	"fell"
					 "feed"	"fed"
					 "feel"	"felt"
					 "fight"	"fought"
					 "find"	"found"
					 "fly"	"flew"
					 "forbid"	"forbade"
					 "forget"	"forgot"
					 "forgive"	"forgave"
					 "freeze"	"froze"
					 "get"	"got"
					 "give"	"gave"
					 "go"	"went"
					 "grind"	"ground"
					 "grow"	"grew"
					 "hang"	"hung"
					 "have"	"had"
					 "hear"	"heard"
					 "hide"	"hid"
					 "hit"	"hit"
					 "hold"	"held"
					 "hurt"	"hurt"
					 "keep"	"kept"
					 "kneel"	"knelt"
					 "know"	"knew"
					 "lay"	"laid"
					 "lead"	"led"
					 "leave"	"left"
					 "lend"	"lent"
					 "let" "let"
					 "lie" 	"lay"
					 "light"	"lit"
					 "lose"	"lost"
					 "make"	"made"
					 "may"	"might"
					 "mean"	"meant"
					 "meet"	"met"
					 "mow"	"mowed"
					 "must"	"had to"
					 "overtake"	"overtook"
					 "pay"	"paid"
					 "put"	"put"
					 "quit" "quit"
					 "read"	"read"
					 "ride"	"rode"
					 "ring"	"rang"
					 "rise"	"rose"
					 "run"	"ran"
					 "saw"	"sawed"
					 "say"	"said"
					 "see"	"saw"
					 "sell"	"sold"
					 "send"	"sent"
					 "set"	"set"
					 "sew"	"sewed"
					 "shake"	"shook"
					 "shall"	"should"
					 "shed"	"shed"
					 "shine"	"shone"
					 "shoot"	"shot"
					 "show"	"showed"
					 "shrink"	"shrank"
					 "shut"	"shut"
					 "sing"	"sang"
					 "sink"	"sank"
					 "sit"	"sat"
					 "sleep"	"slept"
					 "slide"	"slid"
					 "smell"	"smelt"
					 "sow"	"sowed"
					 "speak"	"spoke"
					 "spend"	"spent"
					 "spit"	"spat"
					 "spread"	"spread"
					 "stand"	"stood"
					 "steal"	"stole"
					 "stick"	"stuck"
					 "sting"	"stung"
					 "stink"	"stank"
					 "strike"	"struck"
					 "swear"	"swore"
					 "sweep"	"swept"
					 "swell"	"swelled"
					 "swim"	"swam"
					 "swing"	"swung"
					 "take"	"took"
					 "teach"	"taught"
					 "tear"	"tore"
					 "tell"	"told"
					 "think"	"thought"
					 "throw"	"threw"
					 "understand"	"understood"
					 "wake"	"woke"
					 "wear"	"wore"
					 "weep"	"wept"
					 "will"	"would"
					 "win"	"won"
					 "wind"	"wound"
					 "write"	"wrote")))

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
	(let* ((word-chars (loop for c across verb collect (string c)))
	       (rev-word-chars (reverse word-chars)))
	  (if (string= (first rev-word-chars) "e")
	      (concat verb "d")
	    (if (string= (first rev-word-chars) "y")
		(if (member (first (rest rev-word-chars)) mtl-vowels-list)
		    (concat (add-double-consonant verb) "ed")
		  (concat (apply #'concat (reverse (rest rev-word-chars))) "ied"))
	      (concat verb "ed")))))))


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
  (move-end-of-line nil)
  (when (re-search-forward ":" nil t 1)
    (move-beginning-of-line nil)))

(defun mtl-goto-prev-dialogue ()
  "FIXME Temp func; Move the cursor to prev dialogue."
  (interactive)
  ;; \n\([A-Za-z_?-]+: \)[[] didn't work idk why.
  ;; (let ((line-num (thing-at-point 'line t)))
  ;;   (message line-num))
  (when (re-search-backward ":" nil t 1)
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
