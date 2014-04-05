;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda character
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb")))

(electric-pair-mode +1)

;; Line by line scrolling
(setq scroll-step 1)

;; Enable backup files
;; Change t to nil to turn this off.
(setq make-backup-files t)

;; Save backup files to this dir
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Show column number
(column-number-mode 1)

;; Turn off blinking cursor.
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; Windows will split left-right.
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Modern computers can handle this.
(setq max-lisp-eval-depth 18000)
(setq max-specpdl-size 19500)

;; What do these do?
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'generic-x)

;; Hisp Mode
(define-generic-mode 
  'hisp-mode
  '(";")
  '("map" "filter" "foldl" "cond" "if" "else" "let" "lambda" "define" "require"
    "λ" "foldr")
  '(("(" . 'font-lock-builtin)
    (")" . 'font-lock-builtin))
  '("\\.hisp$")
  nil  ;; other functions to call
  "A simple mode for Hisp code.")

;; Scala Mode
(add-to-list 'load-path "/usr/share/emacs/scala-mode")
(require 'scala-mode-auto)

;; Elm Mode
(add-to-list 'auto-mode-alist '("\\.elm\\'" . haskell-mode))

;; Arch Linux only
(when (eq system-type 'gnu/linux)
  ;; For Racket/Scheme
  (require 'quack)
  ;; Haskell mode
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
  (require 'haskell-mode-autoloads)
;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;; PKGBUILD mode
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
  (setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun java-getter ()
  "Writes a getter function for a variable."
  (interactive)
  (let* ((line (copy-current-line))
	 (splat (split-string line))
	 (type (car splat))
	 (name (string-init (car (cdr splat))))
	 (capped (string-concat (list "get" (capitalize name)))))
    (progn
      (end-of-line)
      (newline-and-indent)
      (newline-and-indent)
      (insert (string-unwords (list "public" type capped "() {")))
      (newline-and-indent)
      (insert (string-concat (list "return " name ";}")))
      (backward-char)
      (newline-and-indent))))

(global-set-key (kbd "C-c g") 'java-getter)

;(string-init (car (cdr (split-string "wee woo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-braces ()
  "Inserts braces for quicker coding in C-like languages."
  (interactive)
  (progn
    (insert "{")
    (newline-and-indent)
    (save-excursion
      (newline)
      (insert "}")
      (beginning-of-line)
      (indent-according-to-mode))))

(global-set-key (kbd "C-c [") 'insert-braces)

(defun better-indent-region ()
  "Indents a region by major mode."
  (interactive)
  (let ((lines (lines-from-region)))
    (for-each (lambda (l) (progn
			    (insert l)
			    (beginning-of-line)
			    (indent-according-to-mode)
			    (end-of-line)
			    (newline)))
	      lines)))

(global-set-key (kbd "C-c SPC") 'better-indent-region)

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (progn
      (kill-sexp -1)
      (insert (format "%s" value)))))

(global-set-key (kbd "C-c r") 'replace-last-sexp)

(defun tex-block (name)
  "Takes a block name and creates \begin{} and \end{} tags."
  (interactive "sBlock name: ")
  (progn
    (insert (string-concat (list "\\begin{" name "}")))
    (newline)
    (save-excursion
      (newline)
      (insert (string-concat (list "\\end{" name "}"))))))

(global-set-key (kbd "C-c b") 'tex-block)

(defun numbered-list ()
  "Numbers each line in a region."
  (interactive)
  (let* ((line-pairs (list-enumerate (lines-from-region)))
	 (longest (length (int-to-string (car (list-last line-pairs))))))
    (insert (string-unlines (map (lambda (pair)
				   (let* ((num (int-to-string (car pair)))
					  (diff (- longest (length num))))
				     (string-concat (list (string-pad " " diff num)
							  ". "
							  (cdr pair)))))
				 line-pairs)))))

(global-set-key (kbd "C-c C-n") 'numbered-list)

(defun flip-lines ()
  "Flips the order of the lines in a marked area."
  (interactive)
  (insert (string-unlines (reverse (lines-from-region)))))

(global-set-key (kbd "C-c C-f") 'flip-lines)

; TODO: Make the boarder char optional.
(defun chartify ()
  "Formats a marked area into a pretty chart."
  (interactive)
  (let* ((text (filter-buffer-substring (mark) (point) 'DELETETHATSHIT))
	 (lines (string-lines text))
	 (longest (longest-word (string-words (string-unwords lines))))
	 (all-words-by-line (map 'string-words lines)))
    (print-all-chart-rows longest all-words-by-line)))
	 
(global-set-key (kbd "C-c C-c") 'chartify)

(defun longest-word (words)
  "Given a list of words, gives the length of the longest one."
  (foldl (lambda (acc word)
	   (let ((word-len (string-width word)))
	     (if (> word-len acc)
		 word-len
	       acc)))
	 0
	 words))

; (longest-word (list "apple" "bear" "winnipeg" "terrificly"))

(defun print-all-chart-rows (cell-width words-by-line)
  "Work for the function `chartify'"
  (unless (null words-by-line)
    (progn
      (print-chart-row cell-width (car words-by-line))
      (newline)
      (underline-complete "*")
      (newline)
      (print-all-chart-rows cell-width (cdr words-by-line)))))

(defun print-chart-row (cell-width words)
  "Prints a row of the chart with given words."
  (if (null words)
      (insert "*")
    (let* ((word-len (string-width (car words)))
	   (wid-diff (- cell-width word-len))
	   (left-pad (string-replicate (/ wid-diff 2) " "))
	   (right-pad (if (evenp wid-diff)
			  left-pad
			(string-replicate (1+ (/ wid-diff 2)) " "))))
      (progn
	(insert "* " left-pad (car words) right-pad " ") 
	(print-chart-row cell-width (cdr words))))))

; (print-chart-row 7 (list "yes" "boss" "shazam!"))

(defun add-def (name)
  "Quickly create a new function."
  (interactive "sFunction name: ")
  (let ((to-add (cond ((eq major-mode 'emacs-lisp-mode) (emacs-lisp-fun name))
		      ((eq major-mode 'scheme-mode) (scheme-fun name))
		      ((eq major-mode 'c-mode) (c-fun name))
		      ((eq major-mode 'java-mode) (java-fun name))
		      ((cons "(def " "")))))
    (progn
      (insert (car to-add))
      (save-excursion
	(insert (cdr to-add))))))

(defun scheme-fun (name)
  (cons (string-concat (list "(define (" name))
	"))"))

(defun emacs-lisp-fun (name)
  (cons (string-concat (list "(defun " name " ("))
	")"))

(defun c-fun (name)
  (cons (string-concat (list "type " name "("))
	") {\n}"))

(defun java-fun (name)
  (cons (string-concat (list "public static " name "("))
	") {\n}"))

(global-set-key (kbd "C-c d") 'add-def)

;; Automatic right-parenthesis completer.
(defun auto-parens ()
  "Adds as many `)' as necessary from (point) onwards.
BUG: Parens found in string and comments affect the regex match count!!!"
  (interactive)
  (let ((end (point))
	(start (find-first-paren)))
    (when start
      (let* ((lefts (count-matches "(" start end))   ; Bug here.
	     (rights (count-matches ")" start end))  ; And here.
	     (diff (- lefts rights)))
	(if (< diff 0)
	    (message "You have %d too many parentheses already." (* (- 1) diff))
	  (progn
	    (goto-char end)
	    (insert (string-replicate diff ")"))
	    (message "Inserted %d right parentheses." diff)))))))

(global-set-key (kbd "C-c C-p") 'auto-parens)
      
(defun find-first-paren ()
  "Relative to (point), finds the first instance of `(' which
appears in column zero. It's (point) is returned."
  (let ((here (point)))
    (save-excursion
      (search-backward "(")
      (if (= here (point))
	  nil  ; The search failed.
	(if (point-at-col-zerop)
	    (point)
	  (find-first-paren))))))

;; Underline Completer
(defun underline-complete (item)
  "Given a string of length 1, draws a line with that string equal in length
to the line above it.
If the current line to be inserted in is not blank, a newline will be added.
Also, if the line above is blank, nothing will happen."
  (interactive "sChar to underline with: ")
  (cond ((= 1 (line-number-at-pos)) (message "Can't. You're at line 1."))
	((not (= 1 (string-width item))) (message "Can't. Arg was too long."))
	((let ((len (save-excursion
		       (previous-line) (end-of-line) (current-column))))	       
	   (if (zerop len)
	       (message "Line above is blank.")
	     (let ((here (point-at-line-start))
		   (underline (string-replicate len item)))
	       (progn
		 (goto-char here)
		 (unless (string-emptyp (get-current-line))
		   (save-excursion
		     (newline)))
		 (insert underline))))))))

(global-set-key (kbd "C-c C-u") 'underline-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-singlep (items)
  "True if the given list only has one element."
  (= 1 (length items)))

(defun list-intersperse (x items)
  "Adds `x` between every item in the given list."
  (cond ((null items) nil)
	((list-singlep items) items)
	((cons (car items) (cons x (list-intersperse x (cdr items)))))))

(defun list-enumerate (items)
  "Pairs each element of a list with a number."
  (zip (range 1 (1+ (length items))) items))

(defun list-init (items)
  "Returns the first n-1 items in a length n list."
  (if (> 2 (length items))
      nil
    (cons (car items) (list-init (cdr items)))))

(defun list-last (items)
  "Returns the last item in a list."
  (if (null (cdr items))
      (car items)
    (list-last (cdr items))))

(defun list-elemp (x items)
  "Determines if `x' is a member of `items'."
  (if (null items)
      nil
    (if (equal x (car items))
	t
      (list-elemp x (cdr items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-emptyp (item)
  "Deteremines if a given string is empty."
  (string= item ""))

(defun string-concat (strings)
  (mapconcat 'identity strings ""))

(defun string-replicate (n item)
  "Fuses a string to itself `n' times."
  (if (< n 1)
      ""
    (concat item (string-replicate (1- n) item))))

(defun string-nth-char (n item)
  "Returns the nth char of a given string."
  (let ((chars (string-to-list item)))
    (nth n chars)))

(defun string-to-string-list (item)
  "Converts a string to a list of each char as a string."
  (mapcar 'string (string-to-list item)))

(defun string-reverse (item)
  "Reverses all the chars in a given string."
  (apply 'string (reverse (string-to-list item))))

; A whitespace is `32'
; A japanese whitespace is `12288'
(defun string-words (item)
  "Splits a string into a list of its words"
  (let ((spaces (list 32 12288)))
    (map (lambda (w) (apply 'string w))
	 (foldr (lambda (x acc)
		  (cond ((null acc) (list (list x)))
			((list-elemp x spaces) (cons nil acc))
			((cons (cons x (car acc)) (cdr acc)))))
		nil
		(string-to-list item)))))

(defun string-unwords (words)
  "Given a list of strings, fuses them via whitespace to make a sentence."
  (mapconcat 'identity words " "))

(defun string-lines (line-str)
  "Given a string with newlines, splits it by line and returns a list."
  (split-string line-str "\n" t))

(defun string-unlines (line-list)
  "Given a list of strings, joins them via newline chars."
  (mapconcat 'identity line-list "\n")) 

(defun string-init (line)
  "Returns the first n-1 chars of a length n string."
  (string-concat (list-init (string-to-string-list line))))

(defun string-pad (c n line)
  "Pads a string with `n` copies of a given char."
  (string-concat (list (string-replicate n c) line)))

(defun string-stripr (line)
  "Strips trailing whitespace."
  (when (string-match "[ \t]*$" line)
    (replace-match "" nil nil line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONAL PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun map (f items)
  (mapcar f items))

(defun foldl (f zero items)
  "Folds a list via a function `f' from left to right."
  (if (null items)
      zero
    (foldl f (funcall f zero (car items)) (cdr items))))

(defun foldr (f zero items)
  "Folds a list via a function `f' from right to left."
  (if (null items)
      zero
    (funcall f (car items) (foldr f zero (cdr items)))))

(defun filter (p items)
  "A classic filter function."
  (cond ((null items) nil)
	((funcall p (car items)) (cons (car items) (filter p (cdr items))))
	((filter p (cdr items)))))

(defun take (n items)
  "Yields the first `n` items from a list."
  (cond ((null items) nil)
	((= 0 n) nil)
	((cons (car items) (take (1- n) (cdr items))))))

(defun drop (n items)
  "Yields a list with the first `n` items removed."
  (if (= 0 n)
      items
    (drop (1- n) (cdr items))))

(defun take-while (p items)
  "Performs `take` while `p` is true on the list's elements."
  (cond ((null items) nil)
	((funcall p (car items)) (cons (car items) (take-while p (cdr items))))
	(nil)))

(defun drop-while (p items)
  "Performs `drop` while `p` is true on the list's elements."
  (cond ((null items) nil)
	((not (funcall p (car items))) items)
	((drop-while p (cdr items)))))

(defun zip-by (f l1 l2)
  "Fuses two lists together via a given function."
  (cond ((null l1) nil)
	((null l2) nil)
	((cons (funcall f (car l1) (car l2)) (zip-by f (cdr l1) (cdr l2))))))

(defun zip (l1 l2)
  "Combines two lists into a list of pairs."
  (zip-by 'cons l1 l2))

(defun sum (nums)
  "Adds all the numbers in a list."
  (foldl '+ 0 nums))

(defun product (nums)
  "Mulitplies all the numbers in a list."
  (foldl '* 1 nums))

(defun select-by (f items)
  "Selects the element in the list with the most `f`ness."
  (if (null items)
      nil
    (foldl (lambda (acc x) (funcall f acc x)) (car items) (cdr items))))

(defun maximum (nums)
  "Finds the greatest number in a list."
  (select-by 'max nums))

(defun minimum (nums)
  "Finds the lowest number in a list."
  (select-by 'min nums))

(defun group-by (p items)
  (if (null items)
      nil
    (let ((pred (lambda (x) (funcall p x (car items)))))
      (cons (take-while pred items)
	    (group-by p (drop-while pred items))))))

(defun group (items)
  "Groups by equality."
  (group-by 'equal items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMBER STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mean (nums)
  "Finds the average of a set of numbers."
  (/ (sum nums) (float (length nums))))

(defalias 'average 'mean)

(defun evenp (num)
  "Determines if a number is even."
  (= 0 (% num 2)))

(defun oddp (num)
  "Determines if a number is odd."
  (not (evenp num)))

(defun range (start end)
  "Produces a list of ints from `start' to `end' exclusive.
BUG: Recursion depth limit destroys this."
  (if (= start end)
      nil
    (cons start (range (1+ start) end))))

(defun id (x)
  "Returns what it's given."
  x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun for-each (f items)
  "Performs some action `f` on every item in the list.
The result of `f` on the last item of the list is returned."
  (if (null items)
      nil
    (progn
      (funcall f (car items))
      (for-each f (cdr items)))))

(defun do-n (n f)
  "Perform `f` `n` times."
  (if (< n 0)
      nil
    (progn
      (funcall f)
      (do-n (1- n) f))))

(defun lines-from-region ()
  "Grabs lines from a region."
  (let ((text (filter-buffer-substring (mark) (point) 'DELETE)))
    (string-lines text)))

(defun point-at-col-zerop ()
  "Determines if (point) is in column zero."
  (= 0 (current-column)))

(defun get-current-line ()
  "Returns the string of the line that `point' resides in."
  (interactive)
  (save-excursion
    (filter-buffer-substring (point-at-line-start)
			     (point-at-line-end)
			     t)))

(defun copy-current-line ()
  "Returns the string of the line that `point' resides in. Non-destructive."
  (interactive)
  (save-excursion
    (filter-buffer-substring (point-at-line-start)
			     (point-at-line-end))))

(defun point-at-line-start ()
  "Returns the `point' of the point at Column 0 in the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (point)))

(defun point-at-line-end ()
  "Returns the `point' of the point at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(quack-programs (quote ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
