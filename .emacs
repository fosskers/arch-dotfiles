;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb")))

(electric-pair-mode +1)

;; Line by line scrolling
(setq scroll-step 1)

;; Enable backup files
(setq make-backup-files t)  ;; Change t to nil to turn this off.

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

;; Is this safe?
(setq max-lisp-eval-depth 1500)

;; What do these do?
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme mode for Hisp and Racket
(add-to-list 'auto-mode-alist '("\\.hisp\\'" . scheme-mode))
(require 'quack)
;; (add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;; Haskell mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(require 'haskell-mode-autoloads)
;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; PKGBUILD mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Takes a set of lines and turns it into a numbered list."
  (interactive)
  (defun number (curr rest)
    (if (null rest)
	nil
      (let* ((as-s  (int-to-string curr))
	     (fused (concat as-s ". " (car rest))))
	(cons fused (number (1+ curr) (cdr rest))))))
  (let ((lines (lines-from-region)))
    (insert (string-unlines (number 1 lines)))))

(global-set-key (kbd "C-c C-n") 'numbered-list)

(defun flip-lines ()
  "Flips the order of the lines in a marked area."
  (interactive)
  (insert (string-unlines (reverse (lines-from-region)))))

(global-set-key (kbd "C-c C-f") 'flip-lines)

(defun lines-from-region ()
  "Grabs lines from a region."
  (let ((text (filter-buffer-substring (mark) (point) 'DELETE)))
    (string-lines text)))

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

(defun js-frame ()
  "Inserts basic HTML to be filled with Javascript."
  (interactive)
  (let ((top "<html><body><script type=\"text/javascript\">")
	(bottom "</script></body></html>"))
    (progn
      (insert top)
      (newline)
      (newline)
      (save-excursion
	(newline)
	(newline)
	(insert bottom)))))

(global-set-key (kbd "C-c j") 'js-frame)

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

"
TEST YEAH BABY WIN MOOSE
THIS IS THE BEST EVER
OMG LIKE THREE WHOLE LINE

日本語　机　椅子
自宅　爆発　物理的
答え　無念　新幹線
四字熟語　あら　大変
"

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
	   (left-pad (replicate-string (/ wid-diff 2) " "))
	   (right-pad (if (evenp wid-diff)
			  left-pad
			(replicate-string (1+ (/ wid-diff 2)) " "))))
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
		      ((list "(def " "")))))
    (progn
      (insert (car to-add))
      (save-excursion
	(insert (car (cdr to-add)))))))

(defun scheme-fun (name)
  (list (string-concat (list "(define (" name))
	"))"))

(defun emacs-lisp-fun (name)
  (list (string-concat (list "(defun " name " ("))
	")"))

(defun c-fun (name)
  (list (string-concat (list name "("))
	") {\n}"))

(defun java-fun (name)
  (list (string-concat (list "public static " name "("))
	") {\n}"))

(global-set-key (kbd "C-c d") 'add-def)

(defun bill-template ()
  "Adds a template for a fresh month in my bills file."
  (interactive)
  (progn
    (save-excursion
      (insert "(- (+ ; Salary
      ; Reimbursment
   )
   85000  ; Savings
   ; Bills
   (+ ; Cell
      ; Internet
      ; Denki
      ; Gas
      ; Rent
   )
   ; Spending
   (+ ; Food
      ; Non-food
   ))")
    (end-of-line))))    

(global-set-key (kbd "C-c C-t") 'bill-template)

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
	    (insert (replicate-string diff ")"))
	    (message "Inserted %d right parentheses." diff)))))))

(global-set-key (kbd "C-c C-p") 'auto-parens)
      
(defun find-first-paren ()
  "Relative to (point), finds the first instance of `(' which
appears in column zero. It's (point) is returned."
  (save-excursion
    (let ((here (point)))
      (progn
	(search-backward "(")
	(if (= here (point))
	    nil  ; The search failed.
	  (if (point-at-col-zerop)
	      (point)
	    (find-first-paren)))))))

(defun point-at-col-zerop ()
  "Determines if (point) is in column zero."
  (= 0 (current-column)))

(defun replicate-string (n item)
  "Fuses a string to itself `n' times."
  (if (< n 1)
      ""
    (concat item (replicate-string (1- n) item))))

; (replicate-string 5 "*")
; (replicate-string 0 "*")

; Is this used?
(defun insert-replicated-string (n some-char)
  "Writes a line made up of `some-char' of length `n'."
  (interactive "p\nsSupply a char: ")
  (insert (replicate-string n some-char)))

;; Custom `progn'
(defun my-progn (&rest funs)
  "Takes a list of function calls and executes them 
__while retaining a functional code structure__.
You MUST quote all argument lists to this function."
  (progn-work (reverse funs)))

(defun progn-work (funs)
  "Does the work."
  (when funs
    (apply (car (car funs))
	   (discard-second (cdr (car funs))
			   (progn-work (cdr funs))))))

(defun discard-second (first second)
  "Returns its first argument."
  first)

;; TEST

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
		   (underline (replicate-string len item)))		   
	       (progn
		 (goto-char here)
		 (unless (string-emptyp (get-current-line))
		   (save-excursion
		     (newline)))
		 (insert underline))))))))

(global-set-key (kbd "C-c C-u") 'underline-complete)

(defun get-current-line ()
  "Returns the string of the line that `point' resides in."
  (interactive)
  (save-excursion
    (filter-buffer-substring (point-at-line-start) 
			     (point-at-line-end)
			     t)))

(defun point-at-line-start ()
  "Returns the `point' of the point at Column 0 in the current line."
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line)
      (point))))

(defun point-at-line-end ()
  "Returns the `point' of the point at the end of the current line."
  (interactive)
  (save-excursion
    (progn
      (end-of-line)
      (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun range (start end)
  "Produces a list of ints from `start' to `end' exclusive.
BUG: Recursion depth limit destroys this."
  (if (= start end)
      nil
    (cons start
	  (range (1+ start) end))))

(defun my-reverse (items)
  "Reverses a list."
  (let ((f (lambda (x acc) (append acc (list x)))))
    (foldr f nil items)))

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

(defun string-nth-char (n item)
  "Returns the nth char of a given string."
  (let ((chars (string-to-list item)))
    (nth n chars)))

(defun string-to-string-list (item)
  "Converts a string to a list of each char as a string."
  (map 'string (string-to-list item)))

; (string-to-string-list "this is a list")

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

(defun stripr (line)
  "Strips trailing whitespace."
  (when (string-match "[ \t]*$" line)
    (replace-match "" nil nil line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONAL PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defalias 'map 'mapcar)

(defun filter (p items)
  "A classic filter function."
  (if (p (car items))
      (cons (car items) (filter p (cdr items)))
    (filter p (cdr items))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMBER STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evenp (num)
  "Determines if a number is even."
  (= 0 (% num 2)))

(defun oddp (num)
  "Determines if a number is odd."
  (! (evenp num)))

(defun ! (bool)
  "True becomes nil, nil becomes true."
  (if bool
      nil
    t))

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
