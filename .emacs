;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda character
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb")))
(global-set-key (kbd "M-z") (lambda () (interactive) (insert "ℤ")))

(electric-pair-mode +1)

;; Line by line scrolling
(setq scroll-step 1)

;; Indents are done with spaces, not tabs.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq ruby-indent-level 4)
(defvaralias 'c-basic-offset 'tab-width)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'pomodoro)
(pomodoro-add-to-mode-line)

;; Helm
(require 'helm-config)
(helm-mode 1)

(require 'generic-x)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(add-hook 'c-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list "/usr/include/freetype2"))))

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

(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)))
;            (setq indent-tabs-mode t)))

;; Java sucks
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))

;; Arch Linux only
(when (eq system-type 'gnu/linux)
  ;; Ensime
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  ;; Haskell mode
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  ;;(add-to-list 'load-path "/home/colin/.emacs.d/elpa/haskell-mode-20141230.1141")
  ;;(load "haskell-mode-autoloads.el")
  ;;(add-to-list 'Info-default-directory-list "/home/colin/.emacs.d/elpa/haskell-mode-20141230.1141")
;;  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-type 'stack-ghci))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c h b") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (global-set-key (kbd "C-c h r") 'haskell-process-restart)
  (define-key haskell-mode-map (kbd "C-c h t") 
    (lambda () (interactive)
      (if (equal 'stack-ghci haskell-process-type)
          (progn
            (setq haskell-process-type 'cabal-repl)
            (message "Now in cabal-repl mode."))
        (progn 
          (setq haskell-process-type 'stack-ghci)
          (message "Now in stack mode.")))))

  ;; PKGBUILD mode
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
  (setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                                auto-mode-alist))

  ;; Scala Mode
;  (add-to-list 'load-path "/usr/share/emacs/scala-mode")
;  (require 'scala-mode-auto)
  ;(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))
  ;; Rust Mode
  (autoload 'rust-mode "rust-mode" "Rust mode" t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))
  ;; Erlang Mode
;;  (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.14/emacs")
;;  (require 'erlang-start)
  ;; Purescript mode
;;  (add-to-list 'load-path "/home/colin/.emacs.d/elpa/purescript-mode-20140525.1952/")
;;  (require 'purescript-mode)
  ;;  (add-to-list 'Info-default-directory-list "/home/colin/.emacs.d/elpa/purescript-mode-20140525.1952/")
  ;; Android Mode
;;  (require 'android-mode)
;;  (custom-set-variables '(android-mode-sdk-dir "/opt/android-sdk")))
  ;; Web mode
;;  (add-to-list 'load-path "/usr/share/emacs/site-lisp/web-mode")
;;  (require 'web-mode)
  ;;  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;(string-init (car (cdr (split-string "wee woo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun japanese (text)
  "Insert some Japanese text into a LaTeX file."
  (interactive "sText: ")
  (insert (string-concat (list "\\begin{CJK}{UTF8}{min}" text "\\end{CJK}"))))

(global-set-key (kbd "C-c j") 'japanese)

(defun wrap-strings (start end)
  "Wrap each string in a region with some given strings."
  (interactive "sStart: \nsEnd: ")
  (let* ((lines (lines-from-region))
         (fixed (map (lambda (l) (string-concat (list start l end))) lines)))
    (insert (string-unlines fixed))))

(global-set-key (kbd "C-c g") 'goto-line)

(defun count-chars-region ()
  "Count the number of chars in a region."
  (interactive)
  (let ((len (length (filter-buffer-substring (mark) (point) nil))))
    (message "Total chars: %d" len)))

;; Borrowed from: http://emacs.stackexchange.com/q/5371/3882
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun line-wrap-region (len)
  "Line wraps a region so its lines aren't longer than a length
specified by the user."
  (interactive "nNew line length: ")
  (let* ((lines   (lines-from-region))
         (words   (list-concat (mapcar 'string-words lines)))
         (wrapped (reverse (foldl
                            (lambda (acc word)
                              (if (null acc)
                                  (list word)
                                (let ((head (car acc)))
                                  (if (< len (+ (length word) (length head)))
                                      (cons word acc)
                                    (cons (string-unwords (list head word))
                                          (cdr acc))))))
                            nil
                            words))))
    (insert (string-unlines wrapped))))

(defun wrap75 ()
  (interactive)
  (line-wrap-region 75))

(defun line-wrap-jap (len)
  "Line wrap a region of Japanese characters. Assumed to have no spaces."
  (interactive "nNew line length: ")
  (let* ((chars (string-to-chars (string-concat (lines-from-region))))
         (wrapped (foldl
                   (lambda (acc char)
                     (if (null acc)
                         (list (list char))
                       (let ((head (car acc)))
                         (if (>= len (+ 1 (length head)))
                             (cons (cons char head) (cdr acc))
                           (cons (list char) acc)))))
                   nil
                   chars))
         (fixed (mapcar (lambda (cs) (string-from-chars (reverse cs)))
                        wrapped)))
    (insert (string-unlines (reverse fixed)))))

(defun wrap-jap-38 ()
  (interactive)
  (line-wrap-jap 38))

(defun lorem ()
  "Insert the first paragraph of Lorem Ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Maecenas sed urna eget tellus ultrices dapibus sed sit amet enim.
Phasellus ultricies venenatis augue, vel imperdiet orci bibendum imperdiet.
Etiam feugiat turpis in dictum imperdiet. Nunc quis urna pulvinar, condimentum
purus nec, ullamcorper nisl. Proin mollis sed dui non interdum.
Duis varius magna sed odio adipiscing, eget facilisis metus dictum. Cras
quis turpis sit amet diam dignissim aliquet a iaculis lorem.
Sed et rutrum velit."))

(defun now ()
  "Insert string for today's date like: year month day @ time"
  (interactive)
  (insert (format-time-string "%Y %B %e @ %H:%M")))

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
    (beginning-of-line)
    (indent-according-to-mode)
    (end-of-line)
    (newline)
    (save-excursion
      (newline)
      (insert (string-concat (list "\\end{" name "}")))
      (beginning-of-line)
      (indent-according-to-mode))
    (indent-according-to-mode)))

(global-set-key (kbd "C-c b") 'tex-block)

(defun numbered-list ()
  "Numbers each line in a region."
  (interactive)
  (let* ((line-pairs (list-enumerate (lines-from-region)))
	 (longest (length (int-to-string (car (list-last line-pairs))))))
    (insert (string-unlines (map (lambda (pair)
				   (let* ((num (int-to-string (car pair)))
					  (diff (- longest (length num))))
				     (string-concat (list
                                                     (string-pad " " diff num)
                                                     ". "
                                                     (cdr pair)))))
				 line-pairs)))))

(global-set-key (kbd "C-c n") 'numbered-list)

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
	 
;(global-set-key (kbd "C-c C-c") 'chartify)

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

(global-set-key (kbd "C-c u") 'underline-complete)

;; From: http://stackoverflow.com/q/13981899
(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defalias 'list-++ 'append)

(defun list-concat (lists)
  "[[a]] -> [a]"
  (foldr (lambda (list acc) (list-++ list acc)) nil lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-emptyp (item)
  "Deteremines if a given string is empty."
  (string= item ""))

(defun string-concat (strings)
  "Concatinate a list of strings together."
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

(defun string-from-chars (chars)
  "[Char] -> String"
  (concat chars))

(defun string-to-chars (item)
  "String -> [Char]"
  (string-to-list item))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONAL PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'map 'mapcar)

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

(defun id (x)
  "Returns what it's given."
  x)

(defun compose (f g)
  "Compose two functions together."
  (lambda (x) (funcall f (funcall g x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMBER STUFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (interactive)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android-sdk")
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-python-flake8-executable "flake8-python2")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (tmm-menubar))))
 '(pomodoro-break-time 2)
 '(pomodoro-long-break-time 30)
 '(purescript-mode-hook
   (quote
    (capitalized-words-mode turn-on-purescript-indentation)))
 '(visible-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
