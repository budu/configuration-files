;;;; budu's dot emacs file

;;; local paths
;;;   *emacs-d*
;;;   *main-site-lisp*
;;;   *default-directory*
;;;   *clojure-bin*
;;;   *scheme-bin*
;;;   *haskell-bin*
(load "~/.local.el")

;;; add user site-lisp directory and sub-directories to load-path
(add-to-list 'load-path *main-site-lisp*)
(cd *main-site-lisp*)
(normal-top-level-add-subdirs-to-load-path)

;;;; various utilities =================================================

(defun filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(setq inferior-lisp-buffer "*inferior-lisp*")
(defun inferior-lisp-proc ()
  (let ((proc (get-buffer-process
                (if (eq major-mode 'inferior-lisp-mode)
                  (current-buffer)
                  inferior-lisp-buffer))))
    (or proc
      (error "No current process. See variable inferior-lisp-buffer"))))

(defun eval-in-inferior-lisp (expression-string)
  (comint-send-string (inferior-lisp-proc)
    (format " %s\n" expression-string)))

;;; http://emacs-fu.blogspot.com/2009/01/counting-words.html
(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined,
  count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
         (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

;;; unfilling functions (http://www.emacswiki.org/emacs/UnfillParagraph)
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of
  text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region (from to)
  "Takes a region containing multi-line paragraphs and makes them into a
  single line of text."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end))))
  (let ((fill-column (point-max)))
    (fill-region from to)))

;;;; settings ==========================================================

(setq auto-save-interval 237)
(setq default-major-mode 'text-mode)
(setq inhibit-splash-screen t)
(setq require-final-newline nil)
(setq scroll-preserve-screen-position t)
(setq scroll-step 1)
(setq transient-mark-mode t)
(setq ewd-kp-usage 'num)
(setq even-window-heights nil)

(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)
(show-paren-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))

(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;;; default fonts for graphical mode
(let (font (fonts (list "Bitstream Vera Sans Mono-12"
                    "Lucida Console-12")))
  (while fonts
    (condition-case nil
      (progn
        (setq font (car fonts))
        (set-default-font font)
        (setq fonts nil))
      (error (setq fonts (cdr fonts))))))

;;; set frame size (http://stackoverflow.com/questions/92971)
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
    (progn
      ;; use 140 char wide window for largeish displays
      ;; and smaller 80 column windows for smaller displays
      ;; pick whatever numbers make sense for you
      (if (> (x-display-pixel-width) 1280)
        (add-to-list 'default-frame-alist (cons 'width 140))
        (add-to-list 'default-frame-alist (cons 'width 80)))
      ;; for the height, subtract a couple hundred pixels
      ;; from the screen height (for panels, menubars and
      ;; whatnot), then divide by the height of a char to
      ;; get the height we want
      (add-to-list 'default-frame-alist
        (cons 'height (/ (- (x-display-pixel-height) 100)
                        (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;; set frame position
(set-frame-position (selected-frame)  100 20)

;;; improve font lock mode
(cond ((fboundp 'global-font-lock-mode)
        (global-font-lock-mode t)
        (setq font-lock-maximum-decoration t)))

;;; set fill column mode (disabled by default)
(setq auto-fill-mode 0)
(setq-default fill-column 72)

;;;; key bindings ======================================================

;;; custom key bindings
(global-set-key "\C-xc"    'comment-region)
(global-set-key "\C-xu"    'uncomment-region)
(global-set-key "\C-h"     'delete-backward-char)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-c" 'kill-region)
(global-set-key "\C-x\M-q" 'save-buffers-kill-emacs)
(global-set-key [f4]       'browse-url)
(global-set-key [f5]       'start-kbd-macro)
(global-set-key [f6]       'end-kbd-macro)
(global-set-key [(control shift ?h)] 'help-command)

;;; remove unused key bindings
(global-unset-key "\C-x\C-b")

;;; macros
(global-set-key "\C-x\C-b"    (fset 'parens   "\C-xb"))
(global-set-key "\C-x\C-n"    (fset 'parens   "\C-x\C-b\C-xo\C-x\C-b\C-xo\C-x\C-b"))
(global-set-key "\C-z"        (fset 'parens   "()\C-b"))
(global-set-key "\M-z"        (fset 'brackets "[]\C-b"))
(global-set-key "\C-\M-z"     (fset 'braces   "{}\C-b"))

;;;; default color theme ===============================================

(require 'color-theme)
(defun color-theme-budu-midnight ()
  (interactive)
  (color-theme-install
    '(color-theme-midnight
       ((font . "fixed")
         (width . 130)
         (height . 50)
         (background-color . "black")
         (foreground-color . "white")
         (background-mode . dark)
         (mouse-color . "grey85")
         (cursor-color . "grey85"))
       (default ((t (nil))))
       (font-lock-builtin-face
         ((t (:foreground "SteelBlue"))))
       (font-lock-comment-face
         ((t (:italic t :foreground "orange3" :slant oblique))))
       (font-lock-constant-face
         ((t (:bold t :foreground "Gold" :weight bold))))
       (font-lock-doc-face
         ((t (:italic t :slant oblique :foreground "BurlyWood"))))
       (font-lock-doc-string-face
         ((t (:italic t :slant oblique :foreground "BurlyWood"))))
       (font-lock-function-name-face
         ((t (:bold t :foreground "LightSkyBlue" :weight bold
               :height 0.9 :family "Verdana"))))
       (font-lock-keyword-face
         ((t (:foreground "LightGreen"))))
       (font-lock-preprocessor-face
         ((t (:bold t :foreground "Gold" :weight bold))))
       (font-lock-reference-face
         ((t (:foreground "SteelBlue"))))
       (font-lock-string-face
         ((t (:italic t :foreground "BurlyWood" :slant oblique))))
       (font-lock-type-face
         ((t (:bold t :foreground "PaleGreen" :weight bold :height 0.9
               :family "Verdana"))))
       (font-lock-variable-name-face
         ((t (:foreground "Aquamarine"))))
       (font-lock-warning-face
         ((t (:bold t :foreground "chocolate" :weight bold))))
       (highline-face
         ((t (:background "grey12"))))
       (setnu-line-number-face
         ((t (:background "Grey15" :foreground "White" :bold t))))
       (show-paren-match-face
         ((t (:background "grey30"))))
       (region
         ((t (:background "grey15"))))
       (highlight
         ((t (:background "blue"))))
       (secondary-selection
         ((t (:background "navy"))))
       (widget-field-face
         ((t (:background "navy"))))
       (widget-single-line-field-face
         ((t (:background "royalblue")))))))

(color-theme-budu-midnight)

;;;; web help ==========================================================

(require 'url)

(defun google ()
  (interactive)
  (let ((url (if (and transient-mark-mode mark-active)
               (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))
               (thing-at-point 'symbol))))
    (browse-url
      (concat "http://www.google.com/search?"
        (format "q=%s" (url-hexify-string url))))))

(defun search-all-in-url (query site inurl)
  (concat "http://www.google.com/"
    (format "search?q=allinurl:%s+site:%s+inurl:%s&btnI"
      (url-hexify-string query)
      (url-hexify-string site)
      (url-hexify-string inurl))))

(defun web-help (site inurl)
  (browse-url (search-site-url site inurl
                (thing-at-point 'symbol))))

(defun java-help ()
  (interactive)
  (web-help "java.sun.com" "/javase/6/docs/api/"))

(defun ruby-help ()
  (interactive)
  (web-help "www.ruby-doc.org" "/core/"))

(global-set-key [(f1)] 'google)
(global-set-key [(f2)] 'java-help)
(global-set-key [(f3)] 'ruby-help)

;;;; miscellaneous =====================================================

;;; clean emacs
(defvar *backup-dir* (concat *emacs-d* "emacs-backups/"
                       (user-login-name)))
(setq backup-directory-alist (list (cons "." *backup-dir*)))

;;; add flyspell-mode to text-mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;; and auto-dictionary-mode to flyspell-mode
(require 'auto-dictionary)
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

;;; hook flyspell-prog-mode to some other modes
(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook 'flyspell-prog-mode 1))

;;; gnus setup
(setq gnus-select-method
  '(nntp "nntp.aioe.org"
     (nntp-port-number 119)))

;;; htmlize
(load "htmlize")

;;; default inferior lisp
(setq inferior-lisp-program "sbcl")

;;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;; set default working directory
(cd *default-directory*)

;;; it's magit

(require 'magit)

(global-set-key "\C-xg" 'magit-status)

;;; cygwin-mount

(when *cygwin-bin*
  (setenv "PATH" (concat *cygwin-bin* ";" (getenv "PATH")))
  (setq exec-path (cons *cygwin-bin* exec-path))
  (require 'cygwin-mount)
  (cygwin-mount-activate))

;;;; language-specific configuration ===================================

(setq c-basic-offset 4)

;;; python
(autoload 'python-mode "python-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(defun python-stuff ()
  (setq
    tab-width 4
    py-indent-offset 4
    indent-tabs-mode nil
    py-smart-indentation nil))
(add-hook 'python-mode-hook 'python-stuff)

;;; c
(defun c-stuff ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'c-stuff)

;;; haskell
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook
  (function
    (lambda ()
      (setq haskell-program-name *haskell-bin*)
      (setq haskell-ghci-program-name *haskell-bin*))))

;;; ruby
(autoload #'ruby-mode "ruby-mode" "Start ruby-mode" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

;;; javascript
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;;; groovy
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;;; lisps =============================================================

(setq lisp-indent-offset 2)

;; clojure-mode
(require 'clojure-mode)
(setq auto-mode-alist (cons '("\\.clj$" . clojure-mode)
                        auto-mode-alist))

;; swank-clojure

(require 'swank-clojure-autoload)
(swank-clojure-config
  (setq swank-clojure-binary (file-truename *clojure-bin*)))

;; slime

(require 'slime)
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))
(slime-setup)

(defvar *default-window-configuration* nil)
(defvar *inferior-list-window-configuration* nil)

(global-set-key "\C-cb" 'slime-eval-buffer)
  
(global-set-key [f7]
  '(lambda () (interactive)
     (if *default-window-configuration*
       (progn
         (set-window-configuration *default-window-configuration*)
         (slime-repl)
         (slime-repl-clear-buffer))
       (progn
         (split-window-vertically)
         (enlarge-window 16)
         (other-window 1)
         (slime)
         (setq *inferior-list-window-configuration*
           (current-window-configuration))
         (sit-for 7)
         (setq *default-window-configuration*
           (current-window-configuration))))))

(global-set-key [f8]
  '(lambda () (interactive)
     (when *inferior-list-window-configuration*
       (set-window-configuration *inferior-list-window-configuration*))))

(global-set-key [f9]
  '(lambda () (interactive)
     (slime-repl)
     (slime-repl-clear-buffer)
     (slime-restart-inferior-lisp)))

;;; scheme
(defun scheme ()
  (interactive)
  (require 'quack)
  (run-scheme *scheme-bin*))
