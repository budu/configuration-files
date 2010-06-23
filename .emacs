
(load "~/.local.el")

;;; add user site-lisp directory and sub-directories to load-path
(add-to-list 'load-path *main-site-lisp*)
(cd *main-site-lisp*)
(normal-top-level-add-subdirs-to-load-path)

;;; ELPA
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(load "budu-help")
(load "budu-style") ; needs color-theme (not in ELPA)
(load "budu-misc")

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

;;; more complex ones
(global-set-key
 [f7]
 '(lambda () (interactive)
    (if (< (count-windows) 2)
        (split-window-vertically))
    (enlarge-window 16)))

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

;;; gnus setup
(setq gnus-select-method
  '(nntp "nntp.aioe.org"
     (nntp-port-number 119)))

;;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;; Cygwin Shell

(setq w32shell-cygwin-bin *cygwin-bin*)

;;; set default working directory
(cd *default-directory*)

;;;; ELPA packages configuration =======================================

;;; htmlize
(load "htmlize")

;;; auto-dictionary
(require 'auto-dictionary)
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

;;; magit
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

;;; slime
(require 'slime)

(global-set-key [f8]
  '(lambda () (interactive)
     (slime-repl)
     (slime-repl-clear-buffer)
     (slime-restart-inferior-lisp)))

(global-set-key [f9] 'slime-eval-buffer)
(global-set-key [f10] 'swank-clojure-project)

(setq swank-clojure-classpath
      (jars-in-below-directory "~/.emacs.d/jars"))

(setq swank-clojure-extra-vm-args
      (list "-Dcom.sun.management.jmxremote=true"))

(setq swank-clojure-init-files
      (list *clojure-init-file*))

;;;; Others

(require 'chuck-mode)
(setq chuck-exec *chuck-exec*)
