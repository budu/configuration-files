
(load "~/.local.el")

(setenv "PATH" (concat "~/bin:" (getenv "PATH")))

;;; add user site-lisp directory and sub-directories to load-path
(add-to-list 'load-path *main-site-lisp*)
(cd *main-site-lisp*)
(normal-top-level-add-subdirs-to-load-path)

;;; ELPA
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(require 'ruby-mode)
(require 'magit)
(require 'auto-dictionary)
(require 'rinari)
(require 'slime)
(require 'yaml-mode)
(require 'tidy)
(require 'ido)

;;; Custom
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
(setq default-tab-width 2)
(setq initial-scratch-message nil)
(setq default-buffer-file-coding-system 'unix)

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

;; save history between sessions
(setq savehist-additional-variables
  '(search-ring regexp-search-ring compile-history)
  savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

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
(global-set-key "\M-j"        (fset 'parens   "\C-u-\C-xo"))
(global-set-key "\M-k"        (fset 'parens   "\C-xo"))
(global-set-key "\C-x\C-b"    (fset 'parens   "\C-xb"))
(global-set-key "\C-x\C-n"    (fset 'parens   "\C-x\C-b\C-xo\C-x\C-b\C-xo\C-x\C-b"))
(global-set-key "\C-z"        (fset 'parens   "()\C-b"))
(global-set-key "\M-z"        (fset 'brackets "[]\C-b"))
(global-set-key "\C-\M-z"     (fset 'braces   "{}\C-b"))
(global-set-key "\C-x\""      (fset 'braces   "\"\"\C-b"))
(global-set-key "\C-x'"       (fset 'braces   "''\C-b"))
(global-set-key "\C-c\C-z"    (fset 'braces   "<%  %>\C-b\C-b\C-b"))
(global-set-key "\C-c\M-z"    (fset 'braces   "<%  %>\n<% end %>\C-p\C-b\C-b\C-b"))
(global-set-key "\C-cz"       (fset 'braces   "<%=  %>\C-b\C-b\C-b"))
(global-set-key
 [f7]
 '(lambda () (interactive)
    (if (< (count-windows) 2)
        (split-window-vertically))
    (enlarge-window 16)))

;;;; miscellaneous =====================================================

;;; ido mode

(ido-mode t)
(setq ido-enable-flex-matching t)

;;; bash color support
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; clean emacs
(defvar *backup-dir* (concat *emacs-d* "emacs-backups/"
                       (user-login-name)))
(setq backup-directory-alist (list (cons "." *backup-dir*)))

;;; dired

(put 'dired-find-alternate-file 'disabled nil)

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

;;; set default working directory
(cd *default-directory*)

;;; htmlize
(load "htmlize")

;;; mode-compile

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

;;; auto-dictionary

(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

;;; kill completion buffers after 3 seconds

(add-hook 'completion-setup-hook
  (lambda () (run-at-time 3 nil
    (lambda ()
      (delete-windows-on "*Completions*")
      (kill-buffer "*Completions*")))))

;;; magit

(setq magit-revert-item-confirm t)

(global-set-key "\C-xg" 'magit-status)

;;; css

(setq css-indent-level 2)

;;; erc

(require 'erc)
(require 'erc-services)

(when (locate-library "erc")
  (autoload 'erc-select "erc" nil t)

  (erc-services-mode 1)

  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-prompt-for-password nil)
  (setq erc-server "irc.freenode.net")
  (setq erc-port 6667)
  (setq erc-user-full-name "Nicolas Buduroi")
  (setq erc-email-userid "budu")
  (setq erc-nick '("budu"))
  (setq erc-nickserv-passwords
        `((freenode ((,*erc-freenode-nick* . ,*erc-freenode-password*)))))

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(erc-modules
     (quote
      (autojoin button completion fill irccontrols list match
       menu move-to-prompt netsplit networks noncommands readonly
       ring services stamp track)))))

;;; slime

(global-set-key
 [f8]
 '(lambda () (interactive)
    (start-process "swank-clojure"
                   "*swank-clojure*"
                   "~/.lein/bin/swank-clojure")
    (set-process-filter (get-buffer-process "*swank-clojure*")
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" "4005")
                            (with-current-buffer (slime-output-buffer t)
                              (setq default-directory root))
                            (set-process-filter process nil))))))

(global-set-key [f9] 'slime-eval-buffer)
(global-set-key [f10] 'clojure-jack-in)

(setq swank-clojure-extra-vm-args
      (list "-Dcom.sun.management.jmxremote=true"))

;;; ruby

(setq rinari-tags-file-name "TAGS")

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;;; org mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-4 ((t (:inherit outline-4 :foreground "green")))))

;;; yaml

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; markdown

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(setq markdown-command "markdown2 /dev/stdin")

(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;; coffee-mode

(add-to-list 'load-path "~/.emacs.d/site-lisp/coffee-mode")
(require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;;; scss-mode

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;;; end

