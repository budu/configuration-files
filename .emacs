
(load "~/.local.el")

;;; add user site-lisp directory and sub-directories to load-path
(add-to-list 'load-path *main-site-lisp*)
(cd *main-site-lisp*)
(normal-top-level-add-subdirs-to-load-path)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/"))))

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
(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
(global-set-key "\C-j"      'newline-and-indent)
(global-set-key "\C-xc"     'comment-region)
(global-set-key "\C-xu"     'uncomment-region)
(global-set-key "\C-h"      'delete-backward-char)
(global-set-key "\C-w"      'backward-kill-word)
(global-set-key "\C-x\C-h"  'mark-whole-buffer)
(global-set-key "\C-x\C-c"  'kill-region)
(global-set-key "\C-x\M-q"  'save-buffers-kill-emacs)
(global-set-key [f5]        'start-kbd-macro)
(global-set-key [f6]        'end-kbd-macro)
(global-set-key [f7]        'shell)

(global-set-key (kbd "S-C-k") 'shrink-window)
(global-set-key (kbd "S-C-j") 'enlarge-window)
(global-set-key (kbd "S-C-h") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-l") 'enlarge-window-horizontally)

(global-set-key [f11]
  (lambda ()
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list))))))

;;; remove unused key bindings
(global-unset-key "\C-x\C-b")

;;; macros
(global-set-key "\M-j"        (fset 'parens   "\C-u-\C-xo"))
(global-set-key "\M-k"        (fset 'parens   "\C-xo"))
(global-set-key "\C-x\C-b"    (fset 'parens   "\C-xb"))
(global-set-key "\C-z"        (fset 'parens   "()\C-b"))
(global-set-key "\M-z"        (fset 'brackets "[]\C-b"))
(global-set-key "\C-\M-z"     (fset 'braces   "{}\C-b"))
(global-set-key "\C-x\""      (fset 'braces   "\"\"\C-b"))
(global-set-key "\C-x'"       (fset 'braces   "''\C-b"))

(global-set-key "\C-c\C-z"    (fset 'braces   "<%  %>\C-b\C-b\C-b"))
(global-set-key "\C-c\M-z"    (fset 'braces   "<%  %>\n<% end %>\C-p\C-b\C-b\C-b"))
(global-set-key "\C-cz"       (fset 'braces   "<%=  %>\C-b\C-b\C-b"))
(global-set-key "\C-ct"       (fset 'braces   "<%= t('.') %>\C-b\C-b\C-b\C-b\C-b"))

;;; mouse stuff

(global-set-key [mouse-3] 'mouse-yank-primary)

;;;; miscellaneous =====================================================

;;; trailing whitespace

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;;; automatically create missing directories

(add-hook
 'before-save-hook
 (lambda ()
   (when buffer-file-name
     (let ((dir (file-name-directory buffer-file-name)))
       (when (not (file-exists-p dir))
         (make-directory dir t))))))

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

;;; mode-compile

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

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
(setq css-indent-offset 2)

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

;;; slime

(global-set-key
 [f8]
 '(lambda () (interactive)
    (let ((p (get-process "*swank-clojure*")))
      (if p (process-kill-without-query p)))
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

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)

(add-hook
 'ruby-mode-hook
 (lambda ()
   (autopair-mode)
   (add-hook 'local-write-file-hooks
             '(lambda ()
                (save-excursion
                  (untabify (point-min) (point-max))
                  (delete-trailing-whitespace))))))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

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

(require 'yaml-mode)
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

;; js2-mode

(add-hook 'js2-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'delete-trailing-whitespace)
             (define-key js2-mode-map (kbd "A-r") 'js2-execute-buffer)
             (define-key js2-mode-map (kbd "A-R") 'js2-execute-line)
             (define-key js2-mode-map "\C-L" 'js2-insert-console)
             (defun js-continued-var-decl-list-p ()
               "Return non-nil if point is inside a continued variable declaration
list."
               (interactive)
               (let ((start (save-excursion (js-re-search-backward "\\<var\\>" nil t))))
                 (and start
                      (save-excursion (re-search-backward "\n" start t))
                      (not (save-excursion
                             (js-re-search-backward
                              ";\\|[^, \t][ \t]*\\(/[/*]\\|$\\)" start t))))))
             (defun js-proper-indentation (parse-status)
             "Return the proper indentation for the current line."
             (save-excursion
               (back-to-indentation)
               (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
                     (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
                     (continued-expr-p (js-continued-expression-p)))
                 (cond (ctrl-stmt-indent)
                       ((js-continued-var-decl-list-p)
                        (js-re-search-backward "\\<var\\>" nil t)
                        (+ (current-indentation) js2-basic-offset))
                       ((nth 1 parse-status)
                        (goto-char (nth 1 parse-status))
                        (if (looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
                            (progn
                              (skip-syntax-backward " ")
                              (when (= (char-before) ?\)) (backward-list))
                              (back-to-indentation)
                              (cond (same-indent-p
                                     (current-column))
                                    (continued-expr-p
                                     (+ (current-column) (* 2 js2-basic-offset)))
                                    (t
                                     (+ (current-column) js2-basic-offset))))
                          (unless same-indent-p
                            (forward-char)
                            (skip-chars-forward " \t"))
                          (current-column)))
                       (continued-expr-p js2-basic-offset)
                       (t 0)))))))

;;; end

(custom-set-variables

 '(erc-modules '(autojoin button completion fill irccontrols list
                 match menu move-to-prompt netsplit networks
                 noncommands readonly ring services stamp track))

 '(safe-local-variable-values
   '((encoding . utf-8)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")))))

(put 'erase-buffer 'disabled nil)

;;; hideshow

(load-library "hideshow")

(global-set-key (kbd "C--") 'hs-hide-all)
(global-set-key (kbd "C-+") 'hs-show-all)

(add-hook 'ruby-mode-hook 'hs-minor-mode)
