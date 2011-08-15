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

(set-frame-position (selected-frame)  100 20)

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
         ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
       (font-lock-keyword-face
         ((t (:foreground "LightGreen"))))
       (font-lock-preprocessor-face
         ((t (:bold t :foreground "Gold" :weight bold))))
       (font-lock-reference-face
         ((t (:foreground "SteelBlue"))))
       (font-lock-string-face
         ((t (:italic t :foreground "BurlyWood" :slant oblique))))
       (font-lock-type-face
         ((t (:bold t :foreground "PaleGreen" :weight bold))))
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
