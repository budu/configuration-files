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

(defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#181818")
    ("zenburn-bg+05" . "#282828")
    ("zenburn-bg+1"  . "#2F2F2F")
    ("zenburn-bg+2"  . "#3F3F3F")
    ("zenburn-bg+3"  . "#4F4F4F")))
(load-theme 'zenburn t)
