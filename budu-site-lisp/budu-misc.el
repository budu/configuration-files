
(defun count-windows (&optional minibuf)
  (let ((count 0))
    (walk-windows (function (lambda (w) (setq count (+ count 1))))
                  (and (memq (cdr (assoc 'minibuffer (frame-parameters)))
                             '(only t))
                       minibuf))
    count))

(defun jars-in-below-directory (directory)
  "List the .jar files in DIRECTORY and in its sub-directories."
  ;; based on http://www.delorie.com/gnu/docs/emacs-lisp-intro/emacs-lisp-intro_225.html
  (let (jar-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (let ((file (car (car current-directory-list))))
        (cond
         ((equal ".jar" (substring file -4))
          (setq jar-files-list
                (cons file jar-files-list)))
         ((eq t (car (cdr (car current-directory-list))))
          (if (equal (or "." "..")
                     (substring file -1))
              ()
            (progn
              (setq jar-files-list
                    (append
                     (jars-in-below-directory file)
                     jar-files-list)))))))
      (setq current-directory-list (cdr current-directory-list)))
    jar-files-list))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))
