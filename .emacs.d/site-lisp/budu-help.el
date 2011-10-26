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
  (browse-url (search-all-in-url
                (thing-at-point 'symbol)
                site
                inurl)))

(defun ruby-help ()
  (interactive)
  (web-help "www.ruby-doc.org" "/core/"))

(defun rails-help ()
  (interactive)
  (web-help "apidock.com" "/rails/"))

(defun java-help ()
  (interactive)
  (web-help "download.oracle.com" "/javase/6/docs/api/"))

(defun browse-current-buffer ()
  (interactive)
  (browse-url
    (buffer-file-name (current-buffer))))

(global-set-key [(f1)] 'google)
(global-set-key [(f2)] 'ruby-help)
(global-set-key [(f3)] 'rails-help)
(global-set-key [(f4)] 'java-help)
