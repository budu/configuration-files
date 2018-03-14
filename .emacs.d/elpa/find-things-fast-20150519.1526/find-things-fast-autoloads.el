;;; find-things-fast-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "find-things-fast" "find-things-fast.el" (22802
;;;;;;  13710 495919 193000))
;;; Generated autoloads from find-things-fast.el

(let ((loads (get 'find-things-fast 'custom-loads))) (if (member '"find-things-fast" loads) nil (put 'find-things-fast 'custom-loads (cons '"find-things-fast" loads))))

(autoload 'ftf-add-filetypes "find-things-fast" "\
Makes `ftf-filetypes' local to this buffer and adds the
elements of list types to the list

\(fn TYPES)" nil nil)

(autoload 'ftf-project-directory "find-things-fast" "\
Returns what we should use as `default-directory'.

\(fn)" nil nil)

(autoload 'ftf-grepsource "find-things-fast" "\
Greps the current project, leveraging local repository data
for speed and falling back on a big \"find | xargs grep\"
command if we aren't.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git or mercurial repository OR a project root
defined by the optional `project-root.el' package OR the default
directory if none of the above is found.

\(fn CMD-ARGS)" t nil)

(autoload 'ftf-find-file "find-things-fast" "\
Prompt with a completing list of all files in the project to find one.

The project's scope is defined first as a directory containing
either a `.dir-locals.el' file or an `.emacs-project' file OR the
root of the current git or mercurial repository OR a project root
defined by the optional `project-root.el' package OR the default
directory if none of the above is found.

\(fn)" t nil)

(autoload 'with-ftf-project-root "find-things-fast" "\
Run BODY with `default-directory' set to what the
find-things-fast project root. A utility macro for any of your
custom functions which might want to run

\(fn &rest BODY)" nil t)

(autoload 'ftf-compile "find-things-fast" "\
Run the `compile' function from the project root.

\(fn)" t nil)

(autoload 'ftf-gdb "find-things-fast" "\
Run the `gdb' function from the project root.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; find-things-fast-autoloads.el ends here
