;;; riscy-io.el --- Elisp functions for exporting riscy.io html.

;;; Commentary:

;; This file is expected to be run using "emacs --script":
;; - Defines `riscy-io-get-note' and `riscy-io-compile-notes'
;; - Installs "htmlize" (if it isn't installed already)
;; - Exports every file listed in `command-line-args'

;;; Code:

(require 'subr-x)
(require 'ox-html)

(defun riscy-io-get-note (org-file &optional more-text)
  "Pull the preview or fulltext out of ORG-FILE.
MORE-TEXT can be used to adjust the 'more' button, if any."
  (with-temp-buffer
    (insert-file-contents org-file)
    (let ((has-preview (re-search-forward "^# preview\n" nil t)))
      (concat
       (string-trim (buffer-substring
                     (if has-preview (point) (point-min))
                     (if (re-search-forward "\n\n" nil t) (point-at-bol) (point-max))))
       (when has-preview (format ".. ([[./%s.html][%s]])"
                                 (string-trim-right org-file ".org")
                                 (or more-text "more")))))))

(defun riscy-io-compile-notes (directory)
  "Compile commonplace notes from DIRECTORY."
  (mapconcat
   (lambda (org-file)
     (when (string-match "^[^_]*\\.org$" org-file)
       (concat (string-trim-right org-file ".org") ". "
               (riscy-io-get-note
                (concat (file-name-as-directory directory) org-file)))))
   (reverse (directory-files directory))
   "\n\n"))

;; everything needed to build the .html files:
(package-initialize)
(package-refresh-contents)
(package-install 'htmlize)
(setq org-confirm-babel-evaluate nil)
(dolist (filename (nthcdr 3 command-line-args))
  (find-file filename)
  (org-html-export-to-html))

(provide 'riscy-io)
;;; riscy-io.el ends here
