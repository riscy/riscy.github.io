;;; riscy-io.el --- Elisp functions for exporting riscy.io html

;;; Commentary:

;; This file is expected to be run using "emacs --script riscy-io.el":
;; - Installs "htmlize" (if it isn't installed already)
;; - Exports every file listed in `command-line-args'

;;; Code:

(require 'package)
(require 'subr-x)
(require 'ox-html)

(defun riscy-io-get-note (org-file &optional more-text)
  "Pull the preview or fulltext out of ORG-FILE.
MORE-TEXT can be used to adjust the 'more' button, if any."
  (with-temp-buffer
    (insert-file-contents org-file)
    (let ((has-preview (re-search-forward "^# preview\n" nil t)))
      (concat
       (string-trim
        (buffer-substring
         (if has-preview (point) (point-min))
         (if (re-search-forward "\n\n" nil t) (point-at-bol) (point-max))))
       (when has-preview
         (format ".. ([[./%s.html][%s]])"
                 (string-trim-right org-file ".org")
                 (or more-text "more")))))))

(defun riscy-io-compile-notes (directory)
  "Compile commonplace notes from DIRECTORY.
Call this from an org source block as follows:
#+begin_src emacs-lisp :exports results :results raw
  (riscy-io-compile-notes DIRECTORY)
#+end_src"
  (string-trim
   (mapconcat
    (lambda (org-file)
      (when (string-match "^[^_].*\\.org$" org-file)
        (let ((basename (file-name-base org-file)))
          (format
           "@@html:%s <a id=%s href=#%s>#</a>@@\n%s\n\n"
           basename basename basename
           (riscy-io-get-note
            (concat (file-name-as-directory directory) org-file))))))
    ;; reverse chronological order:
    (reverse (directory-files directory))
    "")))

(unless noninteractive (error "This file must be run with 'emacs --script'"))
;; everything needed to build the .html files:
(package-initialize)
(unless (require 'htmlize nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-install 'htmlize))
(setq org-confirm-babel-evaluate nil)
(setq make-backup-files nil)
(dolist (filename (nthcdr 3 command-line-args))
  (find-file filename)
  (org-html-export-to-html))

(provide 'riscy-io)
;;; riscy-io.el ends here
