;;; simple-bookmarks-interactive.el

;; Author: Julian T. Knabenschuh <jtkdevelopments@gmail.com>

;;; MIT License

;; Copyright (c) 2016 Julian T. Knabenschuh

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Code:

(require 'simple-bookmarks-funcs)
(require 'simple-bookmarks-filters)
(require 'simple-bookmarks-utils)

(require 'files)
(require 'desktop)
(require 'dired)
(require 'browse-url)
(require 'cl-lib)

(defun simple-bookmarks-interactive-add (&optional name func args more)
  (interactive "Sbookmark name: \nabookmark func: \nxbookmark args: \nxbookmark more: ")
  (let ((result (simple-bookmarks-funcs-add-save simple-bookmarks-bookmarks (simple-bookmarks-funcs-create name func args more))))
    (when result
      (message "added bookmark '%s'" name)
      (setq simple-bookmarks-bookmarks result)
      result)))

(defun simple-bookmarks-interactive-remove (&optional filter)
  (interactive "abookmark filter: ")
  (let* ((bookmark-filter (lambda (bookmark-apair) (funcall filter (simple-bookmarks-utils-val bookmark-apair))))
         (list-filter (lambda (list) (cl-remove-if-not bookmark-filter list)))
         (names (mapcar 'symbol-name (simple-bookmarks-funcs-names (simple-bookmarks-utils-update-by-func simple-bookmarks-bookmarks 'list list-filter))))
         (name (intern (completing-read "remove bookmark: " names nil t)))
         (result (simple-bookmarks-funcs-remove-save simple-bookmarks-bookmarks (simple-bookmarks-funcs-get simple-bookmarks-bookmarks name))))
    (message "removed bookmark '%s'" name)
    (setq simple-bookmarks-bookmarks result)
    result))

(defun simple-bookmarks-interactive-execute (&optional filter)
  (interactive "abookmark filter: ")
  (let* ((bookmark-filter (lambda (bookmark-apair) (funcall filter (simple-bookmarks-utils-val bookmark-apair))))
         (list-filter (lambda (list) (cl-remove-if-not bookmark-filter list)))
         (names (mapcar 'symbol-name (simple-bookmarks-funcs-names (simple-bookmarks-utils-update-by-func simple-bookmarks-bookmarks 'list list-filter))))
         (name (intern (completing-read "call bookmark: " names nil t)))
         (result (simple-bookmarks-funcs-execute-by-name simple-bookmarks-bookmarks name)))
    (message "call bookmark '%s'" name)
    result))

(defun simple-bookmarks-interactive-remove-from-all ()
  (interactive)
  (simple-bookmarks-interactive-remove 'simple-bookmarks-filters-any-p))

(defun simple-bookmarks-interactive-execute-from-all ()
  (interactive)
  (simple-bookmarks-interactive-execute 'simple-bookmarks-filters-any-p))

(defun simple-bookmarks-interactive-add-file (&optional name path more)
  (interactive "Sfile-bookmark name: \nffile-bookmark path:\ni")
  (simple-bookmarks-interactive-add name 'find-file (list path) more))

(defun simple-bookmarks-interactive-remove-file ()
  (interactive)
  (simple-bookmarks-interactive-remove 'simple-bookmarks-filters-file-bookmark-p))

(defun simple-bookmarks-interactive-execute-file ()
  (interactive)
  (simple-bookmarks-interactive-execute 'simple-bookmarks-filters-file-bookmark-p))

(defun simple-bookmarks-interactive-add-directory (&optional name path more)
  (interactive "Sdirectory-bookmark name: \nDdirectory-bookmark path: \ni")
  (simple-bookmarks-interactive-add name 'dired (list path) more))

(defun simple-bookmarks-interactive-remove-directory ()
  (interactive)
  (simple-bookmarks-interactive-remove 'simple-bookmarks-filters-directory-bookmark-p))

(defun simple-bookmarks-interactive-execute-directory ()
  (interactive)
  (simple-bookmarks-interactive-execute 'simple-bookmarks-filters-directory-bookmark-p))

(defun simple-bookmarks-interactive-add-desktop (&optional name path more)
  (interactive "Sdesktop-bookmark name: \nDdesktop-bookmark path: \ni")
  (simple-bookmarks-interactive-add name 'desktop-change-dir (list path) '((prompt t))))

(defun simple-bookmarks-interactive-create-desktop (&optional name path more)
  (interactive "Sdesktop-bookmark name: \nDdesktop-bookmark path: \ni")
  (desktop-change-dir path)
  (simple-bookmarks-interactive-add name 'desktop-change-dir (list path) '((prompt t))))

(defun simple-bookmarks-interactive-remove-desktop ()
  (interactive)
  (simple-bookmarks-interactive-remove 'simple-bookmarks-filters-desktop-bookmark-p))

(defun simple-bookmarks-interactive-execute-desktop ()
  (interactive)
  (simple-bookmarks-interactive-execute 'simple-bookmarks-filters-desktop-bookmark-p))

(defun simple-bookmarks-interactive-add-url (&optional name path more)
  (interactive "Surl-bookmark name: \nsurl-bookmark path: \ni")
  (simple-bookmarks-interactive-add name 'browse-url (list path) more))

(defun simple-bookmarks-interactive-remove-url ()
  (interactive)
  (simple-bookmarks-interactive-remove 'simple-bookmarks-filters-url-bookmark-p))

(defun simple-bookmarks-interactive-execute-url ()
  (interactive)
  (simple-bookmarks-interactive-execute 'simple-bookmarks-filters-url-bookmark-p))

(provide 'simple-bookmarks-interactive)
;;; simple-bookmarks-interactive.el ends here
