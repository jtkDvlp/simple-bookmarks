;; MIT License

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

(require 'simple-bookmarks-funcs)
(require 'simple-bookmarks-filters)
(require 'simple-bookmarks-utils)

(require 'files)
(require 'desktop)
(require 'dired)
(require 'browse-url)

(defun sbi/add (&optional name func args more)
  (interactive "Sbookmark name: \nabookmark func: \nxbookmark args: \nxbookmark more: ")
  (when-let ((result (sbf/add-save sb/bookmarks (sbf/create name func args more))))
    (message "added bookmark '%s'" name)
    (setq sb/bookmarks result)
    result))

(defun sbi/remove (&optional filter)
  (interactive "abookmark filter: ")
  (let* ((bookmark-filter (lambda (bookmark-apair) (funcall filter (sbu/val bookmark-apair))))
         (list-filter (lambda (list) (remove-if-not bookmark-filter list)))
         (names (mapcar 'symbol-name (sbf/names (sbu/update-by-func sb/bookmarks 'list list-filter))))
         (name (intern (completing-read "remove bookmark: " names nil t)))
         (result (sbf/remove-save sb/bookmarks (sbf/get sb/bookmarks name))))
    (message "removed bookmark '%s'" name)
    (setq sb/bookmarks result)
    result))

(defun sbi/execute (&optional filter)
  (interactive "abookmark filter: ")
  (let* ((bookmark-filter (lambda (bookmark-apair) (funcall filter (sbu/val bookmark-apair))))
         (list-filter (lambda (list) (remove-if-not bookmark-filter list)))
         (names (mapcar 'symbol-name (sbf/names (sbu/update-by-func sb/bookmarks 'list list-filter))))
         (name (intern (completing-read "call bookmark: " names nil t)))
         (result (sbf/execute-by-name sb/bookmarks name)))
    (message "call bookmark '%s'" name)
    result))

(defun sbi/remove-from-all ()
  (interactive)
  (sbi/remove 'sbf/any-p))

(defun sbi/execute-from-all ()
  (interactive)
  (sbi/execute 'sbf/any-p))

(defun sbi/add-file (&optional name path more)
  (interactive "Sfile-bookmark name: \nffile-bookmark path:\ni")
  (sbi/add name 'find-file (list path) more))

(defun sbi/remove-file ()
  (interactive)
  (sbi/remove 'sbf/file-bookmark-p))

(defun sbi/execute-file ()
  (interactive)
  (sbi/execute 'sbf/file-bookmark-p))

(defun sbi/add-directory (&optional name path more)
  (interactive "Sdirectory-bookmark name: \nDdirectory-bookmark path: \ni")
  (sbi/add name 'dired (list path) more))

(defun sbi/remove-directory ()
  (interactive)
  (sbi/remove 'sbf/directory-bookmark-p))

(defun sbi/execute-directory ()
  (interactive)
  (sbi/execute 'sbf/directory-bookmark-p))

(defun sbi/add-desktop (&optional name path more)
  (interactive "Sdesktop-bookmark name: \nDdesktop-bookmark path: \ni")
  (sbi/add name 'desktop-change-dir (list path) '((prompt t))))

(defun sbi/create-desktop (&optional name path more)
  (interactive "Sdesktop-bookmark name: \nDdesktop-bookmark path: \ni")
  (desktop-save-in-desktop-dir path)
  (sbi/add name 'desktop-change-dir (list path) '((prompt t))))

(defun sbi/remove-desktop ()
  (interactive)
  (sbi/remove 'sbf/desktop-bookmark-p))

(defun sbi/execute-desktop ()
  (interactive)
  (sbi/execute 'sbf/desktop-bookmark-p))

(defun sbi/add-url (&optional name path more)
  (interactive "Surl-bookmark name: \nsurl-bookmark path: \ni")
  (sbi/add name 'browse-url (list path) more))

(defun sbi/remove-url ()
  (interactive)
  (sbi/remove 'sbf/url-bookmark-p))

(defun sbi/execute-url ()
  (interactive)
  (sbi/execute 'sbf/url-bookmark-p))

(provide 'simple-bookmarks-interactive)
