;;; simple-bookmarks-filters.el

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

(defun simple-bookmarks-filters-any-p (bookmark)
  t)

(defun simple-bookmarks-filters-file-bookmark-p (bookmark)
  (simple-bookmarks-funcs-type-p 'find-file bookmark))

(defun simple-bookmarks-filters-directory-bookmark-p (bookmark)
  (simple-bookmarks-funcs-type-p 'dired bookmark))

(defun simple-bookmarks-filters-desktop-bookmark-p (bookmark)
  (simple-bookmarks-funcs-type-p 'desktop-change-dir bookmark))

(defun simple-bookmarks-filters-url-bookmark-p (bookmark)
  (simple-bookmarks-funcs-type-p 'browse-url bookmark))

(provide 'simple-bookmarks-filters)
;;; simple-bookmarks-filters.el ends here
