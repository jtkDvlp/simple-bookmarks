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

(defun simple-bookmarks-utils-key (apair)
  (car apair))

(defun simple-bookmarks-utils-val (apair)
  (car (cdr apair)))

(defun simple-bookmarks-utils-get (alist key)
  (car (assoc-default key alist)))

(defun simple-bookmarks-utils-update (alist key new-val)
  (append (assq-delete-all key (copy-alist alist))
          (list (list key new-val))))

(defun simple-bookmarks-utils-update-by-func (alist key func &rest args)
  (simple-bookmarks-utils-update alist key (apply 'funcall func (simple-bookmarks-utils-get alist key) args)))

(provide 'simple-bookmarks-utils)
;;; simple-bookmarks-utils.el ends here
