;;; simple-bookmarks-funcs.el

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

(require 'simple-bookmarks-utils)
(require 'cl-lib)

(defvar simple-bookmarks-funcs-added-hook nil)
(defvar simple-bookmarks-funcs-removed-hook nil)
(defvar simple-bookmarks-funcs-executed-hook nil)
(defvar simple-bookmarks-funcs-loaded-hook nil)
(defvar simple-bookmarks-funcs-saved-hook nil)
(defvar simple-bookmarks-funcs-added-saved-hook nil)
(defvar simple-bookmarks-funcs-removed-saved-hook nil)

(defvar simple-bookmarks-funcs-prompt-really "really?: ")

(defun simple-bookmarks-funcs-create (name func args &optional more)
  "Use the `more` arg to input any information you want in the bookmark. The `more` arg should be an `alist` or `nil`."
  (append more
          (list (list 'name name))
          (list (list 'func func))
          (list (list 'args args))))

(defun simple-bookmarks-funcs-get (bookmarks name)
  (simple-bookmarks-utils-get (simple-bookmarks-funcs-list bookmarks) name))

(defun simple-bookmarks-funcs-list (bookmarks)
  (simple-bookmarks-utils-get bookmarks 'list))

(defun simple-bookmarks-funcs-names (bookmarks)
  (mapcar 'simple-bookmarks-utils-key (simple-bookmarks-funcs-list bookmarks)))

(defun simple-bookmarks-funcs-type-p (type bookmark)
  (cl-equalp type (simple-bookmarks-utils-get bookmark 'func)))

(defun simple-bookmarks-funcs-add (bookmarks new-bookmark)
  (let ((result (simple-bookmarks-utils-update
                 bookmarks
                 'list (simple-bookmarks-utils-update
                        (simple-bookmarks-funcs-list bookmarks)
                        (simple-bookmarks-utils-get new-bookmark 'name)
                        new-bookmark))))
    (run-hook-with-args 'simple-bookmarks-funcs-added-hook new-bookmark result)
    result))

(defun simple-bookmarks-funcs-remove (bookmarks old-bookmark)
  (let ((result (simple-bookmarks-utils-update
                 bookmarks
                 'list (assq-delete-all (simple-bookmarks-utils-get old-bookmark 'name)
                                        (copy-alist (simple-bookmarks-funcs-list bookmarks))))))
    (run-hook-with-args 'simple-bookmarks-funcs-removed-hook old-bookmark result)
    result))

(defun simple-bookmarks-funcs-execute (bookmark)
  (let* ((func (simple-bookmarks-utils-get bookmark 'func))
         (args (simple-bookmarks-utils-get bookmark 'args))
         (prompt (simple-bookmarks-utils-get bookmark 'prompt))
         (executep (if prompt
                       (string-equal "yes" (completing-read simple-bookmarks-funcs-prompt-really '("yes" "no") nil t))
                     t)))
    (when executep
      (let ((result (apply 'funcall func args)))
        (run-hook-with-args 'simple-bookmarks-funcs-executed-hook bookmark result)
        result))))

(defun simple-bookmarks-funcs-read (file)
  (let ((result (list (list 'file file)
                      (list 'list (if (file-exists-p file)
                                      (read (with-temp-buffer
                                              (insert-file-contents file)
                                              (buffer-string)))
                                    '())))))
    (run-hook-with-args 'simple-bookmarks-funcs-loaded-hook file result)
    result))

(defun simple-bookmarks-funcs-write (bookmarks)
  (let ((file (simple-bookmarks-utils-get bookmarks 'file))
        (list (simple-bookmarks-utils-get bookmarks 'list)))
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (insert (prin1-to-string list))))
    (run-hook-with-args 'simple-bookmarks-funcs-saved-hook file bookmarks)
    bookmarks))

(defun simple-bookmarks-funcs-add-save (bookmarks new-bookmark)
  (let ((result (simple-bookmarks-funcs-write (simple-bookmarks-funcs-add bookmarks new-bookmark))))
    (run-hook-with-args 'simple-bookmarks-funcs-added-saved-hook new-bookmark result)
    result))

(defun simple-bookmarks-funcs-remove-save (bookmarks old-bookmark)
  (let ((result (simple-bookmarks-funcs-write (simple-bookmarks-funcs-remove bookmarks old-bookmark))))
    (run-hook-with-args 'simple-bookmarks-funcs-removed-saved-hook old-bookmark result)
    result))

(defun simple-bookmarks-funcs-execute-by-name (bookmarks name)
  (simple-bookmarks-funcs-execute (simple-bookmarks-funcs-get bookmarks name)))

(provide 'simple-bookmarks-funcs)
;;; simple-bookmarks-funcs.el ends here
