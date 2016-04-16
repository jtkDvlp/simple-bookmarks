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

(require 'simple-bookmarks-utils)

(defvar sbf/added-hook nil)
(defvar sbf/removed-hook nil)
(defvar sbf/executed-hook nil)
(defvar sbf/loaded-hook nil)
(defvar sbf/saved-hook nil)
(defvar sbf/added-saved-hook nil)
(defvar sbf/removed-saved-hook nil)

(defvar sbf/prompt-really "really?: ")

(defun sbf/create (name func args &optional more)
  (append more
          (list (list 'name name))
          (list (list 'func func))
          (list (list 'args args))))

(defun sbf/get (bookmarks name)
  (sbu/get (sbf/list bookmarks) name))

(defun sbf/list (bookmarks)
  (sbu/get bookmarks 'list))

(defun sbf/names (bookmarks)
  (mapcar 'sbu/key (sbf/list bookmarks)))

(defun sbf/type-p (type bookmark)
  (equalp type (sbu/get bookmark 'func)))

(defun sbf/add (bookmarks new-bookmark)
  (let ((result (sbu/update
                 bookmarks
                 'list (sbu/update
                        (sbf/list bookmarks)
                        (sbu/get new-bookmark 'name)
                        new-bookmark))))
    (run-hook-with-args 'sbf/added-hook new-bookmark result)
    result))

(defun sbf/remove (bookmarks old-bookmark)
  (let ((result (sbu/update
                 bookmarks
                 'list (assq-delete-all (sbu/get old-bookmark 'name)
                                        (copy-alist (sbf/list bookmarks))))))
    (run-hook-with-args 'sbf/removed-hook old-bookmark result)
    result))

(defun sbf/execute (bookmark)
  (let* ((func (sbu/get bookmark 'func))
         (args (sbu/get bookmark 'args))
         (prompt (sbu/get bookmark 'prompt))
         (executep (if prompt
                       (string-equal "yes" (completing-read sbf/prompt-really '("yes" "no") nil t))
                     t)))
    (when executep
      (let ((result (apply 'funcall func args)))
        (run-hook-with-args 'sbf/executed-hook bookmark result)
        result))))

(defun sbf/read (file)
  (let ((result (list (list 'file file)
                      (list 'list (if (file-exists-p file)
                                      (read (with-temp-buffer
                                              (insert-file-contents file)
                                              (buffer-string)))
                                    '())))))
    (run-hook-with-args 'sbf/loaded-hook file result)
    result))

(defun sbf/write (bookmarks)
  (let ((file (sbu/get bookmarks 'file))
        (list (sbu/get bookmarks 'list)))
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (insert (prin1-to-string list))))
    (run-hook-with-args 'sbf/saved-hook file bookmarks)
    bookmarks))

(defun sbf/add-save (bookmarks new-bookmark)
  (let ((result (sbf/write (sbf/add bookmarks new-bookmark))))
    (run-hook-with-args 'sbf/added-saved-hook new-bookmark result)
    result))

(defun sbf/remove-save (bookmarks old-bookmark)
  (let ((result (sbf/write (sbf/remove bookmarks old-bookmark))))
    (run-hook-with-args 'sbf/removed-saved-hook old-bookmark result)
    result))

(defun sbf/execute-by-name (bookmarks name)
  (sbf/execute (sbf/get bookmarks name)))

(provide 'simple-bookmarks-funcs)
