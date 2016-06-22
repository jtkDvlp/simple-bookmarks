# Simple Bookmarks

Simple Bookmarks is a smart enhancement for Emacs. It provides a simple but convenient interface to your recently and often used bookmarks / functioncalls.

Actually it just holds a persistence list on disk consisting of named function calls with relevant arguments (your bookmarks). The following things are supported:
- Add/remove/execute(call) bookmarks consisting of a name, a function (any function), some relevant arguments/parameters and any other information you want to save with the bookmark
- Add/remove/execute(call) already specialized bookmarks for
  - find-file
  - dired
  - desktop (supports both add existing desktops to bookmark-list and create desktops)
  - browse-url
- Mini-Buffer-Completion for the functions above (should work with ido and ivy)
- Filter the bookmark-list by type/func what ever you want

## Getting started

### Get it

Download it and [set up your load path](http://www.emacswiki.org/emacs/InstallingPackages).

### Usage
```
;; the bookmark file will automatically be loaded
(require 'simple-bookmarks)

;; handles all bookmarks
(global-set-key (kbd "M-- l") 'sbi/execute-from-all)
(global-set-key (kbd "M-- L") 'sbi/execute)
(global-set-key (kbd "M-- c") 'sbi/add)
(global-set-key (kbd "M-- r") 'sbi/remove-from-all)
(global-set-key (kbd "M-- R") 'sbi/remove)

;; handles only file-bookmarks
(global-set-key (kbd "M-- f l") 'sbi/execute-file)
(global-set-key (kbd "M-- f c") 'sbi/add-file)
(global-set-key (kbd "M-- f r") 'sbi/remove-file)

;; handles only directory-bookmarks
(global-set-key (kbd "M-- d l") 'sbi/execute-directory)
(global-set-key (kbd "M-- d c") 'sbi/add-directory)
(global-set-key (kbd "M-- d r") 'sbi/remove-directory)

;; handles only desktop-bookmarks
(global-set-key (kbd "M-- s l") 'sbi/execute-desktop)
(global-set-key (kbd "M-- s c") 'sbi/create-desktop)
(global-set-key (kbd "M-- s a") 'sbi/add-desktop)
(global-set-key (kbd "M-- s r") 'sbi/remove-desktop)

;; handles only url-bookmarks
(global-set-key (kbd "M-- u l") 'sbi/execute-url)
(global-set-key (kbd "M-- u c") 'sbi/add-url)
(global-set-key (kbd "M-- u r") 'sbi/remove-url)
```
## Customization

If there are other functions you often use, maybe it´s advisable to save these function calls as bookmark. You can make your own bookmark functions. See the functions for file-bookmarks below.
```
(defun sbi/add-file (&optional name path more)
  (interactive "Sfile-bookmark name: \nffile-bookmark path:\ni")
  (sbi/add name 'find-file (list path) more))
```
The two functions below are just filtered versions of "sbi/remove", so these are not really needed. If you want to provide them, of course you has to provide the corresponding filter.
```
(defun sbi/remove-file ()
  (interactive)
  (sbi/remove 'sbf/file-bookmark-p))

(defun sbi/execute-file ()
  (interactive)
  (sbi/execute 'sbf/file-bookmark-p))
```
## Appendix
Simple Bookmarks are my first steps into Elisp. I´d be thankful to receive patches, comments and constructive criticism.

Hope the enhancement is useful :-)

## License

MIT License

Copyright (c) 2016 Julian T. Knabenschuh

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.