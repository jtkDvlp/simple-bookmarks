# Simple Bookmarks [![MELPA](http://melpa.org/packages/simple-bookmarks-badge.svg)](http://melpa.org/#/simple-bookmarks) [![MELPA Stable](https://stable.melpa.org/packages/simple-bookmarks-badge.svg)](https://stable.melpa.org/#/simple-bookmarks)

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

- Via [![MELPA](http://melpa.org/packages/simple-bookmarks-badge.svg)](http://melpa.org/#/simple-bookmarks) or [![MELPA Stable](https://stable.melpa.org/packages/simple-bookmarks-badge.svg)](https://stable.melpa.org/#/simple-bookmarks)
- Or manually download it and [set up your load path](http://www.emacswiki.org/emacs/InstallingPackages).

### Usage
```
(require 'simple-bookmarks)
(simple-bookmarks-init)

;; handles all bookmarks
(global-set-key (kbd "M-- l") 'simple-bookmarks-interactive-execute-from-all)
(global-set-key (kbd "M-- L") 'simple-bookmarks-interactive-execute)
(global-set-key (kbd "M-- c") 'simple-bookmarks-interactive-add)
(global-set-key (kbd "M-- r") 'simple-bookmarks-interactive-remove-from-all)
(global-set-key (kbd "M-- R") 'simple-bookmarks-interactive-remove)

;; handles only file-bookmarks
(global-set-key (kbd "M-- f l") 'simple-bookmarks-interactive-execute-file)
(global-set-key (kbd "M-- f c") 'simple-bookmarks-interactive-add-file)
(global-set-key (kbd "M-- f r") 'simple-bookmarks-interactive-remove-file)

;; handles only directory-bookmarks
(global-set-key (kbd "M-- d l") 'simple-bookmarks-interactive-execute-directory)
(global-set-key (kbd "M-- d c") 'simple-bookmarks-interactive-add-directory)
(global-set-key (kbd "M-- d r") 'simple-bookmarks-interactive-remove-directory)

;; handles only desktop-bookmarks
(global-set-key (kbd "M-- s l") 'simple-bookmarks-interactive-execute-desktop)
(global-set-key (kbd "M-- s c") 'simple-bookmarks-interactive-create-desktop)
(global-set-key (kbd "M-- s a") 'simple-bookmarks-interactive-add-desktop)
(global-set-key (kbd "M-- s r") 'simple-bookmarks-interactive-remove-desktop)

;; handles only url-bookmarks
(global-set-key (kbd "M-- u l") 'simple-bookmarks-interactive-execute-url)
(global-set-key (kbd "M-- u c") 'simple-bookmarks-interactive-add-url)
(global-set-key (kbd "M-- u r") 'simple-bookmarks-interactive-remove-url)
```
## Customization

If there are other functions you often use, maybe it�s advisable to save these function calls as bookmark. You can make your own bookmark functions. See the functions for file-bookmarks below.
```
(defun simple-bookmarks-interactive-add-file (&optional name path more)
  (interactive "Sfile-bookmark name: \nffile-bookmark path:\ni")
  (simple-bookmarks-interactive-add name 'find-file (list path) more))
```
The two functions below are just filtered versions of "simple-bookmarks-interactive-remove-file" and "simple-bookmarks-interactive-execute-file", so these are not really needed. If you want to provide them, of course you has to provide the corresponding filter.
```
(defun simple-bookmarks-interactive-remove-file ()
  (interactive)
  (simple-bookmarks-interactive-remove 'simple-bookmarks-filters-file-bookmark-p))

(defun simple-bookmarks-interactive-execute-file ()
  (interactive)
  (simple-bookmarks-interactive-execute 'simple-bookmarks-filters-file-bookmark-p))
```
## Appendix
Simple Bookmarks are my first steps into Elisp. I�d be thankful to receive patches, comments and constructive criticism.

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