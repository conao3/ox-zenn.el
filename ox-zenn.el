;;; ox-zenn.el --- Zenn flavored markdown backend for org export engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; URL: https://github.com/conao3/ox-zenn.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Zenn flavored markdown backend for org export engine.
;; Zenn: https://zenn.dev/


;;; Code:

(require 'ox-md)
(require 'ox-publish)

(defgroup ox-zenn nil
  "Zenn flavored markdown backend for org export engine.
Zenn: https://zenn.dev/"
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/ox-zenn.el"))

(org-export-define-derived-backend 'zennmd 'md
  :menu-entry
  '(?z "Export to Zenn Flavored Markdown"
       ((?Z "To temporary buffer"
            (lambda (a s v b) (org-zenn-export-as-markdown a s v)))
        (?z "To file"
            (lambda (a s v b) (org-zenn-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a
                  (org-zenn-export-to-markdown t s v)
                (org-open-file (org-zenn-export-to-markdown nil s v))))))))


;;; functions


;;; frontend

;;;###autoload
(defun org-zenn-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Zenn Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org GFM Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'zennmd "*Org ZennMD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-zenn-convert-region-to-md ()
  "Assume `org-mode' syntax, and convert it to Zenn Flavored Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in `org-mode' syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'zennmd))

;;;###autoload
(defun org-zenn-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Zenn Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'zennmd outfile async subtreep visible-only)))

;;;###autoload
(defun org-zenn-publish-to-gfm (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'zennmd filename ".md" plist pub-dir))

(provide 'ox-zenn)

;;; ox-zenn.el ends here
