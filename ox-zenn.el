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

(provide 'ox-zenn)

;;; ox-zenn.el ends here
