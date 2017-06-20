;;; gh-viewer-stringify-short.el ---         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:
(require 'eieio)
(require 'github-graphql-client)

(defmethod gh-viewer-stringify-short ((pr ggc:pull-request))
  (with-slots (number state title comments) pr
    (let ((comments-count (format "%2d" (oref comments total-count)))
          (author (format "by %s" (gh-viewer-stringify (oref pr author)))))
      (format "#%s [%s] [%s] %s %s" (format "%4d" number) state comments-count (propertize title 'face 'bold) author))))

(defmethod gh-viewer-stringify-short ((repo ggc:repository))
  (oref repo name-with-owner))

(provide 'gh-viewer-stringify-short)
;;; gh-viewer-stringify-short.el ends here
