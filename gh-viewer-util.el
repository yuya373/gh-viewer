;;; gh-viewer-util.el ---                            -*- lexical-binding: t; -*-

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

(defun gh-viewer-format-time-string (time-string)
  (format-time-string "%Y-%m-%d %H:%M:%S"
                      (gh-viewer-parse-time-string time-string)))

(defun gh-viewer-parse-time-string (time-string)
  (date-to-time time-string))

(defun gh-viewer-convert-unix-time-string (time-string)
  (format-time-string "%s" (gh-viewer-parse-time-string time-string)))

(provide 'gh-viewer-util)
;;; gh-viewer-util.el ends here
