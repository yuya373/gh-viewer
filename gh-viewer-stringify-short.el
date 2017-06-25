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
(require 'gh-viewer-graphql)

(defmethod gh-viewer-stringify-short ((conn ggc:pull-request-review-connection))
  (with-slots (nodes) conn
    (cl-remove-duplicates (cl-remove-if #'null (mapcar #'(lambda (e) (oref e state)) nodes))
                          :test #'string=)))

(defun gh-viewer-review-status-face (state)
  (cond
   ((cl-typep state 'ggc:changes-requested) 'error)
   ((cl-typep state 'ggc:commented) 'warning)
   ((cl-typep state 'ggc:approved) 'success)))

(defmethod gh-viewer-stringify-short ((conn ggc:label-connection))
  (with-slots (nodes) conn
    (let ((str (mapconcat #'gh-viewer-stringify nodes ", ")))
      (if (gh-viewer-blank? str) ""
        (format " [%s]" str)))))

(defmethod gh-viewer-stringify-short ((issue gh-viewer-issue))
  (with-slots (number state title comments) issue
    (let* ((comments-count (format "%2d" (oref comments total-count)))
           (author (format "by %s" (gh-viewer-stringify (oref issue author))))
           (new (if (oref issue new)
                    (propertize "*" 'face 'error)
                  " "))
           (labels (gh-viewer-stringify-short (oref issue labels))))
      (format "%s #%s [%s] [%s] %s %s%s"
              new
              (format "%4d" number)
              state
              (if (oref (oref issue comments) has-new-comments)
                  (propertize comments-count 'face 'error)
                comments-count)
              (propertize title 'face 'bold)
              author
              labels))))

(defmethod gh-viewer-stringify-short ((pr gh-viewer-pull-request))
  (let ((str (call-next-method))
        (review-states (gh-viewer-stringify-short (oref pr reviews))))
    (format "%s%s" str
            (if (< 0 (length review-states))
                (format " [%s]" (mapconcat #'identity
                                           (mapcar #'(lambda (e) (propertize e
                                                                             'face (gh-viewer-review-status-face e)))
                                                   review-states)
                                           ", "))
              ""))))

(defmethod gh-viewer-stringify-short ((repo ggc:repository))
  (oref repo name-with-owner))

(provide 'gh-viewer-stringify-short)
;;; gh-viewer-stringify-short.el ends here
