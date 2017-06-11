;;; gh-viewer-notification.el ---                    -*- lexical-binding: t; -*-

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
(require 'gh-viewer-issue)
(require 'gh-viewer-pull-request)

(defclass gh-viewer-notification ()
  ((repo :initarg :repo :type gh-viewer-repo)
   (changes :initarg :changes :initform nil)
   (issue :initarg :issue)
   (title-builder :initarg :title-builder :initform nil)
   (message-builder :initarg :message-builder :initform nil)))

(defclass gh-viewer-issue-notification
  (gh-viewer-notification)
  ())

(defclass gh-viewer-pull-request-notification
  (gh-viewer-notification)
  ())

(cl-defun gh-viewer-notification-create
    (new-issue old-issue repo &key
               (title-builder #'gh-viewer-notification-title)
               (message-builder #'gh-viewer-notification-message))
  (let ((class (if (gh-viewer-pull-request-p new-issue)
                   'gh-viewer-pull-request-notification
                 'gh-viewer-issue-notification))
        (changes (and old-issue
                      (gh-viewer-issue-changes new-issue old-issue))))
    (if (or changes (not old-issue))
        (make-instance class
                       :repo repo
                       :changes changes
                       :issue new-issue
                       :title-builder title-builder
                       :message-builder message-builder))))

(defmethod gh-viewer-repo-to-string
  ((notification gh-viewer-notification))
  (gh-viewer-repo-to-string (oref notification repo)))

(defmethod gh-viewer-notification-issue-state
  ((notification gh-viewer-notification))
  (or (and (oref notification changes) "Updated")
      "Created"))

(defmethod gh-viewer-notification-title
  ((notification gh-viewer-issue-notification))
  (format "\\[%s] Issue %s"
          (gh-viewer-repo-to-string notification)
          (gh-viewer-notification-issue-state notification)))

(defmethod gh-viewer-notification-title
  ((notification gh-viewer-pull-request-notification))
  (format "\\[%s] Pull Request %s"
          (gh-viewer-repo-to-string notification)
          (gh-viewer-notification-issue-state notification)))

(defmethod gh-viewer-notification-issue-username
  ((notification gh-viewer-notification))
  (gh-viewer-issue-user-name (oref notification issue)))

(defmethod gh-viewer-notification-message
  ((notification gh-viewer-notification))
  (with-slots (number title) (oref notification issue)
    (format "#%s %s by %s"
            number title
            (gh-viewer-notification-issue-username notification))))

(defmethod gh-viewer-notification-notify
  ((notification gh-viewer-notification))
  (with-slots (title-builder message-builder) notification
    (let ((message (and (functionp message-builder)
                        (funcall message-builder notification)))
          (title (and (functionp title-builder)
                      (funcall title-builder notification))))
      (and message
           (alert message :title title)))))


(provide 'gh-viewer-notification)
;;; gh-viewer-notification.el ends here
