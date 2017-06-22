;;; gh-viewer-fetcher.el ---                         -*- lexical-binding: t; -*-

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
(require 'gh-viewer-repo)
(require 'gh-viewer-graphql)

(defcustom gh-viewer-token nil
  "Github Token."
  :group 'gh-viewer)

(defcustom gh-viewer-repository-query-location
  (concat user-emacs-directory "el-get/gh-viewer/graphql/repository.graphql")
  "Query for fetch repository."
  :group 'gh-viewer)

(defcustom gh-viewer-pull-request-comments-query-location
  (concat user-emacs-directory "el-get/gh-viewer/graphql/pull-request-comments.graphql")
  "Query for fetch pull-request-comments."
  :group 'gh-viewer)

(defvar gh-viewer-before-merge-hook nil
  "Hook runs before Repository merged.\n`(lambda (new-repository old-repository) ...)'.")

(defmethod gh-viewer-notifier ((new-repository ggc:repository) old-repository)
  (alert "Hook runs!!!" :title (gh-viewer-stringify-short new-repository)))

(defun gh-viewer-query (location)
  (let ((path location))
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert-file-contents-literally path nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun gh-viewer-fetch-repository (owner name &key
                                            (success nil)
                                            (error nil)
                                            (http-error nil)
                                            (pull-request-states '("OPEN"))
                                            (pull-request-order-field "CREATED_AT")
                                            (pull-request-order-direction "DESC"))
  (let ((query (gh-viewer-query gh-viewer-repository-query-location))
        (token gh-viewer-token)
        (variables (list (cons "owner" owner)
                         (cons "name" name)
                         (cons "pullRequestStates" pull-request-states)
                         (cons "pullRequestOrderField" pull-request-order-field)
                         (cons "pullRequestOrderDirection" pull-request-order-direction))))
    (cl-labels
        ((on-success (data base pull-request-before)
                     (let* ((repo (gh-viewer-graphql-initialize-repository
                                   (plist-get (plist-get data :data) :repository))))

                       (when base
                         (gh-viewer-merge base repo t)
                         (setq repo base))

                       (if (gh-viewer-has-more (oref repo pull-requests)
                                               pull-request-order-direction)
                           (fetch repo
                                  (gh-viewer-next-cursor (oref repo pull-requests)
                                                         pull-request-order-direction))
                         (when (functionp success)
                           (funcall success repo)))
                       ))
         (on-error (data)
                   (message (format "%s" (plist-get data :errors)))
                   (when (functionp error)
                     (funcall (plist-get data :errors))))
         (on-http-error (&key error-thrown &allow-other-keys)
                        (message (format "%s" error-thrown))
                        (when (functionp http-error)
                          (funcall http-error error-thrown)))
         (fetch (&optional base pull-request-before)
                (github-graphql-client-request
                 query token
                 :success #'(lambda (data) (on-success data base pull-request-before))
                 :error #'on-error
                 :http-error #'on-http-error
                 :variables (if pull-request-before
                                (cons (cons "pullRequestBefore" pull-request-before)
                                      variables)
                              variables))))
      (fetch))))

(cl-defmethod gh-viewer-fetch ((repo gh-viewer-repo) &optional success error)
  (cl-labels
      ((on-success (new-repository)
                   (if (oref repo repository)
                       (run-hook-with-args 'gh-viewer-before-merge-hook new-repository (oref repo repository))
                       (gh-viewer-merge (oref repo repository) new-repository)
                     (oset repo repository new-repository))

                   (oset repo last-fetched (time-to-seconds))
                   (when (functionp success)
                     (funcall success (oref repo repository)))
                   (message "Fetched %s" (gh-viewer-stringify repo)))
       (on-error (errors) (message "Errors: %s" errors)))
    (message "Fetching %s ..." (gh-viewer-stringify repo))
    (gh-viewer-fetch-repository (oref repo owner) (oref repo name)
                                :success #'on-success :error error)))

(cl-defmethod gh-viewer-fetch ((conn ggc:issue-comment-connection) pull-request repository
                               &optional success error)
  (let* ((next-cursor (gh-viewer-next-cursor conn "ASC"))
         (owner (oref (oref repository owner) login))
         (name (oref repository name))
         (query (gh-viewer-query gh-viewer-pull-request-comments-query-location))
         (variables (list (cons "owner" owner)
                          (cons "name" name)
                          (cons "pullRequestNumber" (oref pull-request number)))))
    (cl-labels
        ((on-success (data base)
                     (let ((new-conn (gh-viewer-graphql-initialize-issue-comment-connection
                                      (plist-get
                                       (plist-get
                                        (plist-get
                                         (plist-get data :data)
                                         :repository)
                                        :pullRequest)
                                       :comments))))
                       (gh-viewer-merge new-conn base)
                       (oset pull-request comments new-conn)
                       (if (gh-viewer-has-more (oref pull-request comments) "ASC")
                           (fetch (oref pull-request comments)
                                  (gh-viewer-next-cursor (oref pull-request comments)
                                                         "ASC"))
                         (when (functionp success)
                           (funcall success)))))
         (on-error (data)
                   (message "Error: %s" data))
         (fetch (&optional base next-cursor)
                (github-graphql-client-request
                 query
                 gh-viewer-token
                 :success #'(lambda (data) (on-success data base))
                 :error #'on-error
                 :variables (if next-cursor
                                (cons (cons "commentsAfter" next-cursor)
                                      variables)
                              variables))))
      (fetch conn next-cursor))))

(provide 'gh-viewer-fetcher)
;;; gh-viewer-fetcher.el ends here
