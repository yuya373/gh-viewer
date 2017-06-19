;;; gh-viewer-graphql.el ---                         -*- lexical-binding: t; -*-

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
(require 'json)
(require 'github-graphql-client)

(defclass gh-viewer-repositories ()
  ((repositories :initarg :repositories :initform nil :type list)))

(defvar gh-viewer-graphql-repositories nil)

(defcustom gh-viewer-token nil
  "Github Token."
  :group 'gh-viewer)

(defcustom gh-viewer-repository-query-location
  (expand-file-name "./graphql/repository.graphql")
  "Query for fetch repository."
  :group 'gh-viewer)

(defun gh-viewer-repository-query ()
  (let ((path gh-viewer-repository-query-location))
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert-file-contents-literally path nil)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gh-viewer-graphql-handle-json-boolean (json-boolean)
  (if (eq :json-false json-boolean)
      nil
    t))

(defun gh-viewer-graphql-initialize-page-info (page-info)
  (make-instance 'ggc:page-info
                 :end-cursor (plist-get page-info :endCursor)
                 :has-next-page (gh-viewer-graphql-handle-json-boolean (plist-get page-info :has-next-page))))

(defun gh-viewer-graphql-initialize-connection (connection node-builder class)
  (let ((nodes (mapcar node-builder (plist-get connection :nodes)))
        (total-count (plist-get connection :totalCount)))
    (apply #'make-instance class
           :nodes nodes
           :page-info (gh-viewer-graphql-initialize-page-info (plist-get connection :pageInfo))
           (if total-count (list :total-count total-count) nil))))

(defun gh-viewer-graphql-comment-props (comment)
  (list
   :body (decode-coding-string (plist-get comment :body) 'utf-8)
   :author (gh-viewer-graphql-initialize-actor (plist-get comment :author))
   :published-at (plist-get comment :publishedAt)
   :editor (gh-viewer-graphql-initialize-actor (plist-get comment :editor))
   :last-edited-at (plist-get comment :lastEditedAt)))

(defun gh-viewer-graphql-initialize-actor (actor &optional class)
  (when actor
    (make-instance (or class 'ggc:actor)
                   :avatar-url (plist-get actor :avatarUrl)
                   :login (plist-get actor :login)
                   :url (plist-get actor :url))))

(defun gh-viewer-graphql-initialize-user (user)
  (gh-viewer-graphql-initialize-actor user 'ggc:user))

(defun gh-viewer-graphql-initialize-user-connection (connection)
  (gh-viewer-graphql-initialize-connection connection
                                           #'gh-viewer-graphql-initialize-user
                                           'ggc:user-connection))

(defun gh-viewer-graphql-initialize-label (label)
  (let ((name (plist-get label :name))
        (color (plist-get label :color)))
    (make-instance 'ggc:label
                   :name name
                   :color color)))

(defun gh-viewer-graphql-initialize-label-connection (labels)
  (gh-viewer-graphql-initialize-connection labels
                                           #'gh-viewer-graphql-initialize-label
                                           'ggc:label-connection))

(defun gh-viewer-graphql-initialize-issue-comment (comment)
  (apply #'make-instance 'ggc:issue-comment (gh-viewer-graphql-comment-props comment)))

(defun gh-viewer-graphql-initialize-issue-comment-connection (comments)
  (gh-viewer-graphql-initialize-connection comments
                                           #'gh-viewer-graphql-initialize-issue-comment
                                           'ggc:issue-comment-connection))

(defun gh-viewer-graphql-initialize-review-request (review-request)
  (let ((reviewer (gh-viewer-graphql-initialize-user (plist-get review-request :reviewer))))
    (make-instance 'ggc:review-request
                   :reviewer reviewer)))

(defun gh-viewer-graphql-initialize-review-request-connection (review-requests)
  (gh-viewer-graphql-initialize-connection review-requests
                                           #'gh-viewer-graphql-initialize-review-request
                                           'ggc:review-request-connection))

(defun gh-viewer-graphql-initialize-pull-request-review (pull-request-review)
  (apply #'make-instance 'ggc:pull-request-review
         :state (plist-get pull-request-review :state)
         :comments (gh-viewer-graphql-initialize-issue-comment-connection (plist-get pull-request-review :comments))
         (gh-viewer-graphql-comment-props pull-request-review)))

(defun gh-viewer-graphql-initialize-pull-request-review-connection (reviews)
  (gh-viewer-graphql-initialize-connection reviews
                                           #'gh-viewer-graphql-initialize-pull-request-review
                                           'ggc:pull-request-review-connection))

(defun gh-viewer-graphql-initialize-pull-request (pull-request)
  (let ((assignees (gh-viewer-graphql-initialize-user-connection
                    (plist-get pull-request :assignees)))
        (labels (gh-viewer-graphql-initialize-label-connection
                 (plist-get pull-request :labels)))
        (comments
         (gh-viewer-graphql-initialize-issue-comment-connection
          (plist-get pull-request :comments)))
        (review-requests
         (gh-viewer-graphql-initialize-review-request-connection
          (plist-get pull-request :reviewRequests)))
        (reviews
         (gh-viewer-graphql-initialize-pull-request-review-connection
          (plist-get pull-request :reviews))))
    (make-instance 'ggc:pull-request
                   :assignees assignees
                   :labels labels
                   :comments comments
                   :review-requests review-requests
                   :reviews reviews
                   :id (plist-get pull-request :id)
                   :number (plist-get pull-request :number)
                   :title (decode-coding-string
                           (plist-get pull-request :title)
                           'utf-8)
                   :author (gh-viewer-graphql-initialize-actor (plist-get pull-request :author))
                   :body (decode-coding-string
                          (plist-get pull-request :body)
                          'utf-8)
                   :head-ref-name (plist-get pull-request :headRefName)
                   :base-ref-name (plist-get pull-request :baseRefName)
                   :merged (gh-viewer-graphql-handle-json-boolean (plist-get pull-request :merged))
                   :merged-at (plist-get pull-request :mergedAt)
                   :state (plist-get pull-request :state)
                   :url (plist-get pull-request :url)
                   :published-at (plist-get pull-request :publishedAt)
                   )))


(defun gh-viewer-graphql-initialize-pull-request-connection (pull-requests)
  (let ((conn (gh-viewer-graphql-initialize-connection
               pull-requests
               #'gh-viewer-graphql-initialize-pull-request
               'ggc:pull-request-connection)))
    (oset conn
          total-count
          (plist-get pull-requests :totalCount))
    conn))

(defun gh-viewer-graphql-initialize-repository (repository)
  (let* ((pull-requests (gh-viewer-graphql-initialize-pull-request-connection (plist-get repository :pullRequests)))
         (repo (make-instance 'ggc:repository
                              :id (plist-get repository :id)
                              :name (plist-get repository :name)
                              :name-with-owner (plist-get repository :nameWithOwner)
                              :pull-requests pull-requests
                              )))
    (setq gh-viewer-graphql-repositories
          (cons repo (cl-remove-if #'(lambda (e) (string= (oref e id)
                                                          (oref repo id)))
                                   gh-viewer-graphql-repositories)))))

(cl-defun gh-viewer-fetch-repository (owner name &key
                                            (success nil)
                                            (error nil)
                                            (http-error nil)
                                            (pull-request-states '("OPEN"))
                                            (pull-request-order-field "CREATED_AT")
                                            (pull-request-order-direction "DESC"))
  (let ((query (gh-viewer-repository-query))
        (token gh-viewer-token)
        (variables (list (cons "owner" owner)
                         (cons "name" name)
                         (cons "pullRequestStates" pull-request-states)
                         (cons "pullRequestOrderField" pull-request-order-field)
                         (cons "pullRequestOrderDirection" pull-request-order-direction))))
    (cl-labels
        ((on-success (data)
                     (gh-viewer-graphql-initialize-repository
                      (plist-get (plist-get data :data) :repository))
                     (when (functionp success)
                       (funcall success)))
         (on-error (data)
                   (message (format "%s" (plist-get data :errors)))
                   (when (functionp error)
                     (funcall (plist-get data :errors))))
         (on-http-error (&key error-thrown &allow-other-keys)
                        (message (format "%s" error-thrown))
                        (when (functionp http-error)
                          (funcall http-error error-thrown))))
      (github-graphql-client-request query token
                                     :success #'on-success
                                     :error #'on-error
                                     :http-error #'on-http-error
                                     :variables variables))))

(gh-viewer-fetch-repository "rebaseinc" "instabase")

(provide 'gh-viewer-graphql)
;;; gh-viewer-graphql.el ends here
