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

(defclass gh-viewer-pull-request (ggc:pull-request)
  ((new :initform nil :type boolean)))

(defclass gh-viewer-issue-comment (ggc:issue-comment)
  ((new :initform nil :type boolean)))

(defclass gh-viewer-issue-comment-connection (ggc:issue-comment-connection)
  ((has-new-comments :initform nil :type boolean)))

(defvar gh-viewer-graphql-repositories nil)

(defcustom gh-viewer-token nil
  "Github Token."
  :group 'gh-viewer)

(defcustom gh-viewer-repository-query-location
  (concat user-emacs-directory "el-get/gh-viewer/graphql/repository.graphql")
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
                 :has-previous-page (gh-viewer-graphql-handle-json-boolean
                                     (plist-get page-info :hasPreviousPage))
                 :start-cursor (plist-get page-info :startCursor)
                 :end-cursor (plist-get page-info :endCursor)
                 :has-next-page (gh-viewer-graphql-handle-json-boolean
                                 (plist-get page-info :hasNextPage))))

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
  (apply #'make-instance 'gh-viewer-issue-comment
         :id (plist-get comment :id)
         (gh-viewer-graphql-comment-props comment)))

(defun gh-viewer-graphql-initialize-issue-comment-connection (comments)
  (gh-viewer-graphql-initialize-connection comments
                                           #'gh-viewer-graphql-initialize-issue-comment
                                           'gh-viewer-issue-comment-connection))

(defun gh-viewer-graphql-initialize-review-request (review-request)
  (let ((reviewer (gh-viewer-graphql-initialize-user
                   (plist-get review-request :reviewer))))
    (make-instance 'ggc:review-request
                   :reviewer reviewer)))

(defun gh-viewer-graphql-initialize-review-request-connection (review-requests)
  (gh-viewer-graphql-initialize-connection
   review-requests
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
    (make-instance 'gh-viewer-pull-request
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

(defun gh-viewer-graphql-initialize-repository-owner (owner)
  (make-instance 'ggc:repository-owner
                 :avatar-url (plist-get owner :avatarUrl)
                 :login (plist-get owner :login)
                 :url (plist-get owner :url)))

(defun gh-viewer-graphql-initialize-repository (repository)
  (let* ((pull-requests (gh-viewer-graphql-initialize-pull-request-connection
                         (plist-get repository :pullRequests)))
         (owner (gh-viewer-graphql-initialize-repository-owner
                 (plist-get repository :owner)))
         (id (plist-get repository :id))
         (repo (make-instance 'ggc:repository
                              :id id
                              :name (plist-get repository :name)
                              :name-with-owner (plist-get repository :nameWithOwner)
                              :owner owner
                              :pull-requests pull-requests))
         (existing (gh-viewer-find-repository id)))
    (if existing
        (gh-viewer-merge existing repo)
      (setq gh-viewer-graphql-repositories
            (cons repo gh-viewer-graphql-repositories))
      repo)))

(defmethod gh-viewer-equal-p ((comment ggc:issue-comment) other)
  (string= (oref comment id) (oref other id)))

(defmethod gh-viewer-equal-p ((pr ggc:pull-request) other)
  (string= (oref pr id) (oref other id)))

(defmethod gh-viewer-merge ((base ggc:repository) other)
  (oset base name (oref other name))
  (oset base owner (oref other owner))
  (oset base name-with-owner (oref other name))
  (oset base pull-requests (gh-viewer-merge
                            (oref base pull-requests)
                            (oref other pull-requests)))
  base)

(defmethod gh-viewer-merge ((new-comment gh-viewer-issue-comment) old-comment)
  (if old-comment
      (oset new-comment new (oref old-comment new))
    (oset new-comment new t)))

(defmethod gh-viewer-merge ((base gh-viewer-issue-comment-connection) old)
  (let ((old-comments (oref old nodes))
        (new-comments (oref base nodes)))
    (cl-loop for new-comment in new-comments
             do (let ((old-comment (cl-find-if #'(lambda (e) (gh-viewer-equal-p new-comment e))
                                               old-comments)))
                  (gh-viewer-merge new-comment old-comment)))

    (mapc #'(lambda (e) (cl-pushnew e new-comments :test #'gh-viewer-equal-p))
          old-comments)

    (if (or (oref old has-new-comments) (cl-find-if #'(lambda (e) (oref e new)) new-comments))
        (oset base has-new-comments t))))

(defmethod gh-viewer-merge ((pr gh-viewer-pull-request) old)
  (if old
      (progn
        (oset pr new (oref old new))
        (gh-viewer-merge (oref pr comments) (oref old comments)))
    (oset pr new t)))

(defmethod gh-viewer-merge ((base ggc:pull-request-connection) other)
  (oset base page-info (oref other page-info))
  (let ((old-pull-requests (oref base nodes))
        (new-pull-requests (oref other nodes)))
    (if (< (length old-pull-requests) 1)
        (oset base nodes new-pull-requests)
      (cl-loop for pr in new-pull-requests
               do (let ((old (cl-find-if #'(lambda (e) (gh-viewer-equal-p pr e)) old-pull-requests)))
                    (gh-viewer-merge pr old)))

      (mapc #'(lambda (pr) (cl-pushnew pr new-pull-requests :test #'gh-viewer-equal-p))
            old-pull-requests)

      (oset base nodes (cl-sort new-pull-requests #'> :key #'(lambda (pr) (oref pr number))))))
  base)

(defmethod gh-viewer-has-more ((conn ggc:pull-request-connection) direction)
  (with-slots (page-info) conn
    (if (string= direction "DESC")
        (oref page-info has-previous-page)
      (oref page-info has-next-page))))

(defmethod gh-viewer-next-cursor ((conn ggc:pull-request-connection) direction)
  (with-slots (page-info) conn
    (if (string= direction "DESC")
        (oref page-info start-cursor)
      (oref page-info end-cursor))))

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
                     (let ((repo (gh-viewer-graphql-initialize-repository
                                  (plist-get (plist-get data :data) :repository))))
                       (if (gh-viewer-has-more (oref repo pull-requests)
                                               pull-request-order-direction)
                           (fetch (gh-viewer-next-cursor (oref repo pull-requests)
                                                         pull-request-order-direction))

                         (when (functionp success)
                           (funcall success repo)))))
         (on-error (data)
                   (message (format "%s" (plist-get data :errors)))
                   (when (functionp error)
                     (funcall (plist-get data :errors))))
         (on-http-error (&key error-thrown &allow-other-keys)
                        (message (format "%s" error-thrown))
                        (when (functionp http-error)
                          (funcall http-error error-thrown)))
         (fetch (&optional pull-request-before)
                (github-graphql-client-request
                 query token
                 :success #'on-success
                 :error #'on-error
                 :http-error #'on-http-error
                 :variables (if pull-request-before
                                (cons (cons "pullRequestBefore" pull-request-before)
                                      variables)
                              variables))))
      (fetch))))

(provide 'gh-viewer-graphql)
;;; gh-viewer-graphql.el ends here
