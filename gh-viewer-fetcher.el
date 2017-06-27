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

(defvar gh-viewer-directory
  (or (and load-file-name (file-name-directory load-file-name))
      (and (locate-library "gh-viewer") (file-name-directory (locate-library "gh-viewer")))))

(defun gh-viewer-query-location (name)
  (expand-file-name (concat gh-viewer-directory (format "graphql/%s.graphql" name))))

(defcustom gh-viewer-repository-query-location
  (gh-viewer-query-location "repository")
  "Query for fetch repository."
  :group 'gh-viewer)

(defcustom gh-viewer-pull-request-comments-query-location
  (gh-viewer-query-location "pull-request-comments")
  "Query for fetch Pull Request Comments."
  :group 'gh-viewer)

(defcustom gh-viewer-issue-comments-query-location
  (gh-viewer-query-location "issue-comments")
  "Query for fetch Issue Comments."
  :group 'gh-viewer)

(defcustom gh-viewer-repository-fragments-location
  (gh-viewer-query-location "repository-fragments")
  "Shared Query for graphql."
  :group 'gh-viewer)

(defcustom gh-viewer-issue-comment-connection-fragment-location
  (gh-viewer-query-location "issue-comment-connection-fragment")
  "Fragment of IssueCommentConnection."
  :group 'gh-viewer)

(defvar gh-viewer-before-merge-hook nil
  "Hook runs before Repository merged.\n`(lambda (new-repository old-repository) ...)'.")

(defmethod gh-viewer-notifier-template ((_ ggc:pull-request))
  "Pull Request #%s has %s new Comment%s")

(defmethod gh-viewer-notifier-template ((_ ggc:issue))
  "Issue #%s has %s new Comment%s")

(defmethod gh-viewer-notifier ((new gh-viewer-issue) old repo)
  (when old
    (let* ((new-comment-count (oref (oref new comments) total-count))
           (old-comment-count (oref (oref old comments) total-count))
           (comment-count-diff (- new-comment-count old-comment-count)))
      (if (< 0 comment-count-diff)
          (alert (format (gh-viewer-notifier-template new)
                         (oref new number)
                         comment-count-diff
                         (or (and (< 1 comment-count-diff) "s")
                             ""))
                 :title (gh-viewer-stringify-short repo))))))

(defmethod gh-viewer-notifier-template ((_conn ggc:pull-request-connection))
  "has %s new Pull Request%s")

(defmethod gh-viewer-notifier-templage ((_conn ggc:issue-connection))
  "has %s new Issue%s")

(defmethod gh-viewer-notifier ((new ggc:connection) old repo)
  (let* ((new-total-count (oref new total-count))
         (old-total-count (oref old total-count))
         (total-count-diff (- new-total-count old-total-count)))
    (if (< 0 total-count-diff)
        (alert (format (gh-viewer-notifier-template new)
                       total-count-diff
                       (or (and (< 1 total-count-diff) "s")
                           ""))
               :title (gh-viewer-stringify-short repo)))
    (mapc #'(lambda (new-node)
              (gh-viewer-notifier new-node
                                  (cl-find-if #'(lambda (old-node) (string= (oref old-node id)
                                                                            (oref new-node id)))
                                              (oref old nodes))
                                  repo))
          (oref new nodes))))

(defmethod gh-viewer-notifier ((new-repository ggc:repository) old-repository)
  (let ((new-pull-requests (oref new-repository pull-requests))
        (old-pull-requests (oref old-repository pull-requests))
        (new-issues (oref new-repository issues))
        (old-issues (oref old-repository issues)))
    (gh-viewer-notifier new-pull-requests old-pull-requests new-repository)
    (gh-viewer-notifier new-issues old-issues new-repository)))

(defun gh-viewer-query (location)
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert-file-contents-literally location nil)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gh-viewer-repository-query ()
  (format "%s\n%s\n%s"
          (gh-viewer-query gh-viewer-repository-query-location)
          (gh-viewer-query gh-viewer-repository-fragments-location)
          (gh-viewer-query gh-viewer-issue-comment-connection-fragment-location)))

(cl-defun gh-viewer-fetch-repository (owner name &key
                                            (success nil)
                                            (error nil)
                                            (http-error nil)
                                            (pull-request-states '("OPEN"))
                                            (pull-request-order-field "CREATED_AT")
                                            (pull-request-order-direction "DESC")
                                            (issue-states '("OPEN"))
                                            (issue-order-field "CREATED_AT")
                                            (issue-order-direction "DESC")
                                            )
  (let ((query (gh-viewer-repository-query))
        (token gh-viewer-token)
        (variables (list (cons "owner" owner)
                         (cons "name" name)
                         (cons "pullRequestStates" pull-request-states)
                         (cons "pullRequestOrderField" pull-request-order-field)
                         (cons "pullRequestOrderDirection" pull-request-order-direction)
                         (cons "issueStates" issue-states)
                         (cons "issueOrderField" issue-order-field)
                         (cons "issueOrderDirection" issue-order-direction)
                         )))
    (cl-labels
        ((paginate-or-finish (repo)
                             (let ((has-more-pull-requests (gh-viewer-has-more (oref repo pull-requests)
                                                                               pull-request-order-direction))
                                   (has-more-issues (gh-viewer-has-more (oref repo issues)
                                                                        issue-order-direction)))
                               (if (or has-more-pull-requests has-more-issues)
                                   (fetch repo
                                          (and has-more-pull-requests
                                               (gh-viewer-next-cursor (oref repo pull-requests)
                                                                      pull-request-order-direction))
                                          (and has-more-issues
                                               (gh-viewer-next-cursor (oref repo issues)
                                                                      issue-order-direction))
                                          (not has-more-pull-requests)
                                          (not has-more-issues))
                                 (when (functionp success)
                                   (funcall success repo)))))
         (on-success (data base)
                     (let* ((repo (gh-viewer-graphql-initialize-repository
                                   (plist-get (plist-get data :data) :repository))))

                       (when base
                         (gh-viewer-merge base repo t)
                         (setq repo base))

                       (paginate-or-finish repo)))
         (on-error (data)
                   (message (format "%s" (plist-get data :errors)))
                   (when (functionp error)
                     (funcall (plist-get data :errors))))
         (on-http-error (&key error-thrown &allow-other-keys)
                        (message (format "%s" error-thrown))
                        (when (functionp http-error)
                          (funcall http-error error-thrown)))
         (fetch (&optional base pull-request-before issue-before ignore-pull-requests ignore-issues)
                (github-graphql-client-request
                 query token
                 :success #'(lambda (data) (on-success data base))
                 :error #'on-error
                 :http-error #'on-http-error
                 :variables (cons (cons "pullRequestBefore" pull-request-before)
                                  (cons (cons "issueBefore" issue-before)
                                        (cons (cons "ignorePullRequests" (or ignore-pull-requests
                                                                             :json-false))
                                              (cons (cons "ignoreIssues" (or ignore-issues
                                                                             :json-false))
                                                    variables)))))))
      (fetch))))

(cl-defmethod gh-viewer-fetch ((repo gh-viewer-repo) &optional success error)
  (cl-labels
      ((on-success (new-repository)
                   (if (oref repo repository)
                       (progn
                         (run-hook-with-args 'gh-viewer-before-merge-hook new-repository (oref repo repository))
                         (gh-viewer-merge (oref repo repository) new-repository))
                     (oset repo repository new-repository))

                   (oset repo last-fetched (time-to-seconds))
                   (when (functionp success)
                     (funcall success (oref repo repository))))
       (on-error (errors) (message "Errors: %s" errors)))
    (message "Fetching %s ..." (gh-viewer-stringify repo))
    (gh-viewer-fetch-repository (oref repo owner) (oref repo name)
                                :success #'on-success :error error)))

(defmethod gh-viewer-comments-query ((_pr ggc:pull-request))
  (format "%s\n%s"
          (gh-viewer-query gh-viewer-pull-request-comments-query-location)
          (gh-viewer-query gh-viewer-issue-comment-connection-fragment-location)))

(defmethod gh-viewer-comments-query ((_issue ggc:issue))
  (format "%s\n%s"
          (gh-viewer-query gh-viewer-issue-comments-query-location)
          (gh-viewer-query gh-viewer-issue-comment-connection-fragment-location)))

(defmethod gh-viewer-number-param ((pr ggc:pull-request))
  (cons "pullRequestNumber" (oref pr number)))

(defmethod gh-viewer-number-param ((issue ggc:issue))
  (cons "issueNumber" (oref issue number)))

(defmethod gh-viewer-extract-comments ((_pr ggc:pull-request) data)
  (plist-get
   (plist-get
    (plist-get
     (plist-get data :data)
     :repository)
    :pullRequest)
   :comments))

(defmethod gh-viewer-extract-comments ((_issue ggc:issue) data)
  (plist-get
   (plist-get
    (plist-get
     (plist-get data :data)
     :repository)
    :issue)
   :comments))

(cl-defmethod gh-viewer-fetch ((conn ggc:issue-comment-connection) issue-or-pr repository
                               &optional success error)
  (let* ((next-cursor (gh-viewer-next-cursor conn "ASC"))
         (owner (oref (oref repository owner) login))
         (name (oref repository name))
         (query (gh-viewer-comments-query issue-or-pr))
         (variables (list (cons "owner" owner)
                          (cons "name" name)
                          (gh-viewer-number-param issue-or-pr))))
    (cl-labels
        ((on-success (data base)
                     (let ((new-conn (gh-viewer-graphql-initialize-issue-comment-connection
                                      (gh-viewer-extract-comments issue-or-pr data))))
                       (gh-viewer-merge new-conn base)
                       (oset issue-or-pr comments new-conn)
                       (if (gh-viewer-has-more (oref issue-or-pr comments) "ASC")
                           (fetch (oref issue-or-pr comments)
                                  (gh-viewer-next-cursor (oref issue-or-pr comments)
                                                         "ASC"))
                         (when (functionp success)
                           (funcall success)))))
         (on-error (data)
                   (message "Error: %s" data)
                   (when (functionp error)
                     (funcall error data)))
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
