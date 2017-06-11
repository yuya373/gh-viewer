;;; gh-viewer-repo.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <yuya373@yuya373>
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
(require 'gh)
(require 'async)

(defvar gh-viewer-repos nil)
(defvar gh-viewer-load-path load-path)

(defcustom gh-viewer-repo-watch-idle-time 60
  "Interval fetching issues from Github. In second."
  :group 'gh-viewer)

(defclass gh-viewer-repo ()
  ((user :initarg :user :type string)
   (repo :initarg :repo :type string)
   (users :initarg :users :initform nil)
   (issues :initarg :issues :initform nil)
   (old-issues :initform nil)
   (watch-issues-timer :initform nil)
   (notify-new-issue :initarg :notify-new-issue :initform nil)
   (notify-new-pull-request :initarg :notify-new-pull-request :initform nil)
   (notify-updated-issue :initarg :notify-updated-issue :initform nil)
   (notify-updated-pull-request :initarg :notify-updated-pull-request :initform nil)))

(defmethod gh-viewer-repo-equalp ((repo gh-viewer-repo) other)
  (and (string= (oref repo user) (oref other user))
       (string= (oref repo repo) (oref other repo))))

;;;###autoload
(cl-defun gh-viewer-add-repo (user repo &key
                                   (notify-new-issue nil)
                                   (notify-new-pull-request nil)
                                   (notify-updated-issue nil)
                                   (notify-updated-pull-request nil))
  (let ((repo (make-instance 'gh-viewer-repo
                             :user user :repo repo
                             :notify-new-issue notify-new-issue
                             :notify-new-pull-request notify-new-pull-request
                             :notify-updated-issue notify-updated-issue
                             :notify-updated-pull-request notify-updated-pull-request
                             )))
    (setq gh-viewer-repos
          (cl-remove-if #'(lambda (e) (gh-viewer-repo-equalp repo e))
                        gh-viewer-repos))
    (push repo gh-viewer-repos)))

(defmethod gh-viewer-repo-to-string ((repo gh-viewer-repo))
  (format "%s/%s" (oref repo user) (oref repo repo)))

(defun gh-viewer-repo-select ()
  (let* ((alist (mapcar #'(lambda (e) (cons (gh-viewer-repo-to-string e) e))
                        gh-viewer-repos))
         (input (completing-read "Select Repo: " alist))
         (selected (cdr (cl-assoc input alist :test #'string=))))
    selected))

(defmethod gh-viewer-repo-issues ((repo gh-viewer-repo) cb &optional invalidate-cache)
  (cl-labels
      ((fetch-issues ()
                     (async-start
                      `(lambda ()
                         ,(async-inject-variables "gh-viewer-load-path")
                         (dolist (path gh-viewer-load-path)
                           (add-to-list 'load-path path))
                         (require 'gh)
                         (gh-issues-issue-list
                          (gh-issues-api :sync nil :cache nil)
                          ,(oref repo user) ,(oref repo repo))
                         )
                      #'(lambda (response)
                          (oset repo issues (oref response data))
                          (funcall cb (oref repo issues))))))
    (let ((cache (oref repo issues)))
      (if (or invalidate-cache (< (length cache) 1))
          (progn
            (oset repo old-issues (oref repo issues))
            (fetch-issues))
        (funcall cb (oref repo issues))))))

(defmethod gh-viewer-repo-new-issues ((repo gh-viewer-repo))
  (let ((old-issues (oref repo old-issues))
        (new-issues (oref repo issues)))
    (if (< 0 (length old-issues))
        (cl-remove-if #'(lambda (e) (cl-find-if #'(lambda (f) (gh-viewer-issue-equal-p e f))
                                                old-issues))
                      new-issues))))

(defmethod gh-viewer-repo-updated-issues ((repo gh-viewer-repo))
  (let ((old-issues (oref repo old-issues))
        (new-issues (oref repo issues)))
    (if (< 0 (length old-issues))
        (cl-remove-if #'(lambda (new-issue)
                          (let ((old-issue (cl-find-if #'(lambda (e)
                                                           (gh-viewer-issue-equal-p e new-issue))
                                                       old-issues)))
                            (and old-issue
                                 (not (string< (gh-viewer-convert-unix-time-string
                                                (oref old-issue updated-at))
                                               (gh-viewer-convert-unix-time-string
                                                (oref new-issue updated-at)))))))
                      new-issues))))

;;;###autoload
(defun gh-viewer-repo-start-watch-issues ()
  (interactive)
  (mapc #'gh-viewer-repo-watch-issues
        gh-viewer-repos))

(defun gh-viewer-repo-stop-watch-issues ()
  (interactive)
  (mapc #'(lambda (repo) (with-slots ((timer watch-issues-timer)) repo
                           (when (timerp timer)
                             (cancel-timer timer)
                             (setq timer nil)
                             (message "Stopped timer: %s" (gh-viewer-repo-to-string repo)))))
        gh-viewer-repos))

(defmethod gh-viewer-repo--watch-issues ((repo gh-viewer-repo))
  (cl-labels
      ((notify (_issues)
               (let ((new-issues (gh-viewer-repo-new-issues repo))
                     (updated-issues (gh-viewer-repo-updated-issues repo)))
                 (mapc #'(lambda (e) (gh-viewer-issue-notify-new-issue e repo))
                       new-issues)
                 (mapc #'(lambda (e) (gh-viewer-issue-notify-updated-issue e repo))
                       updated-issues))))
    (gh-viewer-repo-issues repo #'notify t)))

(defmethod gh-viewer-repo-watch-issues ((repo gh-viewer-repo))
  (with-slots (watch-issues-timer) repo
    (when (timerp watch-issues-timer) (cancel-timer watch-issues-timer))
    (setq watch-issues-timer
          (run-with-idle-timer gh-viewer-repo-watch-idle-time t
                               #'(lambda () (gh-viewer-repo--watch-issues repo))))
    (message "Started timer: %s" (gh-viewer-repo-to-string repo))))

(defmethod gh-viewer-repo-notify-new-pull-request-p ((repo gh-viewer-repo))
  (oref repo notify-new-pull-request))

(defmethod gh-viewer-repo-notify-new-issue-p ((repo gh-viewer-repo))
  (oref repo notify-new-issue))

(defmethod gh-viewer-repo-notify-updated-pull-request-p ((repo gh-viewer-repo))
  (oref repo notify-updated-pull-request))

(defmethod gh-viewer-repo-notify-updated-issue-p ((repo gh-viewer-repo))
  (oref repo notify-updated-issue))

(provide 'gh-viewer-repo)
;;; gh-viewer-repo.el ends here
