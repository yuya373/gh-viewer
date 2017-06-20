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
   (issues-last-fetched :initform 0)
   (issues :initarg :issues :initform nil)
   (old-issues :initform nil)
   (watch-issues-timer :initform nil)
   (notifier :initarg :notifier :initform nil)
   ))

(defmethod gh-viewer-repo-equalp ((repo gh-viewer-repo) other)
  (and (string= (oref repo user) (oref other user))
       (string= (oref repo repo) (oref other repo))))

;;;###autoload
(cl-defun gh-viewer-add-repo (user repo &key
                                   (notifier #'gh-viewer-repo-default-notifier))
  (let ((repo (make-instance 'gh-viewer-repo
                             :user user :repo repo
                             :notifier notifier
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
      ((callback (issues)
                 (oset repo issues issues)
                 (oset repo issues-last-fetched (time-to-seconds))
                 (funcall cb issues))
       (cache-invalid-p ()
                        (or invalidate-cache
                            (< 120 (- (time-to-seconds) (oref repo issues-last-fetched))))))
    (if (cache-invalid-p)
        (progn
          (oset repo old-issues (oref repo issues))
          (gh-url-add-response-callback
           (gh-issues-issue-list
            (gh-issues-api :sync nil :cache nil)
            (oref repo user) (oref repo repo))
           #'callback))
      (funcall cb (oref repo issues)))))

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

(defun gh-viewer-repo-default-notifier (repo issues)
  "ISSUES: '((new-issue . old-issue)... (new-issue . nil))."
  (mapc #'(lambda (issue)
            (let ((notification (gh-viewer-notification-create (car issue) (cdr issue) repo)))
              (when notification
                (gh-viewer-notification-notify notification))))
        issues))

(defmethod gh-viewer-repo--watch-issues ((repo gh-viewer-repo))
  (cl-labels
      ((notify (_issues)
               (let* ((old-issues (oref repo old-issues))
                      (new-issues (oref repo issues))
                      (issues (cl-loop for new-issue in new-issues
                                       collect (cons new-issue
                                                     (cl-find-if #'(lambda (old-issue)
                                                                     (gh-viewer-issue-equal-p new-issue old-issue))
                                                                 old-issues))))
                      (notifier (oref repo notifier)))
                 (when (functionp notifier)
                   (funcall notifier repo issues)))))
    (gh-viewer-repo-issues repo #'notify t)))

(defmethod gh-viewer-repo-watch-issues ((repo gh-viewer-repo))
  (with-slots (watch-issues-timer) repo
    (when (timerp watch-issues-timer) (cancel-timer watch-issues-timer))
    (cl-labels
        ((set-timer (issue)
                    (setq watch-issues-timer
                          (run-at-time t gh-viewer-repo-watch-idle-time
                                       #'(lambda () (gh-viewer-repo--watch-issues repo))))))
      (gh-viewer-repo-issues repo #'set-timer t)
      (message "Started timer: %s" (gh-viewer-repo-to-string repo)))))

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
