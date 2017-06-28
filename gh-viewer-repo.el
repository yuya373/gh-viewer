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
(require 'github-graphql-client)

(defvar gh-viewer-repos nil)
(defvar gh-viewer-load-path load-path)

(defcustom gh-viewer-repo-watch-idle-time 60
  "Interval fetching issues from Github. In second."
  :group 'gh-viewer)

(defclass gh-viewer-repo ()
  ((owner :initarg :owner :type string)
   (name :initarg :name :type string)
   (repository :initarg :repository :initform nil :type (or null ggc:repository))
   (last-fetched :initarg :last-fetched :initform 0)
   (watch-timer :initform nil)))

(defmethod gh-viewer-repo-equalp ((repo gh-viewer-repo) other)
  (and (string= (oref repo owner) (oref other owner))
       (string= (oref repo name) (oref other name))))

;;;###autoload
(cl-defun gh-viewer-add-repo (owner name &key
                                    (notifier #'gh-viewer-repo-default-notifier))
  (let ((repo (make-instance 'gh-viewer-repo
                             :owner owner :name name
                             ;; :notifier notifier
                             )))
    (setq gh-viewer-repos
          (cl-remove-if #'(lambda (e) (gh-viewer-repo-equalp repo e))
                        gh-viewer-repos))
    (push repo gh-viewer-repos)))

(defmethod gh-viewer-repo-to-string ((repo gh-viewer-repo))
  (format "%s/%s" (oref repo owner) (oref repo name)))

(defun gh-viewer-repo-select ()
  (let* ((alist (mapcar #'(lambda (e) (cons (gh-viewer-stringify e) e))
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
                          (oset repo issues-last-fetched (time-to-seconds))
                          (funcall cb (oref repo issues)))))
       (cache-invalid-p ()
                        (or invalidate-cache
                            (< 120 (- (time-to-seconds) (oref repo issues-last-fetched))))))
    (if (cache-invalid-p)
        (progn
          (oset repo old-issues (oref repo issues))
          (fetch-issues))
      (funcall cb (oref repo issues)))))

;;;###autoload
(defun gh-viewer-repo-start-watch ()
  (interactive)
  (mapc #'gh-viewer-repo-watch gh-viewer-repos))

(defun gh-viewer-repo-stop-watch ()
  (interactive)
  (mapc #'(lambda (repo)
            (when (timerp (oref repo watch-timer))
              (cancel-timer (oref repo watch-timer))
              (oset repo watch-timer nil)
              (message "Stopped timer: %s" (gh-viewer-stringify repo))))
        gh-viewer-repos))

(defun gh-viewer-repo-default-notifier (repo issues)
  "ISSUES: '((new-issue . old-issue)... (new-issue . nil))."
  (mapc #'(lambda (issue)
            (let ((notification (gh-viewer-notification-create (car issue) (cdr issue) repo)))
              (when notification
                (gh-viewer-notification-notify notification))))
        issues))

(defmethod gh-viewer-repo-watch ((repo gh-viewer-repo))
  (when (timerp (oref repo watch-timer)) (cancel-timer (oref repo watch-timer)))
  (cl-labels
      ((set-timer (_)
                  (oset repo watch-timer
                        (run-at-time t gh-viewer-repo-watch-idle-time
                                     #'(lambda () (gh-viewer-fetch repo))))))
    (gh-viewer-fetch repo #'set-timer)
    (message "Started timer: %s" (gh-viewer-stringify repo))))

(defmethod gh-viewer-repo-notify-new-pull-request-p ((repo gh-viewer-repo))
  (oref repo notify-new-pull-request))

(defmethod gh-viewer-repo-notify-new-issue-p ((repo gh-viewer-repo))
  (oref repo notify-new-issue))

(defmethod gh-viewer-repo-notify-updated-pull-request-p ((repo gh-viewer-repo))
  (oref repo notify-updated-pull-request))

(defmethod gh-viewer-repo-notify-updated-issue-p ((repo gh-viewer-repo))
  (oref repo notify-updated-issue))

(defmethod gh-viewer-use-cache-p ((repo gh-viewer-repo))
  (and (oref repo repository)
       (< (- (time-to-seconds) (oref repo last-fetched)) (* 60 5))))

(provide 'gh-viewer-repo)
;;; gh-viewer-repo.el ends here
