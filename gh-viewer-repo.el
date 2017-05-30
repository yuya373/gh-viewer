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

(defvar gh-viewer-repos nil)
(defclass gh-viewer-repo ()
  ((user :initarg :user :type string)
   (repo :initarg :repo :type string)
   (users :initarg :users :initform nil)
   (issues :initarg :issues :initform nil)))

(defmethod gh-viewer-repo-equalp ((repo gh-viewer-repo) other)
  (and (string= (oref repo user) (oref other user))
       (string= (oref repo repo) (oref other repo))))

;;;###autoload
(defun gh-viewer-add-repo (user repo)
  (let ((repo (make-instance 'gh-viewer-repo :user user :repo repo)))
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

(provide 'gh-viewer-repo)
;;; gh-viewer-repo.el ends here
