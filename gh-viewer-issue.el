;;; gh-viewer-issue.el ---                           -*- lexical-binding: t; -*-

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
(require 'gh)
(require 'gh-viewer-util)
(require 'gh-viewer-repo)

(defcustom gh-viewer-issue-queries nil
  "Pre defined Queries.\n `((query-name . query-function))'\n query-function takes issue as argument, return non nil value if matches."
  :group 'gh-viewer)

(defface gh-viewer-issue-title-face
  '((t (:foreground "#FFA000"
                    :weight bold
                    :height 1.0
                    :underline t)))
  "Face used to pull-request title"
  :group 'gh-viewer)

(defface gh-viewer-issue-property-name-face
  '((t (:weight bold :underline t)))
  "Face used to issue property name"
  :group 'gh-viewer)

(defvar gh-viewer-issue-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'gh-viewer-issue-browse)
    keymap))

(defun gh-viewer-issue-browse ()
  (interactive)
  (let ((url (get-text-property 0 'url (thing-at-point 'line))))
    (if url
        (browse-url url)
      (error "Url not found"))))

(defmethod gh-viewer-issue--create-buffer ((repo gh-viewer-repo))
  (let* ((bufname (format "*%s/%s - Issues*"
                          (oref repo user)
                          (oref repo repo)))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min)))
    buf))

;;;###autoload
(defun gh-viewer-issue (&optional invalidate-cache)
  (interactive)
  (let* ((repo (gh-viewer-repo-select))
         (buf (gh-viewer-issue--create-buffer repo)))
    (cl-labels
        ((display (issues)
                  (gh-viewer-issue-render
                   buf
                   (cl-remove-if #'gh-viewer-pull-request-p issues))))
      (gh-viewer-repo-issues repo #'display invalidate-cache))))

(defun gh-viewer-issue-propertize-issue-property (prop-name)
  (propertize prop-name 'face 'gh-viewer-issue-property-name-face))

(defmethod gh-viewer-issue-user-name ((issue gh-issues-issue))
  (oref (oref issue user) login))

(defmethod gh-viewer-issue-issue-to-string ((issue gh-issues-issue))
  (let* ((title (format "#%s [%s] %s"
                        (oref issue number)
                        (oref issue state)
                        (propertize (oref issue title)
                                    'face 'gh-viewer-issue-title-face)))
         (open-at (format "%s: %s by %s"
                          (gh-viewer-issue-propertize-issue-property "opened at")
                          (gh-viewer-format-time-string (oref issue created-at))
                          (gh-viewer-issue-user-name issue)))
         (updated-at (format "%s: %s"
                             (gh-viewer-issue-propertize-issue-property "updated at")
                             (gh-viewer-format-time-string (oref issue updated-at))))
         (assignees (format "%s: %s"
                            (gh-viewer-issue-propertize-issue-property "assignees")
                            (mapconcat #'(lambda (user)
                                           (oref user login))
                                       (oref issue assignees)
                                       ", ")))
         (comments (format "%s comments" (oref issue comments))))
    (format "%s\n%s\n%s\n%s\n%s\n" title open-at updated-at assignees comments)))

(defmethod gh-viewer-issue-propertize-issue ((issue gh-issues-issue))
  (propertize
   (gh-viewer-issue-issue-to-string issue)
   'url (oref issue html-url)
   'keymap gh-viewer-issue-keymap))

(defun gh-viewer-issue-render (buf issues)
  (if (eq 0 (length issues))
      (error "No Issues")
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (mapc #'(lambda (issue)
                (insert (gh-viewer-issue-propertize-issue issue))
                (insert "\n"))
            issues)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)))

(defmethod gh-viewer-issue-assignees ((issue gh-issues-issue))
  (mapcar #'(lambda (user) (oref user login))
          (oref issue assignees)))

(defun gh-viewer-issue-assignee-equal-p (issue assignee)
  (cl-find-if #'(lambda (login) (string= assignee login))
              (gh-viewer-issue-assignees issue)))

(defun gh-viewer-issue-user-equal-p (issue user)
  (string= user (gh-viewer-issue-user-name issue)))

;;;###autoload
(defun gh-viewer-issue-filtered ()
  (interactive)
  (let* ((repo (gh-viewer-repo-select))
         (query-name (completing-read "Select Filter: " gh-viewer-issue-queries))
         (query (cdr (assoc query-name gh-viewer-issue-queries))))
    (cl-labels
        ((display (issues)
                  (gh-viewer-issue-render
                   (gh-viewer-issue--create-buffer repo)
                   (cl-remove-if-not query issues))))
      (gh-viewer-repo-issues repo #'display))))

(defmethod gh-viewer-issue-notification-message ((issue gh-issues-issue))
  (with-slots (number title) issue
    (format "#%s %s by %s"
            number title (gh-viewer-issue-user-name issue))))

(defmethod gh-viewer-issue-equal-p ((issue gh-issues-issue) other)
  (equal (gh-issues--issue-id issue)
         (gh-issues--issue-id other)))

(defmethod gh-viewer-issue-labels ((issue gh-issues-issue))
  (mapcar #'(lambda (label) (oref label name))
          (oref issue labels)))

(defmethod gh-viewer-issue-changes ((issue gh-issues-issue) old)
  (cl-labels
      ((build-props
        (issue)
        (with-slots
            (state title body user milestone comments updated-at) issue
          (list (cons "state" state)
                (cons "title" title)
                (cons "body" body)
                (cons "user" user)
                (cons "labels" (gh-viewer-issue-labels issue))
                (cons "assignees" (gh-viewer-issue-assignees issue))
                (cons "milestone" (oref milestone title))
                (cons "comments" comments)))))
    (let ((props (build-props issue))
          (old-props (build-props old)))
      (cl-remove-if #'null
                    (mapcar #'(lambda (prop)
                                (let ((new-value (cdr prop))
                                      (old-value (cdr (cl-assoc (car prop) old-props
                                                                :test #'string=))))
                                  (unless (equal new-value old-value)
                                    (cons (car prop) (cons old-value new-value)))))
                            props)))))

(provide 'gh-viewer-issue)
;;; gh-viewer-issue.el ends here
