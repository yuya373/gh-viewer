;;; gh-viewer-pull-request.el ---                    -*- lexical-binding: t; -*-

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
(require 'gh-viewer-repo)

(defface gh-viewer-pull-request-title-face
  '((t (:foreground "#FFA000"
                    :weight bold
                    :height 1.0
                    :underline t)))
  "Face used to pull-request title"
  :group 'gh-viewer)

(defvar gh-viewer-pull-request-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'gh-viewer-pull-request-browse)
    keymap))

(defun gh-viewer-pull-request-browse ()
  (interactive)
  (let ((url (get-text-property 0 'url (thing-at-point 'line))))
    (if url
        (browse-url url)
      (error "Url not found"))))

(defmethod gh-viewer-pull-request--create-buffer ((repo gh-viewer-repo))
  (let* ((bufname (format "*%s/%s - Pull Requests*" (oref repo user) (oref repo repo)))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min)))
    buf))

(defmethod gh-viewer-pull-request-issues-to-string ((pr gh-issues-issue))
  (let* ((title (format "[%s]\t%s"
                        (oref pr state)
                        (propertize (oref pr title)
                                    'face 'gh-viewer-pull-request-title-face)))
         (info (format "#%s\topend at %s"
                       (oref pr number)
                       (oref pr created-at)))
         (assignees (format "assignees: %s"
                            (mapconcat #'(lambda (user)
                                           (number-to-string
                                            (oref user id)))
                                       (oref pr assignees)
                                       ", "))))
    (format "%s\n%s\n%s\n" title info assignees)))

;;;###autoload
(defun gh-viewer-pull-request ()
  (interactive)
  (let* ((repo (gh-viewer-repo-select))
         (api (gh-issues-api :sync nil :cache nil))
         (issues (gh-issues-issue-list api (oref repo user) (oref repo repo)))
         (pulls (cl-remove-if
                 #'(lambda (issue)
                     (not (gh-issues-pull-request-p (oref issue pull-request))))
                 (oref issues data)))
         (buf (gh-viewer-pull-request--create-buffer repo)))
    (if (eq 0 (length pulls))
        (error "No Pull Requests")
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (mapc #'(lambda (pr)
                  (insert (propertize
                           (gh-viewer-pull-request-issues-to-string pr)
                           'url (oref (oref pr pull-request) html-url)
                           'keymap gh-viewer-pull-request-keymap))
                  (insert "\n"))
              pulls)
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (display-buffer buf))))


(provide 'gh-viewer-pull-request)
;;; gh-viewer-pull-request.el ends here
