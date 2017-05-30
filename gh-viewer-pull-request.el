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

(defmethod gh-viewer-pull-request--create-buffer ((repo gh-viewer-repo))
  (let* ((bufname (format "*%s/%s - Pull Requests*" (oref repo user) (oref repo repo)))
         (buf (get-buffer-create bufname)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min)))
    buf))

(defmethod gh-viewer-pull-request-p ((issue gh-issues-issue))
  (with-slots (pull-request) issue
    (message "%s" pull-request)
    (slot-boundp pull-request 'html-url)))

;;;###autoload
(defun gh-viewer-pull-request (&optional invalidate-cache)
  (interactive)
  (let* ((repo (gh-viewer-repo-select))
         (cache (oref repo issues))
         (issues (if (or invalidate-cache
                         (< (length cache) 1))
                     (oref (gh-issues-issue-list
                            (gh-issues-api :sync nil :cache nil)
                            (oref repo user) (oref repo repo))
                           data)
                   cache))
         (pulls (cl-remove-if
                 #'(lambda (issue)
                     (not (gh-viewer-pull-request-p issue)))
                 issues))
         (buf (gh-viewer-pull-request--create-buffer repo)))
    (oset repo issues issues)
    (gh-viewer-pull-request-render buf pulls)))

(defun gh-viewer-pull-request-render (buf pulls)
  (if (eq 0 (length pulls))
      (error "No Pull Requests")
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (mapc #'(lambda (pr)
                (insert (gh-viewer-issue-propertize-issue pr))
                (insert "\n"))
            pulls)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)))


(provide 'gh-viewer-pull-request)
;;; gh-viewer-pull-request.el ends here
