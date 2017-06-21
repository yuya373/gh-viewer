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
(require 'eieio)
(require 'gh)
(require 'gh-viewer-repo)
(require 'gh-viewer-issue)

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
    (and (slot-boundp pull-request 'html-url)
         (not (string= "unbound" (oref pull-request html-url))))))

;;;###autoload
(defun gh-viewer-pull-request (&optional invalidate-cache)
  (interactive)
  (let* ((repo (gh-viewer-repo-select)))
    (cl-labels
        ((display (issues)
                  (gh-viewer-pull-request-render
                   repo
                   (gh-viewer-pull-request-remove-issues issues))))
      (gh-viewer-repo-issues repo #'display invalidate-cache))))

(defun gh-viewer-pull-request-render (repo pulls)
  (if (eq 0 (length pulls))
      (error "No Pull Requests")
    (let ((buf (gh-viewer-pull-request--create-buffer repo)))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (mapc #'(lambda (pr)
                  (insert (gh-viewer-issue-propertize-issue pr))
                  (insert (gh-viewer-issue-view-comments-button pr repo))
                  (insert "\n"))
              pulls)
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (display-buffer buf))))

(defun gh-viewer-pull-request-remove-issues (issues)
  (cl-remove-if #'(lambda (issue)
                    (not (gh-viewer-pull-request-p issue)))
                issues))

(defun gh-viewer-pull-request-filter-by-assignee ()
  (interactive)
  (let* ((repo (gh-viewer-repo-select))
         (pulls (gh-viewer-pull-request-remove-issues (oref repo issues)))
         (assignee (read-from-minibuffer "Input Assignee: ")))
    (gh-viewer-pull-request-render
     repo
     (gh-viewer-issue--filter-by-assignee pulls assignee))))

;;;###autoload
(defun gh-viewer-pull-request-filtered ()
  (interactive)
  (let* ((repo (gh-viewer-repo-select))
         (query-name (completing-read "Select Filter: " gh-viewer-issue-queries))
         (filter (cdr (assoc query-name gh-viewer-issue-queries))))
    (cl-labels
        ((display (repository)

                  (let ((pull-request (gh-viewer-select
                                       (gh-viewer-filter-pull-request
                                        (oref repository pull-requests)
                                        filter))))
                    (if (gh-viewer-has-more (oref pull-request comments) "ASC")
                        (progn
                          (message "Loading Comments...")
                          (gh-viewer-fetch (oref pull-request comments) pull-request repository
                                           #'(lambda () (gh-viewer-buffer-display pull-request repository))))
                      (gh-viewer-buffer-display pull-request repository)))))
      (if (gh-viewer-use-cache-p repo)
         (display (oref repo repository))
        (gh-viewer-fetch repo #'display)))))

(provide 'gh-viewer-pull-request)
;;; gh-viewer-pull-request.el ends here
