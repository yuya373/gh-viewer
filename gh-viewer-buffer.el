;;; gh-viewer-buffer.el ---                  -*- lexical-binding: t; -*-

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
(require 'emojify)
(require 'github-graphql-client)
(require 'gh-viewer-stringify-short)

(defmethod gh-viewer-buffer-name ((issue gh-viewer-issue) repo)
  (format "* %s - %s *"
          (gh-viewer-stringify-short repo)
          (gh-viewer-stringify-short issue)))

(defmethod gh-viewer-buffer-name ((_conn ggc:pull-request-connection) repo)
  (format "* %s - Pull Requests *"
          (gh-viewer-stringify-short repo)))

(defmethod gh-viewer-buffer-name ((_conn ggc:issue-comment-connection) pr repo)
  (format "* %s - %s - Comments *"
          (gh-viewer-stringify-short repo)
          (gh-viewer-stringify-short pr)))

(defmacro gh-viewer-with-buffer (buf &rest body)
  (declare (indent 2) (debug t))
  `(with-current-buffer ,buf
     (setq buffer-read-only nil)
     (erase-buffer)
     (goto-char (point-min))
     (emojify-mode t)
     ,@body
     (setq buffer-read-only t)
     (goto-char (point-min))))

(defmethod gh-viewer-buffer-display ((issue gh-viewer-issue) repo)
  (let ((buf (get-buffer-create (gh-viewer-buffer-name issue repo))))
    (gh-viewer-with-buffer buf
        (insert (gh-viewer-stringify issue)))
    (display-buffer buf)))

(defmethod gh-viewer-buffer-display ((conn ggc:issue-comment-connection) pr repo)
  (let ((buf (get-buffer-create (gh-viewer-buffer-name conn pr repo))))
    (gh-viewer-with-buffer buf
        (insert (gh-viewer-stringify conn)))
    (display-buffer buf)))

(defmethod gh-viewer-buffer-display ((conn ggc:pull-request-connection) repo)
  (if (not (< 0 (length (oref conn nodes))))
      (message "No Pull Request in %s" (oref repo name-with-owner))
    (let ((buf (get-buffer-create (gh-viewer-buffer-name conn repo))))
      (gh-viewer-buffer buf
                        (insert (gh-viewer-stringify conn repo)))
      (display-buffer buf))))

(provide 'gh-viewer-buffer)
;;; gh-viewer-buffer.el ends here
