;;; gh-viewer-summarize.el ---                 -*- lexical-binding: t; -*-

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
(require 'gh-viewer-finder)

(defface gh-viewer-pull-request-summary-header
  '((t (:foreground "#FFA000"
                    :weight bold
                    :height 1.0
                    :underline t)))
  "Face used to Pull Request summary header"
  :group 'gh-viewer)

(defface gh-viewer-pull-request-summary-info
  '((t (:weight :height 0.9)))
  "Face used to Pull Request summary info"
  :group 'gh-viewer)

(defmethod gh-viewer-summarize ((pr ggc:pull-request) repo)
  (cl-labels
      ((open ()
             (interactive)
             (gh-viewer-buffer-display pr repo)))
    (with-slots (title number state comments published-at author) pr
      (let ((header (format "#%s [%s] %s" number state title))
            (info (format "created by %s at %s"
                          (gh-viewer-stringify author) published-at))
            (comments-count (format "%s comments"
                                    (oref comments total-count))))
        (propertize
         (format "%s\n%s\n%s"
                 (propertize header
                             'face
                             'gh-viewer-pull-request-summary-header)
                 (propertize info
                             'face
                             'gh-viewer-pull-request-summary-info)
                 comments-count)
         'keymap (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "RET") #'open)
                   map))))))

(defmethod gh-viewer-summarize ((conn ggc:pull-request-connection) repo)
  (mapconcat #'(lambda (e) (gh-viewer-summarize e repo)) (oref conn nodes) "\n\n"))

(defmethod gh-viewer-summarize ((conn ggc:issue-comment-connection))
  (mapconcat #'gh-viewer-stringify (last (oref conn nodes) 5) "\n\n"))

(provide 'gh-viewer-summarize)
;;; gh-viewer-summarize.el ends here
