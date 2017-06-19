;;; gh-viewer-stringify.el ---               -*- lexical-binding: t; -*-

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

(defface gh-viewer-pull-request-title
  '((t (:foreground "#FFA000"
                    :weight bold
                    :height 1.2
                    :underline t)))
  "Face used to Pull Request title"
  :group 'gh-viewer)

(defface gh-viewer-pull-request-section-title
  '((t (:weight bold :height 1.2 :underline t)))
  "Face used to Pull Request section title"
  :group 'gh-viewer)

(defface gh-viewer-pull-request-comment-separator
  '((t (:underline t)))
  "Face used to Pull Request comment separator"
  :group 'gh-viewer)

(defmethod gh-viewer-blank? ((str string))
  (not (< 0 (length str))))

(defmethod gh-viewer-stringify ((user ggc:user))
  (with-slots (login) user
    (format "%s" login)))

(defmethod gh-viewer-stringify ((actor ggc:actor))
  (with-slots (login) actor
    (format "%s" login)))

(defmethod gh-viewer-stringify ((conn ggc:user-connection))
  (with-slots (nodes) conn
    (mapconcat #'gh-viewer-stringify nodes ", ")))

(defmethod gh-viewer-stringify ((label ggc:label))
  (with-slots (name color) label
    (propertize (format "#%s" name)
                'face (list :foreground color))))

(defmethod gh-viewer-stringify ((conn ggc:label-connection))
  (with-slots (nodes) conn
    (mapconcat #'gh-viewer-stringify nodes ", ")))

(defmethod gh-viewer-stringify ((comment ggc:issue-comment))
  (with-slots (author published-at body) comment
    (format "%s  %s\n%s"
            (gh-viewer-stringify author)
            published-at
            body)))

(defmethod gh-viewer-stringify ((conn ggc:issue-comment-connection))
  (with-slots (nodes) conn
    (mapconcat #'gh-viewer-stringify nodes
               (format "\n\n%s\n\n" (propertize "                        "
                                                'face 'gh-viewer-pull-request-comment-separator)))))

(defmethod gh-viewer-format-section-title ((title string))
  (propertize title 'face 'gh-viewer-pull-request-section-title))

(defmethod gh-viewer-stringify ((pr ggc:pull-request))
  (let ((assignees (let ((str (gh-viewer-stringify (oref pr assignees))))
                     (or (and (gh-viewer-blank? str) "")
                         (format "%s %s\n"
                                 (gh-viewer-format-section-title "Assignees:")
                                 str))))
        (labels (let ((str (gh-viewer-stringify (oref pr labels))))
                  (or (and (gh-viewer-blank? str) "")
                      (format "%s %s\n"
                              (gh-viewer-format-section-title "Labels:")
                              str))))
        (comments (let ((str (gh-viewer-stringify (oref pr comments))))
                    (or (and (gh-viewer-blank? str) "")
                        (format "%s\n%s\n"
                                (gh-viewer-format-section-title "Comments:")
                                str))))

        (title (propertize (format "#%s %s [%s]\n" (oref pr number) (oref pr title) (oref pr state))
                           'face 'gh-viewer-pull-request-title))
        ;; (review-requests (gh-viewer-stringify
        ;;                   (oref pr review-requests)))
        ;; (reviews (gh-viewer-stringify (oref pr reviews)))
        (body (format "%s\n\n" (oref pr body)))
        ;; (head-ref (oref pr head-ref-name))
        ;; (base-ref (oref pr base-ref-name))
        )
    (format "%s%s%s\n%s%s%s"
            title labels assignees labels body comments)))

(defmethod gh-viewer-stringify ((conn ggc:pull-request-connection))
  (with-slots (nodes) conn
    (mapconcat #'gh-viewer-stringify nodes "\n")))



(provide 'gh-viewer-stringify)
;;; gh-viewer-stringify.el ends here
