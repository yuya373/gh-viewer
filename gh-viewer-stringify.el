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
(require 'parse-time)
(require 'github-graphql-client)

(defface gh-viewer-pull-request-title
  '((t (:foreground "#FFA000"
                    :weight bold
                    :height 1.2
                    :underline t)))
  "Face used to Pull Request title"
  :group 'gh-viewer)

(defface gh-viewer-pull-request-section-title
  '((t (:foreground "#859900"
                    :weight bold
                    :height 1.0)))
  "Face used to Pull Request section title"
  :group 'gh-viewer)

(defface gh-viewer-pull-request-comment-separator
  '((t (:underline t)))
  "Face used to Pull Request comment separator"
  :group 'gh-viewer)

(defface gh-viewer-issue-comment-header-login
  '((t (:foreground "#2aa198"
                    :underline t
                    :weight bold)))
  "Face used to Issue Comment header login"
  :group 'gh-viewer)

(defface gh-viewer-issue-comment-header-datetime
  '((t (:underline t)))
  "Face used to Issue Comment header datetime"
  :group 'gh-viewer)

(defface gh-viewer-clickable
  '((t (:foreground "#FFA000"
                    :underline t)))
  "Face used to RET key aware text"
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

(defmethod gh-viewer-stringify ((time-string string))
  (format-time-string "%Y-%m-%d %H:%M:%S" (parse-iso8601-time-string time-string)))

(defmethod gh-viewer-stringify ((comment ggc:issue-comment) &optional ignore-header)
  (if ignore-header
      (oref comment body)
    (with-slots (author published-at body) comment
      (let ((header (format "%s%s"
                            (propertize (gh-viewer-stringify author)
                                        'face 'gh-viewer-issue-comment-header-login)
                            (propertize (format "  %s"(gh-viewer-stringify published-at))
                                        'face 'gh-viewer-issue-comment-header-datetime))))
        (format "%s\n%s" header body)))))

(defmethod gh-viewer-stringify ((conn ggc:issue-comment-connection) &optional ignore-comment-header)
  (with-slots (nodes) conn
    (mapconcat #'(lambda (e) (gh-viewer-stringify e ignore-comment-header)) nodes "\n\n")))

(defmethod gh-viewer-stringify ((rr ggc:review-request))
  (format "requested a review from %s" (gh-viewer-stringify (oref rr reviewer))))

(defmethod gh-viewer-stringify ((conn ggc:review-request-connection))
  (mapconcat #'gh-viewer-stringify (oref conn nodes) "\n"))

(defface gh-viewer-review-state-changes-requested
  '((t (:underline t :inherit error)))
  "Face used to Review State Changes Requested"
  :group 'gh-viewer)

(defface gh-viewer-review-state-approved
  '((t (:foreground "#268bd2" :underline t)))
  "Face used to Review State Approved"
  :group 'gh-viewer)

(defface gh-viewer-review-state-commented
  '((t (:underline t)))
  "Face used to Review State Commented"
  :group 'gh-viewer)

(defmethod gh-viewer-stringify ((review ggc:pull-request-review))
  (let* ((state (oref review state))
         (header (format "%s%s%s"
                         (propertize (format "%s "
                                             (gh-viewer-stringify (oref review author)))
                                     'face 'gh-viewer-issue-comment-header-login)
                         (propertize (format "%s" state)
                                     'face (cond
                                            ((cl-typep state 'ggc:approved)
                                             'gh-viewer-review-state-approved)
                                            ((cl-typep state 'ggc:commented)
                                             'gh-viewer-review-state-commented)
                                            ((cl-typep state 'ggc:changes-requested)
                                             'gh-viewer-review-state-changes-requested)))
                         (propertize (format " %s"
                                             (gh-viewer-format-time-string (oref review published-at)))
                                     'face 'gh-viewer-issue-comment-header-datetime)))
         (comments (gh-viewer-stringify (oref review comments) t))
         (body (let ((str (oref review body)))
                 (or (and (gh-viewer-blank? str) "")
                     (format "%s\n" str)))))
    (format "%s\n%s%s" header body comments)))

(defmethod gh-viewer-stringify ((conn ggc:pull-request-review-connection))
  (mapconcat #'gh-viewer-stringify (oref conn nodes) "\n\n"))

(defmethod gh-viewer-format-section-title ((title string))
  (propertize title 'face 'gh-viewer-pull-request-section-title))


(defmethod gh-viewer-stringify ((pr ggc:pull-request) repo)
  (cl-labels
      ((open-comments ()
                      (interactive)
                      (gh-viewer-buffer-display (oref pr comments) pr repo))
       (browse-pull-request ()
                            (interactive)
                            (browse-url (oref pr url))))
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
                          (let* ((total (oref (oref pr comments) total-count))
                                 (max total))
                            (format "%s%s\n%s\n%s\n"
                                    (gh-viewer-format-section-title (format "%s Comments:" total))
                                    (if (< max total)
                                        (format " displaying last %s comments" max)
                                      "")
                                    str
                                    (if (< max total)
                                        (format "\n%s\n"
                                                (propertize "[Load More Comments]"
                                                            'face 'gh-viewer-clickable
                                                            'keymap (let ((map (make-sparse-keymap)))
                                                                      (define-key map (kbd "RET") #'open-comments)
                                                                      map)))
                                      ""))))))

          (title (propertize (format "%s\n" (oref pr title))
                             'face 'gh-viewer-pull-request-title
                             'keymap (let ((map (make-sparse-keymap)))
                                       (define-key map (kbd "RET") #'browse-pull-request)
                                       map)))
          (author (gh-viewer-stringify (oref pr author)))
          (info
           (format "%s wants to merge into %s from %s\n"
                   (gh-viewer-stringify (oref pr author))
                   (oref pr base-ref-name)
                   (oref pr head-ref-name)))
          (review-requests (let ((str (gh-viewer-stringify (oref pr review-requests))))
                             (or (and (gh-viewer-blank? str) "")
                                 (format "%s %s\n" (gh-viewer-format-section-title "Review Requests:")
                                         str))))
          (reviews (let ((str (gh-viewer-stringify (oref pr reviews))))
                     (or (and (gh-viewer-blank? str) "")
                         (format "%s\n%s" (gh-viewer-format-section-title "Reviews:")
                                 str))))
          (body (format "%s\n\n" (oref pr body)))
          (separator (mapconcat #'identity
                                (cl-loop for i from 1 to 80
                                         collect "-")
                                ""))
          (reactions (let ((str (gh-viewer-stringify (oref pr reactions))))
                       (or (and (gh-viewer-blank? str) "")
                           (format "%s %s\n"
                                   (gh-viewer-format-section-title "Reactions:")
                                   str)))))
      (format "%s%s%s%s%s"
              (format "%s%s%s%s%s%s" title info reactions assignees labels review-requests)
              (format "\n%s\n\n" separator)
              body
              (format "%s\n\n" separator)
              (format "%s%s" comments reviews)
              ))))

(defmethod gh-viewer-stringify ((conn ggc:reaction-connection))
  (cl-labels
      ((group-by (reactions)
                 (let ((ret (make-hash-table :test 'equal)))
                   (cl-loop for e in reactions
                            do (let* ((content (oref e content))
                                      (existings (gethash content ret)))
                                 (if existings
                                     (puthash content (cons e existings) ret)
                                   (puthash content (list e) ret))))
                   ret)))
    (with-slots (nodes) conn
      (let ((grouped (group-by nodes))
            (ret nil))
        (maphash #'(lambda (key value)
                     (push (format ":%s: %s" (downcase key) (length value))
                           ret))
                 grouped)
        (mapconcat #'identity ret ", ")))))

(defmethod gh-viewer-stringify ((conn ggc:pull-request-connection) repo)
  (with-slots (nodes) conn
    (mapconcat #'(lambda (e) (gh-viewer-stringify e repo)) nodes "\n")))

(defmethod gh-viewer-stringify ((repo gh-viewer-repo))
  (format "%s/%s" (oref repo owner) (oref repo name)))

(provide 'gh-viewer-stringify)
;;; gh-viewer-stringify.el ends here
