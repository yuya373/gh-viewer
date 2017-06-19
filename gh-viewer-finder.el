;;; gh-viewer-finder.el ---                  -*- lexical-binding: t; -*-

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
(require 'gh-viewer-graphql)

(defun gh-viewer-select-repository ()
  (let* ((alist (mapcar #'(lambda (e) (cons (gh-viewer-stringify-short e)
                                            (oref e id)))
                        gh-viewer-graphql-repositories))
         (selected (completing-read "Select Repository: " alist))
         (id (cdr (cl-assoc selected alist :test #'string=))))
    (gh-viewer-find-repository id)))

(defmethod gh-viewer-select ((conn ggc:pull-request-connection))
  (let* ((alist (gh-viewer-comp-read-alist conn))
         (selected (completing-read "Select Pull Request: " alist))
         (id (cdr (cl-assoc selected alist :test #'string=))))
    (gh-viewer-find-pull-request conn id)))

(defmethod gh-viewer-comp-read-alist ((conn ggc:pull-request-connection))
  (with-slots (nodes) conn
    (mapcar #'(lambda (e) (cons (gh-viewer-stringify-short e)
                                (oref e id)))
            nodes)))

(defmethod gh-viewer-find-pull-request ((repo ggc:repository) id)
  (gh-viewer-find-pull-request (oref repo pull-requests) id))

(defmethod gh-viewer-find-pull-request ((conn ggc:pull-request-connection) id)
  (with-slots (nodes) conn
    (cl-find-if #'(lambda (pr) (string= (oref pr id) id))
                nodes)))

(defun gh-viewer-find-repository (id)
  (cl-find-if #'(lambda (e) (string= (oref e id) id))
              gh-viewer-graphql-repositories))

(provide 'gh-viewer-finder)
;;; gh-viewer-finder.el ends here
