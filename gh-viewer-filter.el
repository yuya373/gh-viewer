;;; gh-viewer-filter.el ---                          -*- lexical-binding: t; -*-

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
(require 'github-graphql-client)
(require 'gh-viewer-graphql)

(defmethod gh-viewer-filter-pull-request ((repo ggc:repository) filter)
  (gh-viewer-filter-pull-request (oref repo pull-requests) filter))

(defmethod gh-viewer-filter-pull-request ((conn ggc:pull-request-connection) filter)
  (with-slots (nodes page-info total-count) conn
    (make-instance 'ggc:pull-request-connection
                   :nodes (cl-remove-if-not filter nodes)
                   :page-info page-info
                   :total-count total-count)))

(defmethod gh-viewer-filter-assignee-equal-p ((pr ggc:pull-request) assignee)
  (with-slots (nodes) (oref pr assignees)
    (cl-find-if #'(lambda (user) (string= assignee (oref user login)))
                nodes)))

(defmethod gh-viewer-filter-author-equal-p ((pr ggc:pull-request) author)
  (with-slots (login) (oref pr author)
    (string= login author)))

(defmethod gh-viewer-filter-state-equal-p ((pr ggc:pull-request) state)
  (with-slots ((s state)) pr
    (string= s state)))



(provide 'gh-viewer-filter)
;;; gh-viewer-filter.el ends here
