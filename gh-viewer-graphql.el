;;; gh-viewer-graphql.el ---                         -*- lexical-binding: t; -*-

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

(defcustom gh-viewer-token nil
  "Github Token."
  :group 'gh-viewer)

(defcustom gh-viewer-repository-query
  (with-current-buffer
      (find-file-noselect
       (expand-file-name "./graphql/repository.graphql"))
    (buffer-substring-no-properties (point-min) (point-max)))
  "Query for fetch repository."
  :group 'gh-viewer)

(defun gh-viewer-fetch-repository (owner name)
  (let ((query gh-viewer-repository-query)
        (token gh-viewer-token)
        (variables (list (cons "owner" owner)
                         (cons "name" name))))
    (cl-labels
        ((success (data) (message "%s" data))
         (error (data) (message "%s" data))
         (http-error (&key error-thrown &allow-other-keys)
                     (message "%s" error-thrown)))
      (github-graphql-client-request query token
                                     :success #'success
                                     :error #'error
                                     :http-error #'http-error
                                     :variables variables))))

(gh-viewer-fetch-repository "rebaseinc" "instabase")

(provide 'gh-viewer-graphql)
;;; gh-viewer-graphql.el ends here
