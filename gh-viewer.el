;;; gh-viewer.el --- view github something           -*- lexical-binding: t; -*-

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

(require 'gh-viewer-repo)
(require 'gh-viewer-pull-request)
(require 'gh-viewer-issue)
(require 'gh-viewer-notification)

(defcustom gh-viewer-completing-read-alist
  (list
   (cons "PullRequests"  'gh-viewer-pull-request)
   (cons "Filtered PullRequests"  'gh-viewer-pull-request-filtered)
   (cons "Issues"  'gh-viewer-issue)
   (cons "Filtered Issues"  'gh-viewer-issue-filtered))
  "Feature alist passed to `completing-read'."
  :group 'gh-viewer)

;;;###autoload
(defun gh-viewer ()
  (interactive)
  (let* ((selected (completing-read "Select Feature: "
                                    gh-viewer-completing-read-alist))
         (fun (and selected
                   (cdr (cl-assoc selected gh-viewer-completing-read-alist
                                  :test #'string=)))))
    (unless fun
      (error "Select one feature"))
    (funcall fun)))



(provide 'gh-viewer)
;;; gh-viewer.el ends here
