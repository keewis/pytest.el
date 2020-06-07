;;; pytest-selectors.el --- handle selectors         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  pytest.el developers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; functions to process and format test selectors

;;; Code:

(require 's)

(defun always-list (arg)
  "Ensure ARG is a list by wrapping it if necessary."
  (if (nlistp arg) (list arg) arg))

(defun pytest--split-selector (selector)
  "Split SELECTOR into its components."
  (s-split "::" selector))

(defun pytest--join-selector (selector)
  "Convert SELECTOR to be compatible with pytest."
  (s-join "::" (always-list selector)))

(defun pytest--normalize-selector (selector)
  "Convert SELECTOR to a list of components."
  (pytest--split-selector (pytest--join-selector selector)))

(defun pytest--join-selectors (selectors)
  "Convert the SELECTORS to be compatible with pytest."
  (mapcar 'pytest--join-selector selectors))

(defun pytest--split-selectors (selectors)
  "Split the SELECTORS into their components."
  (mapcar 'pytest--split-selector selectors))

(defun pytest--normalize-selectors (selectors)
  "Convert each selector from SELECTORS to be compatible with pytest.

The format of each selector is typically:
  [directory/]file[::nodeid]
where nodeid is the identifier from python with '.' replaced by '::'."
  (mapcar 'pytest--normalize-selector selectors))

(defun pytest--strip-directory (selector)
  "Remove the directory part of SELECTOR."
  (cons (file-name-nondirectory (car selector)) (cdr selector)))

(defun pytest--format-selector (selector)
  "Format normalized SELECTOR to be used in buffer names."
  (let ((replace (lambda (c) (s-replace-regexp "\\(Test_?\\)\\([^.]*\\)\\(\\.py\\)?" "\\2" c)))
        (preprocessed (if (stringp selector) (pytest--split-selector selector) selector))
        without-directory)
    (setq without-directory (pytest--strip-directory preprocessed))
    (s-join "::" (mapcar replace without-directory))))

(defun pytest--format-selectors (selectors)
  "Format the SELECTORS to be used in buffer names."
  (mapcar 'pytest--format-selector selectors))

(provide 'pytest-selectors)
;;; pytest-selectors.el ends here
