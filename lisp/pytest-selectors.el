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
(require 'cl-lib)

;; predicates
(defun pytest--test-file-p (path)
  "Is PATH a pytest test file?"
  (let ((name (file-name-nondirectory path)))
    (and (s-starts-with-p "test_" name) (s-ends-with-p ".py" name))))

(defun pytest--test-group-p (name)
  "Is NAME a test group?"
  (s-starts-with-p "Test" name))

(defun pytest--test-name-p (name)
  "Is NAME a test?"
  (s-starts-with-p "test_" name))

(defun pytest--test-name-or-group-p (name)
  "Is NAME a test or a test group?"
  (or (pytest--test-group-p name)
      (pytest--test-name-p name)))

(defun pytest--test-p (selector)
  "Is SELECTOR a pytest test?"
  (let* ((file-path (car selector))
         (full-name (cdr selector))
         (groups (butlast full-name))
         (name (car (last full-name)))
         (is-test-file (pytest--test-file-p file-path))
         (is-test-groups (cl-every 'pytest--test-group-p groups))
         (is-test-name (pytest--test-name-or-group-p name)))
    (and is-test-file is-test-groups is-test-name)))

;; manipulation
(defun pytest--always-list (arg)
  "Ensure ARG is a list by wrapping it if necessary."
  (if (nlistp arg) (list arg) arg))

(defun pytest--split-selector (selector)
  "Split SELECTOR into its components."
  (s-split "::" selector))

(defun pytest--join-selector (selector)
  "Convert SELECTOR to be compatible with pytest."
  (s-join "::" (pytest--always-list selector)))

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

(defun pytest--extract-test (selector)
  "Remove functions until SELECTOR describes a test."
  (while (and (not (pytest--test-name-p (car (last selector))))
              (cl-some 'pytest--test-name-p (butlast selector)))
    (setq selector (butlast selector)))
  selector)

(defun pytest--extract-group (selector)
  "Use the right-most test group in SELECTOR."
  (let* ((full-name (cdr selector))
         (path (car selector))
         (group (cl-loop for elem in full-name
                         while (or (pytest--test-group-p elem)
                                   (s-ends-with-p "Test" elem))
                         collect elem)))
    (if group (cons path group) nil)))

;; formatting
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
