;;; pytest-info.el --- info about the current position  -*- lexical-binding: t; -*-

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

;; This file defines a function to extract information about the currently viewed test:
;; * path
;; * fqn of the name
;; It also provides a function to convert this information into a selector

;;; Code:

(require 'python)

(defun pytest-info--current-pos ()
  "Collect information about the current position."
  (save-excursion
    (let ((name (python-info-current-defun))
          (buffer (buffer-file-name)))
      (unless name
        (python-nav-beginning-of-defun)
        (python-nav-forward-statement)
        (setq name (python-info-current-defun)))
      (list buffer name))))

(defun pytest-info--as-selector (info)
  "Convert INFO to a selector."
  (let ((file-path (car info))
        (name (nth 1 info)))
    (cons file-path (s-split "\\." name))))

(defun pytest-info--current-selector ()
  "Get a selector for the current position."
  (let ((info (pytest-info--current-pos)) selector)
    (setq selector (pytest-info--as-selector info))
    selector))

(defun pytest-info-current-test ()
  "Get a selector for the current test."
  (let ((selector (pytest-info--current-selector)))
    selector))

(defun pytest-info--as-group (selector)
  "Get the test group of SELECTOR or nil."
  (let ((file-path (car selector)) (components (cdr selector)) group-components group-selector)
    (setq group-components (loop for elem in components while (s-starts-with-p "Test" elem) collect elem))
    (setq group-selector (if (> (length group-components) 0)
                             (cons file-path group-components)
                           nil))
    group-selector))

(defun pytest-info-current-group ()
  "Get a selector for the current test group."
  (let (selector group-selector)
    (setq selector (pytest-info--current-selector))
    (setq group-selector (pytest-info--as-group selector))
    group-selector))

(provide 'pytest-info)
;;; pytest-info.el ends here
