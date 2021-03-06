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

;; these are the functions provided for external use:
;; - pytest-info--decorator-p
;; - pytest-info-current-pos

;;; Code:

(require 'cl-lib)
(require 'python)
(require 'rx)
(require 's)

(defun pytest-info--decorator-p ()
  "Is there a decorator call at the current position?"
  ;; can't use (python-rx decorator) since that uses ^ to match from
  ;; the start of the line, but looking-at provides only from point on
  (let ((regexp "@[_[:alpha:]][_[:word:]]*")
        at-decorator-p)
    (save-excursion
      (python-nav-beginning-of-statement)
      (setq at-decorator-p (looking-at regexp)))
    at-decorator-p))

(defun pytest-info-current-pos ()
  "Construct a selector for the current position."
  (let (name (path (buffer-file-name)))
    (save-excursion
      (unless (looking-at (python-rx defun))
        (while (pytest-info--decorator-p)
          (python-nav-forward-statement)))
      (unless (looking-at (python-rx defun))
        (python-nav-beginning-of-defun))
      (setq name (python-info-current-defun)))
    (if name (cons path (s-split "\\." name)) nil)))

(provide 'pytest-info)
;;; pytest-info.el ends here
