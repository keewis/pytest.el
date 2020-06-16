;;; test-info.el --- tests for fetching information from a buffer  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'cl-lib)
(require 'projectile)
(require 's)

(require 'pytest-info)

(defun jump-to-line (line)
  "Jump to LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defmacro with-mark-at-line (line body)
  "Run BODY with the mark at LINE in the current buffer."
  `(save-excursion
     (jump-to-line ,line)
     (progn ,body)))

(defun path-join (&rest paths)
  "Join PATHS into a single path."
  (let* ((path (s-join "/" paths))
        (split-path (s-split "/" path))
        (filtered-path (cl-remove-if (lambda (n) (equal n "")) split-path)))
    (s-join "/" (cons "" filtered-path))))

(describe "information functions"
  ;; use before-all to make sure the file is only read once
  :var* ((path "tests/test_example1.py")
         (filepath (path-join (projectile-project-root) path))
         (buffername (format "*buttercup::%s*" filepath))
         (buffer))

  (before-all
    (setq buffer (get-buffer-create buffername))
    (with-current-buffer buffer
      (insert-file-contents filepath)
      (set-buffer-modified-p nil)
      (setq-local buffer-file-name filepath)))

  ;; close the buffer again
  (after-all
    (kill-buffer buffer))

  (before-each
    (set-buffer buffername))

  (describe "a function to check if the current statement is a decorator (pytest-info--decorator-p)"
    (it "detects the first line of a decorator"
      (expect (with-mark-at-line 10 (pytest-info--decorator-p)) :to-be t))

    (it "detects continuation lines of a decorator"
      (expect (with-mark-at-line 42 (pytest-info--decorator-p)) :to-be t))

    (it "does not detect function declarations"
      (expect (with-mark-at-line 11 (pytest-info--decorator-p)) :to-be nil))

    (it "does not detect normal statements"
      (expect (with-mark-at-line 23 (pytest-info--decorator-p)) :to-be nil)))

  (describe "a function to collect information about the current position (pytest-info-current-pos)"
    (it "does not collect information about a empty line at module level"
      (expect (with-mark-at-line 5 (pytest-info-current-pos))
              :to-equal (list filepath nil "\n")))
    (it "detects a plain function"
      (expect (with-mark-at-line 6 (pytest-info-current-pos))
              :to-equal (list filepath "warn" "def warn():\n"))
      (expect (with-mark-at-line 7 (pytest-info-current-pos))
              :to-equal (list filepath "warn" "def warn():\n")))
    (it "detects a function with a decorator"
      (expect (with-mark-at-line 10 (pytest-info-current-pos))
              :to-equal (list filepath "failing" "def failing():\n"))
      (expect (with-mark-at-line 39 (pytest-info-current-pos))
              :to-equal (list filepath "test_skip" "def test_skip():\n")))
    (it "detects a function with multiple decorators"
      (expect (with-mark-at-line 54 (pytest-info-current-pos))
              :to-equal (list filepath "variable" "def variable(request):\n"))
      (expect (with-mark-at-line 51 (pytest-info-current-pos))
              :to-equal (list filepath "variable" "def variable(request):\n"))
      (expect (with-mark-at-line 46 (pytest-info-current-pos))
              :to-equal (list filepath "variable" "def variable(request):\n")))))

(provide 'test-info)
;;; test-info.el ends here
