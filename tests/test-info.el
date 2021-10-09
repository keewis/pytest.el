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

(defmacro with-mark-at-line (buffer line body)
  "Run BODY with the mark at LINE in BUFFER."
  `(with-current-buffer ,buffer
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
  :var* ((path1 "tests/test_example1.py")
         (path2 "tests/test_example2.py")
         (filepath1 (path-join (projectile-project-root) path1))
         (filepath2 (path-join (projectile-project-root) path2))
         (buffername1 (format "*buttercup::%s*" filepath1))
         (buffername2 (format "*buttercup::%s*" filepath2))
         buffer1
         buffer2)

  (before-all
    (setq buffer1 (get-buffer-create buffername1))
    (setq buffer2 (get-buffer-create buffername2))
    (with-current-buffer buffer1
      (insert-file-contents filepath1)
      (set-buffer-modified-p nil)
      (setq-local buffer-file-name filepath1))
    (with-current-buffer buffer2
      (insert-file-contents filepath2)
      (set-buffer-modified-p nil)
      (setq-local buffer-file-name filepath2)))

  ;; close the buffer again
  (after-all
    (kill-buffer buffer1)
    (kill-buffer buffer2))

  (describe "a function to check if the current statement is a decorator (pytest-info--decorator-p)"
    (it "detects the first line of a decorator"
      (expect (with-mark-at-line buffer1 12 (pytest-info--decorator-p)) :to-be t))

    (it "detects continuation lines of a decorator"
      (expect (with-mark-at-line buffer1 44 (pytest-info--decorator-p)) :to-be t))

    (it "detects the indented first line of a decorator"
      (expect (with-mark-at-line buffer2 24 (pytest-info--decorator-p)) :to-be t))

    (it "detects indented continuation lines of a decorator"
      (expect (with-mark-at-line buffer2 19 (pytest-info--decorator-p)) :to-be t))

    (it "does not detect function declarations"
      (expect (with-mark-at-line buffer1 13 (pytest-info--decorator-p)) :to-be nil))

    (it "does not detect normal statements"
      (expect (with-mark-at-line buffer1 23 (pytest-info--decorator-p)) :to-be nil)))

  (describe "a function to collect information about the current position (pytest-info-current-pos)"
    (it "does not collect information about a empty line at module level"
      (expect (with-mark-at-line buffer1 5 (pytest-info-current-pos))
              :to-equal nil))
    (it "detects a plain function"
      (expect (with-mark-at-line buffer1 8 (pytest-info-current-pos))
              :to-equal (list filepath1 "warn"))
      (expect (with-mark-at-line buffer1 8 (pytest-info-current-pos))
              :to-equal (list filepath1 "warn")))
    (it "detects a function with a decorator"
      (expect (with-mark-at-line buffer1 12 (pytest-info-current-pos))
              :to-equal (list filepath1 "failing"))
      (expect (with-mark-at-line buffer1 44 (pytest-info-current-pos))
              :to-equal (list filepath1 "test_skip")))
    (it "detects a function with multiple decorators"
      (expect (with-mark-at-line buffer1 62 (pytest-info-current-pos))
              :to-equal (list filepath1 "variable"))
      (expect (with-mark-at-line buffer1 59 (pytest-info-current-pos))
              :to-equal (list filepath1 "variable"))
      (expect (with-mark-at-line buffer1 52 (pytest-info-current-pos))
              :to-equal (list filepath1 "variable")))
    (it "detects a function from within the function's body"
      (expect (with-mark-at-line buffer1 34 (pytest-info-current-pos))
              :to-equal (list filepath1 "test_xfail")))))


(provide 'test-info)
;;; test-info.el ends here
