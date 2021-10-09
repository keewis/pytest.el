;;; test-process.el --- tests for functions running the pytest program  -*- lexical-binding: t; -*-

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

(require 'pytest-process)

(describe "pytest commands"
  (it "constructs the python command (pytest--python)"
    (expect (pytest--python) :to-match "/python$")
    (let ((pytest-python-executable "/usr/bin/python"))
      (expect (pytest--python) :to-equal "/usr/bin/python"))
    (let ((pytest-python-executable "/usr/local/bin/python"))
      (expect (pytest--python) :to-equal "/usr/local/bin/python")))

  (it "constructs the base command without args (pytest--command)"
    (expect (pytest--command) :to-equal (list (pytest--python) "-m" "pytest")))

  (it "constructs the command with args (pytest--construct-commands)"
    (expect (pytest--construct-command '("-v" "--color=yes"))
            :to-match "python -m pytest -v --color=yes")))

(provide 'test-process)
;;; test-process.el ends here
