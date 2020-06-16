;;; test-selectors.el --- tests for selector related functions  -*- lexical-binding: t; -*-

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

(require 'pytest-selectors)

;; information about a single selector
(describe "information about a single selector"
  (it "does the selector contain a test file? (pytest--test-file-p)"
    (expect (pytest--test-file-p "tests/test_file.py") :to-be t)
    (expect (pytest--test-file-p "test_file.pyi") :to-be nil)
    (expect (pytest--test-file-p "testfile.py") :to-be nil))

  (it "does the last element describe a test? (pytest--test-name-p)"
    (expect (pytest--test-name-p "TestGroup") :to-be t)
    (expect (pytest--test-name-p "test_function") :to-be t)
    (expect (pytest--test-name-p "GroupTest") :to-be nil)
    (expect (pytest--test-name-p "function") :to-be nil)
    (expect (pytest--test-name-p "") :to-be nil))

  (it "does the selector describe tests? (pytest--test-p)"
    (expect (pytest--test-p '("tests/test_file.py" "TestCase" "test_function")) :to-be t)
    (expect (pytest--test-p '("tests/test_file.py" "FactoryTest" "test_function")) :to-be nil)
    (expect (pytest--test-p '("tests/test_file.pyx" "TestCase" "test_function")) :to-be nil)
    (expect (pytest--test-p '("tests/file.py" "TestCase" "test_function")) :to-be nil)
    (expect (pytest--test-p '("tests/test_file.py" "TestCase" "function")) :to-be nil)))

;; single selector manipulation
(describe "manipulation of a single selector"
  (it "split the selector into a list (pytest--split-selector)"
    (expect (pytest--split-selector "test_file.py::TestGroup::test_function")
            :to-equal '("test_file.py" "TestGroup" "test_function"))
    ;; TODO: check how this works with invalid data
    )
  (it "join a selector list into a string (pytest--join-selector)"
    (expect (pytest--join-selector '("test_file.py" "TestGroup" "test_function"))
            :to-equal "test_file.py::TestGroup::test_function")
    ;; TODO: check how this works with invalid data
    )

  (it "make sure the selector is properly split into components (pytest--normalize-selector)"
    (expect (pytest--normalize-selector '("test_file.py" "TestGroup::test_function"))
            :to-equal '("test_file.py" "TestGroup" "test_function"))
    ;; TODO: check how this works with invalid data
    )

  (it "remove the directory component (pytest--strip-directory)"
    (expect (pytest--strip-directory '("tests/test_file.py" "test_function"))
            :to-equal '("test_file.py" "test_function"))
    ;; TODO: try to find edge cases. How does this work when the file
    ;; doesn't have a directory component?
    )

  (it "format the selector for use in a buffer title (pytest--format-selector)"
    (expect (pytest--format-selector '("tests/test_file.py" "TestGroup" "test_function"))
            :to-equal "file::Group::function")
    ;; TODO: more tests
    ))

;; selector list manipulation
(describe "manipulation of a list of selectors"
  (it "split all selectors into components (pytest--split-selectors)"
    (expect (pytest--split-selectors '("test_file1.py::TestGroup1::test_function"
                                      "test_file2.py::TestGroup2::test_function"))
            :to-equal '(("test_file1.py" "TestGroup1" "test_function")
                     ("test_file2.py" "TestGroup2" "test_function")))
    ;; TODO: more tests (especially edge cases)
    )

  (it "join every selector's components into strings (pytest--join-selectors)"
    (expect (pytest--join-selectors '(("test_file1.py" "TestGroup1" "test_function")
                                      ("test_file2.py" "TestGroup2" "test_function")))
            :to-equal '("test_file1.py::TestGroup1::test_function"
                     "test_file2.py::TestGroup2::test_function"))
    ;; TODO: more tests
    )

  (it "make sure all selectors are properly split into components (pytest--normalize-selectors)"
    (expect (pytest--normalize-selectors '(("test_file1.py::test_function1")
                                           ("test_file2.py" "test_function2")))
            :to-equal '(("test_file1.py" "test_function1")
                     ("test_file2.py" "test_function2")))
    ;; TODO: more tests
    )

  (it "format all selectors for use in a buffer title (pytest--format-selectors)"
    (expect (pytest--format-selectors '(("tests/test_file.py" "TestGroup" "test_function1")
                                        ("tests/test_file.py" "test_function2")))
            :to-equal '("file::Group::function1" "file::function2"))
    ;; TODO: more tests
    ))

(provide 'test-selectors)
;;; test-selectors.el ends here
