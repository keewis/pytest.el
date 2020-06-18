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
  (describe "a function to check if the selector contains a test file (pytest--test-file-p)"
    (it "detects a valid test file"
      (expect (pytest--test-file-p "tests/test_file.py") :to-be t))
    (it "does not detect non-python files"
      (expect (pytest--test-file-p "test_file.pyi") :to-be nil))

    (it "does not detect invalid file names"
      (expect (pytest--test-file-p "testfile.py") :to-be nil)))

  (describe "a function to check if a name describes a test group (pytest--test-group-p)"
    (it "detects a valid test group"
      (expect (pytest--test-group-p "TestGroup") :to-be t))

    (it "does not detect unittest groups"
      (expect (pytest--test-group-p "GroupTest") :to-be nil)))

  (describe "a function to check if a name describes a test function (pytest--test-name-p)"
    (it "detects correctly named test functions"
      (expect (pytest--test-name-p "test_function") :to-be t))

    (it "does not detect invalid test function names"
      (expect (pytest--test-name-p "function") :to-be nil))

    (it "does not detect test group names"
      (expect (pytest--test-name-p "TestGroup") :to-be nil))

    (it "does not detect unittest group names"
      (expect (pytest--test-name-p "GroupTest") :to-be nil))

    (it "does not detect empty strings"
      (expect (pytest--test-name-p "") :to-be nil)))

  (describe "a function to check if a selector describes a test (pytest--test-p)"
    (it "detects a test without groups"
      (expect (pytest--test-p '("test_file.py" "test_function"))
              :to-be t))

    (it "detects a group without tests"
      (expect (pytest--test-p '("test_file.py" "TestGroup"))
              :to-be t))

    (it "detects a test with groups"
      (expect (pytest--test-p '("tests/test_file.py" "TestCase" "test_function"))
              :to-be t))

    (it "does not detect invalid files"
      (expect (pytest--test-p '("tests/test_file.pyx" "TestCase" "test_function"))
              :to-be nil)
      (expect (pytest--test-p '("tests/file.py" "TestCase" "test_function"))
              :to-be nil))

    (it "does not detect invalid test group names"
      (expect (pytest--test-p '("tests/test_file.py" "FactoryTest" "test_function"))
              :to-be nil))

    (it "does not detect invalid test function names"
      (expect (pytest--test-p '("tests/test_file.py" "TestCase" "function"))
              :to-be nil))

    (it "does not detect nested tests"
      (expect (pytest--test-p '("test_file.py" "test_function" "TestCase"))
              :to-be nil)
      (expect (pytest--test-p '("test_file.py" "test_function" "test_function"))
              :to-be nil))))

;; single selector manipulation
(describe "manipulation of a single selector"
  (describe "a function to split a selector string into a list (pytest--split-selector)"
    (it "splits a valid selector"
      (expect (pytest--split-selector "test_file.py::TestGroup::test_function")
              :to-equal '("test_file.py" "TestGroup" "test_function")))
    ;; TODO: check how this works with invalid data
    )
  (describe "a function to join a selector list into a string (pytest--join-selector)"
    (it "joins a valid selector"
      (expect (pytest--join-selector '("test_file.py" "TestGroup" "test_function"))
              :to-equal "test_file.py::TestGroup::test_function"))
    ;; TODO: check how this works with invalid data
    )

  (describe "a function to normalize a mixed selector list (pytest--normalize-selector)"
    (it "correctly separates test components"
      (expect (pytest--normalize-selector '("test_file.py" "TestGroup::test_function"))
              :to-equal '("test_file.py" "TestGroup" "test_function")))
    ;; TODO: check how this works with invalid data
    )

  (describe "a function to remove the directory from the path part (pytest--strip-directory)"
    (it "removes a single directory"
      (expect (pytest--strip-directory '("tests/test_file.py" "test_function"))
              :to-equal '("test_file.py" "test_function")))
    ;; TODO: try to find edge cases. How does this work when the file
    ;; doesn't have a directory component?
    )

  (describe "a function to format the selector for use in a buffer title (pytest--format-selector)"
    (it "correctly formats a selector with path and group components"
      (expect (pytest--format-selector '("tests/test_file.py" "TestGroup" "test_function"))
              :to-equal "file::Group::function"))
    ;; TODO: more tests
    )

  (describe "a function to extract a test from a selector (pytest--extract-test)"
    (it "removes multiple trailing non-test function names"
      (expect (pytest--extract-test '("test_file.py" "TestGroup" "test_func" "func1" "func2"))
              :to-equal '("test_file.py" "TestGroup" "test_func")))
    (it "removes a single trailing non-test function name"
      (expect (pytest--extract-test '("test_file.py" "TestGroup" "test_func" "func"))
              :to-equal '("test_file.py" "TestGroup" "test_func")))
    (it "does not remove test function names"
      (expect (pytest--extract-test '("test_file.py" "test_func"))
              :to-equal '("test_file.py" "test_func")))
    (it "does not remove from a test group"
      (expect (pytest--extract-test '("test_file.py" "TestGroup" "func"))
              :to-equal '("test_file.py" "TestGroup" "func")))
    (it "does not remove from top-level"
      (expect (pytest--extract-test '("test_file.py" "func"))
              :to-equal '("test_file.py" "func"))))

  (describe "a function to extract a test group from a selector (pytest--extract-group)"
    (it "removes trailing function names"
      (expect (pytest--extract-group '("test_file.py" "TestGroup" "test_func" "func"))
              :to-equal '("test_file.py" "TestGroup")))
    (it "does not remove test groups"
      (expect (pytest--extract-group '("test_file.py" "TestGroup"))
              :to-equal '("test_file.py" "TestGroup"))
      (expect (pytest--extract-group '("test_file.py" "TestGroup1" "TestGroup2"))
              :to-equal '("test_file.py" "TestGroup1" "TestGroup2")))
    (it "never returns a test function"
      (expect (pytest--extract-group '("test_file.py" "test_func" "func"))
              :to-equal nil))))

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
