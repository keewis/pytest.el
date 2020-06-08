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

(require 'ert)

(require 'pytest-selectors)

;; information about a single selector
(ert-deftest pytest--test-file-p ()
  (should (equal (pytest--test-file-p "tests/test_file.py") t))
  (should (equal (pytest--test-file-p "test_file.pyi") nil))
  (should (equal (pytest--test-file-p "testfile.py") nil)))

(ert-deftest pytest--test-components-p ()
  (should (equal (pytest--test-components-p '("TestGroup1" "TestSubgroup")) t))
  (should (equal (pytest--test-components-p '("TestGroup1")) t))
  (should (equal (pytest--test-components-p '("FactoryTest" "TestSubgroup")) nil))
  (should (equal (pytest--test-components-p '("TestGroup" "SubgroupTest")) nil)))

(ert-deftest pytest--test-name-p ()
  (should (equal (pytest--test-name-p "TestGroup") t))
  (should (equal (pytest--test-name-p "test_function") t))
  (should (equal (pytest--test-name-p "GroupTest") nil))
  (should (equal (pytest--test-name-p "function") nil))
  (should (equal (pytest--test-name-p "") nil)))

(ert-deftest pytest--test-p ()
  (should (equal (pytest--test-p '("tests/test_file.py" "TestCase" "test_function")) t))
  (should (equal (pytest--test-p '("tests/test_file.py" "FactoryTest" "test_function")) nil))
  (should (equal (pytest--test-p '("tests/test_file.pyx" "TestCase" "test_function")) nil))
  (should (equal (pytest--test-p '("tests/file.py" "TestCase" "test_function")) nil))
  (should (equal (pytest--test-p '("tests/test_file.py" "TestCase" "function")) nil)))

;; single selector manipulation
(ert-deftest pytest--split-selector ()
  (let ((selector "test_file.py::TestGroup::test_function")
        (expected '("test_file.py" "TestGroup" "test_function")))
    (should (equal (pytest--split-selector selector) expected))))

(ert-deftest pytest--join-selector ()
  (let ((split-selector '("test_file.py" "TestGroup" "test_function"))
        (expected "test_file.py::TestGroup::test_function"))
    (should (equal (pytest--join-selector split-selector) expected))))
             
(ert-deftest pytest--normalize-selector ()
  (let ((selector '("test_file.py" "TestGroup::test_function"))
        (expected '("test_file.py" "TestGroup" "test_function")))
    (should (equal (pytest--normalize-selector selector) expected))))

(ert-deftest pytest--format-selector ()
  (let ((selector '("tests/test_file.py" "TestGroup" "test_function"))
        (expected "file::Group::function"))
    (should (equal (pytest--format-selector selector) expected))))

(ert-deftest pytest--strip-directory ()
  (let ((selector '("tests/test_file.py" "test_function"))
        (expected '("test_file.py" "test_function")))
    (should (equal (pytest--strip-directory selector) expected))))

;; selector list manipulation
(ert-deftest pytest--join-selectors ()
  (let ((selectors '(("test_file1.py" "TestGroup1" "test_function")
                     ("test_file2.py" "TestGroup2" "test_function")))
        (expected '("test_file1.py::TestGroup1::test_function" "test_file2.py::TestGroup2::test_function")))
    (should (equal (pytest--join-selectors selectors) expected))))

(ert-deftest pytest--split-selectors ()
  (let ((selectors '("test_file1.py::TestGroup1::test_function" "test_file2.py::TestGroup2::test_function"))
        (expected '(("test_file1.py" "TestGroup1" "test_function")
                    ("test_file2.py" "TestGroup2" "test_function"))))
    (should (equal (pytest--split-selectors selectors) expected))))

(ert-deftest pytest--normalize-selectors ()
  (let ((selectors '(("test_file1.py::test_function1") ("test_file2.py" "test_function2")))
        (expected '(("test_file1.py" "test_function1") ("test_file2.py" "test_function2"))))
    (should (equal (pytest--normalize-selectors selectors) expected))))

(ert-deftest pytest--format-selectors ()
  (let ((selectors '(("tests/test_file.py" "TestGroup" "test_function1")
                    ("tests/test_file.py" "test_function2")))
        (expected '("file::Group::function1" "file::function2")))
    (should (equal (pytest--format-selectors selectors) expected))))

(provide 'test-selectors)
;;; test-selectors.el ends here
