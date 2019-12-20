;;; package -- Summary: minor mode to run pytest on the project's test suite
;; Author: keewis <keewis@posteo.de>
;; Version: 0.0.1
;; Package-Requires: ((projectile))
;;; Commentary:
;; This package provides a minor mode for pytest test suites
;;
;; goals:
;; * only select current file / test if:
;;  - python-mode is enabled
;;  - buffer named "test_*.py"
;; * F5 runs the current test file
;; * S-F5 runs the current test
;; * C-F5 runs the whole test suite (runs in the project directory)
;; * M-F5 reruns the previously run tests
;; * C-S-F5 runs the previously failed tests
;; * C-x F5 opens a buffer where individual options can be set (though current test won't be possible)
;; * when the tests are run, the results are collected in a mode buffer where
;;  - individual test files can be expanded / collapsed, hiding tests
;;  - hovering over a ./F/E/xX/s displays the name in the status line
;;  - pressing RET opens a new buffer with an overview of errors and warnings
;;    → each error / warning can be expanded to show more information (traceback, etc.)
;; → magit-popup
;; → buffer names should be identifying the run tests:
;;  - whole test suite: only the project's name. example: xarray
;;  - single / multiple files: filename(s) without extension and prefix. example: xarray — units, sparse
;;  - single / multiple tests: like files, but the test hierarchy should be respected.
;;    examples: units::DataArray::(init, repr), sparse::Variable
;;; Code:
(require 'python)
(require 'pytest-raw)

(defun setup-pytest-keybindings ()
  "Set up pytest keybindings in `python-mode'."
  (define-key python-mode-map (kbd "<f5>") 'pytest-run-current-file)
  (define-key python-mode-map (kbd "<S-f5>") 'pytest-run-current-test)
  (define-key python-mode-map (kbd "<C-f5>") 'pytest-run-current-group)
  (define-key python-mode-map (kbd "<M-f5>") 'pytest-run-all))
(add-hook 'python-mode-hook 'setup-pytest-keybindings)

(require 'projectile)
(require 's)
;; todo:
;; + variables
;; * customization
;; - python executable (default: "python")
;; + functions:
;; * information gathering: context
;; - get project root (working directory)
;; - get current file
;; - get current test name
;; - get failed tests (requires parsing pytest output)
;; * test selector construction
;; - select all project root
;; - select specified file
;; - select specified test
;; - select failed tests
;; * execution
;; - run pytest with passed args in the given directory (default: cwd)
;; - run pytest with passed args in project root
;; * modes
;; - test overview
;; - test details
;; - test selection buffer

(require 'pytest-core)
(provide 'pytest)
;;; pytest.el ends here
