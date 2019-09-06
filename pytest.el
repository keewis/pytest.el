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

(require 'projectile)
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
(defgroup pytest nil
  "pytest integration"
  :group 'python
  :prefix "pytest-")

(defcustom pytest-python-executable "python"
  "The name of the python executable.

It will be used to launch pytest using the -m syntax, thus allowing it
to work in every virtual environment."
  :group 'pytest
  :type 'string)

(defcustom pytest-buffer-name-format "*%M: %t%v%s*"
  "The format string used to name pytest buffers.

The following %-sequences are supported:
`%m' The name of the major mode, but with the `-mode` suffix removed.

`%M' Like \"%m\", but abbreviate `pytest-mode` as `pytest`.

`%t' The project root as defined by `projectile'.

`%v' Used to separate project name from executed tests. If not the
     whole test suite is run, \" — \", else and empty string.

`%s' String representation of the tests that are run."
  :group 'pytest-buffers
  :type 'string)

(defun pytest--command ()
  "Construct the base command for pytest."
  (cons pytest-python-executable '("-m" "pytest")))

(defun pytest--format-selectors (selectors)
  "Format the SELECTORS to be used in buffer names"
(defun pytest--buffer-name (mode &optional selectors)
  "Generate the buffer name for a MODE buffer.
The returned name is based on `pytest-buffer-name-format'."
  (let ((m (substring (symbol-name mode) 0 -5))
        (n (file-name-nondirectory (directory-file-name (projectile-project-root)))))
    (format-spec
     pytest-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'pytest-mode) "pytest" m))
       (?t . ,n)))))

(defun pytest--execute-async (command dir output-buffer)
  "Execute COMMAND asynchronously in DIR."
  (let ((default-directory dir))
    (unless (string-match "&[ \t]*\\'" command)
      (setq command (concat command " &"))
      (shell-command command output-buffer))))

(defun pytest--run (&optional args dir output-buffer)
  "Run pytest with ARGS (if any) in the given DIR.

If ARGS is not supplied, the default of () is used.
If DIR is not supplied, it is either the project's root or, if not
available, the current working directory."
  (let ((command (append (pytest--command) args))
        (default-directory (or dir (projectile-project-root))))
    (pytest--execute-async (s-join " " command) default-directory output-buffer)))


(defun pytest--normalize-selectors (selectors)
  "Convert each selector from SELECTORS to be compatible with pytest.

The format of each selector is typically:
  [directory/]file[::nodeid]
where nodeid is the identifier from python with '.' replaced by '::'."
  (mapcar (lambda (selector) (s-join "::" selector)) selectors))

(cl-defun pytest-run-tests (&key selectors args)
  "Run pytest on the tests specified in SELECTORS.

Each entry has to be either a string or a list of parts."
  (interactive)
  (let ((prepared-selectors (pytest--normalize-selectors selectors))
        (buffer-name (pytest--buffer-name (major-mode))))
    (setq args-with-selectors (append (cond ((nlistp args) ())
                                             ((listp args) args))
                                      prepared-selectors))
    (pytest--run args-with-selectors)))

(provide 'pytest)
;;; pytest.el ends here
