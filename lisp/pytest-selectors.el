;;; pytest-selectors.el --- Summary: handle selectors
;;; Commentary:
;; functions to process and format test selectors
;;; Code:
(require 's)

(defun pytest--test-file-p (path)
  "Is PATH a pytest test file?"
  (let ((name (file-name-nondirectory path)))
    (and (s-starts-with-p "test_" name) (s-ends-with-p ".py" name))))

(defun pytest--test-components-p (components)
  "Is every entry in COMPONENTS a test class?"
  ;; this won't work with unittest.TestCase classes since those can be
  ;; named anything. Does python-mode provide functions to get base classes?
  (every (lambda (x) (s-starts-with-p "Test" x)) components))

(defun pytest--test-name-p (name)
  "Is NAME a test or a test group?"
  (or (s-starts-with-p "test_" name)
      (s-starts-with-p "Test" name)))

(defun pytest--test-p (selector)
  "Is SELECTOR a pytest test?"
  (let ((file-path (car selector))
        (full-name (cdr selector))
        components
        name
        is-test-file
        is-test-components
        is-test-name)
    (setq components (butlast full-name))
    (setq name (car (last full-name)))
    (setq is-test-file (pytest--test-file-p file-path))
    (setq is-test-components (or (not components)
                                 (pytest--test-components-p components)))
    (setq is-test-name (or (not name)
                           (pytest--test-name-p name)))
    (and is-test-file is-test-components is-test-name)))

(defun pytest--always-list (arg)
  "Ensure ARG is a list by wrapping it if necessary."
  (if (nlistp arg) (list arg) arg))

(defun pytest--split-selector (selector)
  "Split SELECTOR into its components."
  (s-split "::" selector))

(defun pytest--join-selector (selector)
  "Convert SELECTOR to be compatible with pytest."
  (s-join "::" (pytest--always-list selector)))

(defun pytest--normalize-selector (selector)
  "Convert SELECTOR to a list of components."
  (pytest--split-selector (pytest--join-selector selector)))

(defun pytest--join-selectors (selectors)
  "Convert the SELECTORS to be compatible with pytest."
  (mapcar 'pytest--join-selector selectors))

(defun pytest--split-selectors (selectors)
  "Split the SELECTORS into their components."
  (mapcar 'pytest--split-selector selectors))

(defun pytest--normalize-selectors (selectors)
  "Convert each selector from SELECTORS to be compatible with pytest.

The format of each selector is typically:
  [directory/]file[::nodeid]
where nodeid is the identifier from python with '.' replaced by '::'."
  (mapcar 'pytest--normalize-selector selectors))

(defun pytest--strip-directory (selector)
  "Remove the directory part of SELECTOR."
  (cons (file-name-nondirectory (car selector)) (cdr selector)))

(defun pytest--format-selector (selector)
  "Format normalized SELECTOR to be used in buffer names."
  (let ((replace (lambda (c) (s-replace-regexp "\\(Test_?\\)\\([^.]*\\)\\(\\.py\\)?" "\\2" c)))
        (preprocessed (if (stringp selector) (pytest--split-selector selector) selector))
        without-directory)
    (setq without-directory (pytest--strip-directory preprocessed))
    (s-join "::" (mapcar replace without-directory))))

(defun pytest--format-selectors (selectors)
  "Format the SELECTORS to be used in buffer names."
  (mapcar 'pytest--format-selector selectors))

(provide 'pytest-selectors)
;;; pytest-selectors.el ends here
