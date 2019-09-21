;;; pytest-raw --- Summary: user facing functions to run tests
;;; Commentary:
;;; These interactive functions should be used to bind keys in python-mode
;;; Code:
(require 'cl)

(require 'pytest-buffers)
(require 'pytest-info)
(require 'pytest-process)

(defun pytest-bury-buffer ()
  "Kill or bury the currently active window."
  (interactive)
  (quit-restore-window))

(defvar pytest-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'pytest-bury-buffer)
    map)
  "Keymap used in `pytest-raw-mode'.")

(define-minor-mode pytest-raw-mode
  "Minor mode for viewing raw pytest output.

\\{pytest-raw-mode-map}"
  :lighter "PytestRaw"
  :group 'pytest-modes
  :keymap pytest-raw-mode-map
  (buffer-disable-undo)
  (setq-local line-move-visual t)
  (setq-local font-lock-syntactic-face-function #'ignore)
  (setq show-trailing-whitespace nil)
  (read-only-mode)
  (setq quit-restore "bury"))

(defun pytest--test-file-p (path)
  "Is PATH a pytest test file?"
  (let ((name (file-name-nondirectory path)))
    (and (s-starts-with-p "test_" name) (s-ends-with-p ".py" name))))

(defun pytest--test-components-p (components)
  "Is every entry in COMPONENTS a test class?"
  (every (lambda (x) (s-starts-with-p "Test" x)) components))

(defun pytest--test-p (selector)
  "Is SELECTOR a pytest test?"
  (let ((file-path (car selector))
        (full-name (cdr selector))
        components
        name)
    (setq components (butlast full-name))
    (setq name (car (last full-name)))
    (setq is-test-file (pytest--test-file-p file-path))
    (setq is-test-components (or (not components)
                                 (pytest--test-components-p components)))
    (setq is-test-name (or (not name)
                           (s-starts-with-p "test_" name)))
    (and is-test-file is-test-components is-test-name)))

(defun pytest--run-raw (&optional args selectors dir buffer-name)
  "Run pytest in a raw buffer named BUFFER-NAME.

If SELECTORS is non-nil, only run SELECTORS.
If ARGS is non-nil, pass them to pytest.
If DIR is non-nil, run pytest in it."
  (let ((selectors (pytest--normalize-selectors selectors))
        (args (-concat args (pytest--join-selectors selectors)))
        (output-buffer (pytest--buffer-by-name buffer-name)))
    (pytest--run args dir output-buffer)
    (with-current-buffer output-buffer
      (setq-local called-selectors selectors)
      (pytest-raw-mode))))

(defun pytest-run-all ()
  "Run the whole test suite."
  (interactive)
  (let ((args '("--color=yes"))
        (selectors nil)
        (dir nil)
        (name (pytest--buffer-name 'pytest-raw-mode)))
    (pytest--run-raw args selectors dir name)))

(defun pytest-run-file (file)
  "Run the single test FILE."
  (interactive "fRun file: ")
  (let ((args (list "--color=yes"))
        (dir nil)
        (prepared-selector (pytest--split-selector file))
        selectors
        name)
    (setq name (pytest--buffer-name 'pytest-raw-mode prepared-selector))
    (setq selectors (list prepared-selector))
    (if (pytest--test-file-p file)
        (pytest--run-raw args selectors dir name))))

(defun pytest-run-selector (selector)
  "Run the single test SELECTOR."
  (let ((prepared-selector (pytest--normalize-selector selector))
        (dir nil)
        (args (list "--color=yes"))
        selectors
        name)
    (setq name (pytest--buffer-name 'pytest-raw-mode (list prepared-selector)))
    (setq selectors (list prepared-selector))
    (if (pytest--test-p prepared-selector)
        (pytest--run-raw args selectors dir name))))

(defun pytest-run-current-file ()
  "Run the currently opened buffer."
  (interactive)
  (pytest-run-file (buffer-file-name)))

(defun pytest-run-current-test ()
  "Run the test at point."
  (interactive)
  (let ((selector (pytest-info-current-pos)))
    (pytest-run-selector selector)))

(provide 'pytest-raw)
;;; pytest-raw.el ends here
