;;; pytest-raw --- Summary: user facing functions to run tests
;;; Commentary:
;;; These interactive functions should be used to bind keys in python-mode
;;; Code:
(require 'pytest-buffers)
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

(defun pytest--run-raw (&optional args dir buffer-name)
  "Run pytest in a raw buffer named BUFFER-NAME.

If ARGS is non-nil, pass them to pytest.
If DIR is non-nil, run pytest in it."
  (let ((output-buffer (pytest--buffer-by-name buffer-name)))
    (pytest--run args dir output-buffer)
    (with-current-buffer output-buffer
      (pytest-raw-mode))))

(defun pytest-run-all ()
  "Run the whole test suite."
  (interactive)
  (let ((args '("--color=yes"))
        (dir nil)
        (name (pytest--buffer-name 'pytest-raw-mode)))
    (pytest--run-raw args dir name)))

(defun pytest-run-file (file)
  "Run the single test FILE."
  (interactive "fRun file: ")
  (let ((args (list "--color=yes" file))
        (dir nil)
        (prepared-selector (pytest--split-selector file)))
    (setq name (pytest--buffer-name 'pytest-raw-mode prepared-selector))
    (if (pytest--test-file-p file)
        (pytest--run-raw args dir name))))

(defun pytest-run-current-file ()
  "Run the currently opened buffer."
  (interactive)
  (pytest-run-file (buffer-file-name)))

(provide 'pytest-raw)
;;; pytest-raw.el ends here
