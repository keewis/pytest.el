;;; pytest-raw --- Summary: user facing functions to run tests
;;; Commentary:
;;; These interactive functions should be used to bind keys in python-mode
;;; Code:
(require 'pytest-buffers)
(require 'pytest-process)

(define-minor-mode pytest-raw-mode "PytestRaw"
  "Minor mode for viewing raw pytest output."
  :group 'pytest-modes
  (read-only-mode))

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
    (if (and (s-starts-with-p "test_") (s-ends-with-p ".py"))
        (pytest--run-raw args dir name))))

(defun pytest-run-current-file ()
  "Run the currently opened buffer."
  (interactive)
  (pytest-run-file (buffer-file-name)))

(provide 'pytest-raw)
;;; pytest-raw.el ends here
