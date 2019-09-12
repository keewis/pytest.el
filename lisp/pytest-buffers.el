;;; pytest-buffers --- Summary:
;;; Commentary:
;;; Code:
(require 'pytest-core)
(require 'pytest-selectors)

(defcustom pytest-buffer-name-format "*%M: %t%v%s*"
  "The format string used to name pytest buffers.

The following %-sequences are supported:
`%m' The name of the major mode, but with the `-mode` suffix removed.

`%M' Like \"%m\", but abbreviate `pytest-mode` as `pytest`.

`%t' The project root as defined by `projectile'.

`%v' Used to separate project name from executed tests.  If not the
     whole test suite is run, \" — \", else and empty string.

`%s' String representation of the tests that are run."
  :group 'pytest-buffers
  :type 'string)

(defun pytest--buffer-name (mode &optional selectors)
  "Generate the buffer name for a MODE buffer.
The returned name is based on `pytest-buffer-name-format' using the
SELECTORS."
  (let ((m (substring (symbol-name mode) 0 -5))
        (n (file-name-nondirectory (directory-file-name (projectile-project-root))))
        (s (s-join ", " (pytest--format-selectors selectors))))
    (format-spec
     pytest-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'pytest-status-mode) "pytest" m))
       (?v . ,(if (eq selectors nil) "" " — "))
       (?s . ,s)
       (?t . ,n)))))

(defun pytest--buffer-by-name (name)
  "Get buffer named NAME or create it."
  (get-buffer-create name))

  
(provide 'pytest-buffers)
;;; pytest-buffers.el ends here
