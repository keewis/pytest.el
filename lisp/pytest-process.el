;;; pytest-process --- Summary: process related variables and functions
;;; Commentary:
;;; Code:
(require 'projectile)
(require 'pytest-core)

(defgroup pytest-process nil
  "python and other external processes used by pytest."
  :group 'pytest)

(defcustom pytest-python-executable "python"
  "The name of the python executable.

It will be used to launch pytest using the -m syntax, thus allowing it
to work in every virtual environment."
  :group 'pytest-process
  :type 'string)

(defun pytest--command ()
  "Construct the base command for pytest."
  (cons pytest-python-executable '("-m" "pytest")))

(defun pytest--execute-async (command dir output-buffer)
  "Execute COMMAND asynchronously in DIR and write to OUTPUT-BUFFER."
  (let ((default-directory dir))
    (unless (string-match "&[ \t]*\\'" command)
      (setq command (concat command " &"))
      (shell-command command output-buffer))))

(defun pytest--construct-command (args)
  "Construct the pytest command using ARGS."
  (let ((command (append (cons pytest-python-executable '("-m" "pytest")) args)))
    (s-join " " command)))

(defun pytest--run (&optional args dir output-buffer)
  "Run pytest with ARGS (if any) in the given DIR and write to OUTPUT-BUFFER.

If optional ARGS is non-nil, these are passed to pytest.

If optional DIR is non-nil, pytest is run in that directory.
Otherwise it is run in the project's root as defined by projectile or
the current working directory.

If optional OUTPUT-BUFFER is non-nil, write to the buffer with that name."
  (let ((command (pytest--construct-command args))
        (default-directory (or dir (projectile-project-root))))
    (pytest--execute-async command default-directory output-buffer)))
(provide 'pytest-process)
;;; pytest-process.el ends here
