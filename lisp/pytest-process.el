;;; pytest-process.el --- process related variables and functions  -*- lexical-binding: t; -*-

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

(require 'projectile)
(require 's)

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

(defun pytest--execute (name command dir output-buffer)
  "Execute COMMAND asynchronously in DIR.

NAME is a name for the process.

stdout is written to OUTPUT-BUFFER, which needs to be a buffer, not a string."
  (let ((default-directory dir)
        proc)
    (with-current-buffer output-buffer
      (ansi-color-for-comint-mode-on)
      (comint-mode))
    (setq proc (start-process-shell-command name
                                            output-buffer
                                            command))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc 'comint-output-filter)
    proc))

(defun pytest--construct-command (args)
  "Construct the pytest command using ARGS."
  (let ((command (append (pytest--command) args)))
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
