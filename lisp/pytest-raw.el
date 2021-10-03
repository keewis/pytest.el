;;; pytest-raw.el --- user facing functions to run tests  -*- lexical-binding: t; -*-

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

;;; These interactive functions should be used to bind keys in python-mode

;;; Code:
(require 'cl-lib)

(require 'pytest-buffers)
(require 'pytest-info)
(require 'pytest-process)
(require 'pytest-selectors)


(defun pytest-bury-buffer ()
  "Kill or bury the currently active window."
  (interactive)
  (quit-restore-window))

(defvar pytest-raw-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'pytest-bury-buffer)
    (define-key map "r" #'pytest-raw-rerun)
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
  (defvar quit-restore)
  (setq quit-restore "bury"))

(defun pytest--run-raw (&optional args selectors dir buffer-name)
  "Run pytest in a raw buffer named BUFFER-NAME.

If SELECTORS is non-nil, only run SELECTORS.
If ARGS is non-nil, pass them to pytest.
If DIR is non-nil, run pytest in it."
  (let ((selectors (pytest--normalize-selectors selectors))
        (args (append args (pytest--join-selectors selectors)))
        (output-buffer (pytest--buffer-by-name buffer-name))
        proc)
    (with-current-buffer output-buffer
      (erase-buffer)
      (ansi-color-for-comint-mode-on)
      (comint-mode)
      (pytest-raw-mode))

    (setq proc (pytest--run args dir output-buffer))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc 'comint-output-filter)

    (with-current-buffer output-buffer
      (defvar called-selectors)
      (setq-local called-selectors selectors))))

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

(defun pytest--report-wrong-selector (selector)
  "Report SELECTOR as not valid."
  (if selector
      (error "Not a valid test: %s" (pytest--join-selector selector))
    (error "No test active")))

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
        (pytest--run-raw args selectors dir name)
      (pytest--report-wrong-selector selector))))

(defun pytest-run-selectors (selectors)
  "Run SELECTORS."
  (let ((prepared-selectors (pytest--normalize-selectors selectors))
        (dir nil)
        (args (list "--color=yes"))
        name)
    (setq name (pytest--buffer-name 'pytest-raw-mode prepared-selectors))
    (if (cl-every 'pytest--test-p prepared-selectors)
        (pytest--run-raw args prepared-selectors dir name))))

(defun pytest-run-current-file ()
  "Run the currently opened buffer."
  (interactive)
  (pytest-run-file (buffer-file-name)))

(defun pytest-run-current-test ()
  "Run the test at point."
  (interactive)
  (let ((selector (pytest--extract-test (pytest-info-current-pos))))
    (pytest-run-selector selector)))

(defun pytest-run-current-group ()
  "Run the test group at point."
  (interactive)
  (let ((selector (pytest--extract-group (pytest-info-current-pos))))
    (pytest-run-selector selector)))

(defun pytest-raw-rerun ()
  "Rerun the selectors in a raw buffer."
  (interactive)
  (let ((selectors (buffer-local-value 'called-selectors (current-buffer))))
    (if selectors (pytest-run-selectors selectors) (pytest-run-all))))

(provide 'pytest-raw)
;;; pytest-raw.el ends here
