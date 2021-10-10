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
(require 'ansi-color)

(require 'pytest-core)
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

(define-derived-mode pytest-raw-mode special-mode "Pytest Raw"
  "Major mode for viewing pytest raw output.

\\{pytest-raw-mode-map}"
  :group 'pytest-modes
  :keymap pytest-raw-mode-map
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local font-lock-syntactic-face-function #'ignore)
  (setq show-trailing-whitespace nil)
  (defvar quit-restore)
  (setq quit-restore "bury"))

(defcustom pytest--process-filter-preprocessors nil
  "Hooks to run before inserting the output of pytest."
  :group 'pytest-modes
  :type 'hook)

(defcustom pytest--process-filter-postprocessors nil
  "Hooks to run after inserting the output of pytest."
  :group 'pytest-modes
  :type 'hook)

(defun pytest--interpret-carriage-motion (buffer min-point max-point)
  "Interpret the carriage motion characters in a region in BUFFER.

That region is between MIN-POINT and MAX-POINT."
  (with-current-buffer buffer
    (comint-carriage-motion min-point max-point)))

(add-hook 'pytest--process-filter-preprocessors 'ansi-color-apply)
(add-hook 'pytest--process-filter-postprocessors 'pytest--interpret-carriage-motion)

(defun pytest--run-process-filter-preprocessors (output)
  "Run the registered process filters on OUTPUT."
  (let ((funs pytest--process-filter-preprocessors) (result output))
    (while funs
      (setq result (funcall (car funs) result))
      (setq funs (cdr funs)))
    result))

(defun pytest--run-process-filter-postprocessors (buffer min-point max-point)
  "Run the registered process filters on BUFFER between MIN-POINT and MAX-POINT."
  (let ((funs pytest--process-filter-postprocessors))
    (while funs
      (funcall (car funs) buffer min-point max-point)
      (setq funs (cdr funs)))))

(defun pytest--process-filter (proc output)
  "Filter for handling the stdout of PROC, which is in OUTPUT."
  (let ((old-buffer (current-buffer)))
    (display-buffer (process-buffer proc))
    (unwind-protect
        (let (moving (inhibit-read-only t))
          (set-buffer (process-buffer proc))
          (setq moving (= (point) (process-mark proc)))
          (save-excursion
            ;; call filters that work just on the text
            (setq output (pytest--run-process-filter-preprocessors output))
            ;; Insert the text, moving the process-marker.
            (goto-char (process-mark proc))
            (insert (pytest--run-process-filter-preprocessors output))
            ;; call filters that need to work on a buffer
            (pytest--run-process-filter-postprocessors (process-buffer proc)
                                                       (process-mark proc)
                                                       (point))
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

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
      (let ((inhibit-read-only t))
        (erase-buffer))
      (pytest-raw-mode))

    (setq proc (pytest--run args dir output-buffer))
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc 'pytest--process-filter)
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
