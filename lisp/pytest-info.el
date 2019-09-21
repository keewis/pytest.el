;;; pytest-info --- info about the current position
;;; Commentary:
;; This file defines a function to extract information about the currently viewed test:
;; * path
;; * fqn of the name
;; It also provides a function to convert this information into a selector
;;; Code:
(require 'python)

(defun pytest-info--current-pos ()
  "Collect information about the current position."
  (save-excursion
    (let ((name (python-info-current-defun))
          (buffer (buffer-file-name)))
      (unless name
        (python-nav-beginning-of-defun)
        (python-nav-forward-statement)
        (setq name (python-info-current-defun)))
      (list buffer name))))

(defun pytest-info--as-selector (info)
  "Convert INFO to a selector."
  (let ((file-path (car info))
        (name (nth 1 info)))
    (cons file-path (s-split "\\." name))))

(defun pytest-info-current-pos ()
  "Get a selector for the current test."
  (let ((info (pytest-info--current-pos)) selector)
    (setq selector (pytest-info--as-selector info))
    selector))

(provide 'pytest-info)
;;; pytest-info.el ends here
