;;; pytest-core.el --- group definitions             -*- lexical-binding: t; -*-

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

(defgroup pytest nil
  "Controlling py.test from Emacs"
  :group 'python)

(defgroup pytest-modes nil
  "Modes used or provided by pytest."
  :group 'pytest)

(defgroup pytest-buffers nil
  "Names for buffers."
  :group 'pytest)
(provide 'pytest-core)
;;; pytest-core.el ends here
