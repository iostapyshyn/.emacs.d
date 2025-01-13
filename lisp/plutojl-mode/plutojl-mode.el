;;; plutojl-mode.el --- Support for Pluto.jl  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 The plutojl-mode Project Contributors

;; Version: 0.10.0
;; Author: Illia Ostapyshyn <illia@yshyn.com>
;; Maintainer: Illia Ostapyshyn <illia@yshyn.com>
;; Keywords: languages julia
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.1")
;;                    (uuidgen "1.3"))

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for Pluto.jl notebooks

;;; Code:

(require 'uuidgen)

(defvar plutojl--process nil)

(defun plutojl-run-stop ()
  "Start (and open) or stop the Pluto server."
  (interactive)
  (if plutojl--process
      (progn
        (delete-process plutojl--process)
        (setq plutojl--process nil)
        (message "Pluto process killed"))
    (setq plutojl--process
          (start-process "plutojl" (get-buffer-create "*plutojl*") "julia" "-e" "import Pluto; Pluto.run(auto_reload_from_file=true, disable_writing_notebook_files=false)"))
    (message "Pluto process started")))


(defun plutojl-insert-cell ()
  (interactive)
  (let ((uuid-prev (save-excursion
                     (when (re-search-backward
                            (concat (regexp-quote "# ╔═╡ ") "\\(" thing-at-point-uuid-regexp "\\)")
                            nil t)
                       (substring-no-properties (match-string 1)))))
        (uuid-new (uuidgen-4)))
    (save-excursion
      (re-search-forward (if uuid-prev
                          (concat "# \\(╠═\\|╟─\\)" (regexp-quote uuid-prev))
                        "# ╔═╡ Cell order:"))
      (insert (concat "\n# ╠═" uuid-new)))
    (beginning-of-line)
    (open-line 2)
    (forward-line)
    (insert "# ╔═╡ " uuid-new)
    (forward-line)
    (back-to-indentation)))

;;;###autoload
(defun turn-on-plutojl-mode ()
  (when (save-excursion
          (goto-char (point-min))
          (looking-at (regexp-quote "### A Pluto.jl notebook ###")))
    (plutojl-mode 1)))

;;;###autoload
(define-minor-mode plutojl-mode
  "Minor mode for Pluto.jl notebooks."
  :lighter " Pluto"
  :keymap (list (cons (kbd "C-<return>") #'plutojl-insert-cell)
                (cons (kbd "C-c C-s") #'plutojl-run-stop))
  (when plutojl-mode
    (turn-on-auto-revert-mode)))

;; (add-to-list 'magic-mode-alist (cons (regexp-quote "### A Pluto.jl notebook ###")
;;                                      #'plutojl-mode))


(provide 'plutojl-mode)

;;; plutojl-mode.el ends here
