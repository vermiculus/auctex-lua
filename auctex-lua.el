;;; auctex-lua.el --- Lua editing support for AUCTeX
;;
;; Copyright (C) 2013 Sean Allred
;;
;; Author: Sean Allred (seallred@smcm.edu)
;; Version: 1.0
;; Package-Requires ((auctex "11.86") (lua-mode "20130528.1415"))
;; URL: http://github.com/vermiculus/auctex-lua
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to
;;
;;     The Free Software Foundation, Inc.
;;     51 Franklin Street, Fifth Floor
;;     Boston, MA, 02110-1301
;;     USA.
;;
;; Keywords: latex, lua
;;
;;; Commentary:
;;
;; This package provides some basic utilities for working with Lua
;; code from within AUCTeX.  To start using it, customize
;; `LaTeX-toggle-Lua-editing-key' to your liking (default "C-c l") and
;; bind `LaTeX-edit-Lua-code-start' to it as appropriate in your
;; setup files (.emacs).


(require 'lua-mode)

;;;###autoload
(defvar LaTeX-toggle-Lua-editing-key (kbd "C-c l")
  "Your favorite key to edit embedded Lua code.")

;;;###autoload
(defvar LaTeX-Lua-environments '("luacode" "luacode*")
    "A list of environments that will contain Lua code.")

(eval-and-compile
  (defun LaTeX-mark-environment-contents ()
    "Marks the contents of the innermost LaTeX environment."
    (interactive)
    (let* ((environment-name (LaTeX-current-environment)))
      (search-forward ; search for the end of the current environment
       (format "\\end{%s}" environment-name))
      (push-mark (search-backward "\\end")) ; end then place a mark
      (search-backward ; search for the beginning of the current env
       (format "\\begin{%s}" environment-name))
      (search-forward "}")))) ; search for the end of the \begin{...

;;;###autoload
(eval-and-compile
  (defun LaTeX-edit-Lua-code-start ()
    "Places Lua code in a separate buffer in `lua-mode'."
    (interactive)
    (if (member (LaTeX-current-environment) LaTeX-Lua-environments)
        (let* ((lua-buffer-name (format "%s [Lua]" (buffer-name)))
               (lua-buffer (get-buffer-create lua-buffer-name))
               (lua-code (progn (LaTeX-mark-environment-contents)
                                (buffer-substring-no-properties (point) (mark))))
               (lua-parent-buffer (current-buffer))
               (lua-where-edit-started (point)))
          (switch-to-buffer lua-buffer)
          (setq LaTeX-edit-Lua-code-parent-buffer lua-parent-buffer)
          (setq LaTeX-edit-Lua-code-parent-buffer-point lua-where-edit-started)
          (lua-mode)
          (mapc (lambda (key) ; set habit keys to finish
                  (local-set-key key 'LaTeX-edit-Lua-code-finish))
                (list (kbd "C-x C-s")
                      (eval LaTeX-toggle-Lua-editing-key)))
          (insert lua-code))
      (message "Not in a Lua code environment."))))

(defun LaTeX-edit-Lua-code-get-parent-buffer ()
  LaTeX-edit-Lua-code-parent-buffer)

(defun LaTeX-edit-Lua-code-get-parent-buffer-point ()
  LaTeX-edit-Lua-code-parent-buffer-point)

(eval-and-compile
  (defun LaTeX-edit-Lua-code-finish ()
    (interactive)
    (if (bufferp (LaTeX-edit-Lua-code-get-parent-buffer))
        (let* ((lua-code (progn (widen)
                                (LaTeX-edit-Lua--chomp
                                 (buffer-substring (point-min)
                                                   (point-max))))))
          (kill-buffer)
          (switch-to-buffer LaTeX-edit-Lua-code-parent-buffer)
          (save-excursion
            (goto-char LaTeX-edit-Lua-code-parent-buffer-point)
            (LaTeX-mark-environment-contents)
            (delete-region (point) (mark))
            (insert lua-code)))
      (message "%s  %s"
               "Something went wrong."
               "Am I *really* in a buffer created with `LaTeX-edit-Lua-code-finish'?"))))

(eval-and-compile
  (defun LaTeX-edit-Lua--chomp (str)
    "Chomp leading and tailing whitespace from STR."
    (while (string-match "\\s*.*\\s*"
                         str)
      (setq str (replace-match "" t t str)))
    str)) ; adapted from the Elisp Cookbook: http://www.emacswiki.org/emacs/ElispCookbook#toc6
