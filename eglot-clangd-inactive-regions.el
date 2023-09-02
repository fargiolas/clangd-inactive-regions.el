;;; eglot-clangd-inactive-regions.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Filippo Argiolas <filippo.argiolas@gmail.com>
;; Based on an example implementation from João Távora <joaotavora@gmail.com> (see bug#65418)

;; Author: Filippo Argiolas <filippo.argiolas@gmail.com>
;; Version: 0.1
;; URL: https://github.com/fargiolas/eglot-clangd-inactive-regions
;; Package-Requires: ((eglot "1.15"))

;; eglot-clangd-inactive-regions is free software: you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends Eglot to enable clangd inactiveRegions LSP
;; protocol extension (introduced in clangd-17)
;; It listens to inactiveRegions server notifications and use them to
;; darken inactive code

(require 'eglot)
(require 'cl-lib)

(defvar eglot-clangd-inactive-regions-opacity 0.6
  "Blending factor to mix foreground and background color of current
region. The lower the blending factor the more text will look
dim.")

(defvar-local eglot-clangd-inactive-regions-overlays '())

;; let's shorten namespace to ecir for private functions:
;; [e]glot-[c]langd-[i]nactive-[r]egions
(defun ecir--color-blend (from-color to-color alpha)
  "Linearly interpolate between two colors."
  (let ((from-rgb (color-name-to-rgb from-color))
        (to-rgb (color-name-to-rgb to-color)))
    (apply 'format
           "#%02x%02x%02x"
           (cl-mapcar #'(lambda (a b)
                          (truncate (* 255 (+ (* a alpha)
                                              (* b (- 1.0 alpha))))))
                      from-rgb to-rgb))))

(defun ecir--darken-region (beg end opacity)
  "Iterate over points in BEG END range, detect face changes and
create an overlay with a darkened face for each region with a
different face. Current face foreground is blended with
background with OPACITY as a blending factor. Results may vary
depending on current theme."
  (save-excursion
    (goto-char beg)
    (let ((cur-face (face-at-point))
          (ovbeg beg))
      (while (<= (point) end)
        (let ((new-face (face-at-point)))
          (when (or (not (eq new-face cur-face))
                    (eq (point) end))
            (let* ((ovend (point))
                   (ov (make-overlay ovbeg ovend)))
              (unless (facep cur-face)
                (setq cur-face 'default))
              (let* ((clangd-inactive-face-name
                      (concat (face-name cur-face) "-clangd-inactive"))
                     (clangd-inactive-face (intern clangd-inactive-face-name))
                     (clangd-inactive-doc
                      (concat (face-documentation cur-face)
                              " (clangd inactive region darkened face)"))
                     (fg (face-foreground cur-face nil 'default))
                     (bg (face-background cur-face nil 'default))
                     (clangd-inactive-fg
                      (ecir--color-blend fg bg opacity)))
                (unless (facep clangd-inactive-face)
                  (eval `(defface ,clangd-inactive-face '((t nil)) ,clangd-inactive-doc)))
                (set-face-foreground clangd-inactive-face clangd-inactive-fg)
                (overlay-put ov 'face clangd-inactive-face)
                (push ov eglot-clangd-inactive-regions-overlays)))
            (setq ovbeg (point))
            (setq cur-face new-face)))
        (forward-char)))))

(cl-defmethod eglot-client-capabilities :around (server)
  (let ((base (cl-call-next-method)))
    (when (cl-find "clangd" (process-command (jsonrpc--process server))
                   :test #'string-match)
      (setf (cl-getf (cl-getf base :textDocument)
                     :inactiveRegionsCapabilities)
            '(:inactiveRegions t)))
    base))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql textDocument/inactiveRegions))
           &key regions textDocument &allow-other-keys)
  (if-let* ((path (expand-file-name (eglot--uri-to-path
                                     (cl-getf textDocument :uri))))
            (buffer (find-buffer-visiting path)))
      (with-current-buffer buffer
        (mapc #'delete-overlay eglot-clangd-inactive-regions-overlays)
        (setq eglot-clangd-inactive-regions-overlays '())
        (cl-loop
         for r across regions
         for (beg . end) = (eglot--range-region r)
         do
         (ecir--darken-region beg end eglot-clangd-inactive-regions-opacity)))))

(provide 'eglot-clangd-inactive-regions)
