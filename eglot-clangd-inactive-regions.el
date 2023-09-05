;;; eglot-clangd-inactive-regions.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Filippo Argiolas <filippo.argiolas@gmail.com>
;; Based on an example implementation from João Távora <joaotavora@gmail.com> (see bug#65418)

;; Author: Filippo Argiolas <filippo.argiolas@gmail.com>
;; Version: 0.2
;; URL: https://github.com/fargiolas/eglot-clangd-inactive-regions
;; Package-Requires: ((eglot "1.15"))

;; eglot-clangd-inactive-regions is free software: you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; eglot-clangd-inactive-regions is distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with eglot-clangd-inactive-regions.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends Eglot to enable clangd inactiveRegions LSP
;; protocol extension (introduced in clangd-17)
;; It listens to inactiveRegions server notifications and use them to
;; darken inactive code

;;; Code:

(require 'eglot)
(require 'cl-lib)

(defvar eglot-clangd-inactive-regions-opacity 0.3
  "Blending factor to mix foreground and background color of current
region. The lower the blending factor the more text will look
dim.")

(defvar-local eglot-clangd-inactive-regions-overlays '())
(defvar-local eglot-clangd-inactive-regions-ranges '())

(define-minor-mode eglot-clangd-inactive-regions-mode
  ""
  :global nil
  (cond (eglot-clangd-inactive-regions-mode
         (add-function :after (local 'font-lock-fontify-region-function)
                       #'eglot-inactive-regions-fontify))
        (t
         (remove-function (local 'font-lock-fontify-region-function)
                          #'eglot-inactive-regions-fontify)
         (eglot-clangd-inactive-regions-cleanup))))

(defun eglot-clangd-inactive-regions--color-blend (from-color to-color alpha)
  "Linearly interpolate between two colors."
  (let ((from-rgb (color-name-to-rgb from-color))
        (to-rgb (color-name-to-rgb to-color))
        (alpha (min 1.0 (max 0.0 alpha))))
    (apply 'format
           "#%02x%02x%02x"
           (cl-mapcar #'(lambda (a b)
                          (truncate (* 255 (+ (* a alpha)
                                              (* b (- 1.0 alpha))))))
                      from-rgb to-rgb))))

(defun eglot-clangd-inactive-regions-cleanup ()
  "Clean up inactive regions."
  (message "CLEANUP")
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(ecir-inactive nil)))
  (mapc #'delete-overlay eglot-clangd-inactive-regions-overlays)
  (font-lock-update))

(defun eglot-clangd-inactive-regions--get-face (pos)
  (or (get-text-property pos 'face)
      'default))

(defun eglot-inactive-regions-fontify (start end &rest args)
  ;; sometimes font lock fontifies in chunks and each fontification
  ;; functions takes care of extending the region to something
  ;; syntactically relevant... guess we need to do the same, extend to
  ;; cover whole lines seems to work with c modes
  (save-excursion
    (save-restriction
      (widen)
      (goto-char end)
      (when (not (eolp))
        (end-of-line)
        (setq end (point)))

      (goto-char start)
      (when (not (bolp))
        (beginning-of-line)
        (setq start (point)))))

  ;; find the inactive region inside the region to fontify
  (while (and start (< start end))
    (setq from (or (text-property-any start end 'ecir-inactive t)
                    end))
    (setq to (or (text-property-any from end 'ecir-inactive nil)
                    end))

    ;; the idea now is to iterate through the region, split it at face
    ;; changes, create a new inactive face with darkened color and
    ;; apply it

    ;; code is a bit more convoluted than I'd like but I'm pretty new
    ;; at elisp and it seems to work
    (setq beg from)
    (when (> to from)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char from)
          (setq cur-face (eglot-clangd-inactive-regions--get-face (point)))
          (while (<= (point) to)
            (let* ((new-face (eglot-clangd-inactive-regions--get-face (point))))
              ;; chunk to fontify ends either at a face change or at
              ;; TO region limit. Use face name to check if chunk is already fontified
              (when (and (or (not (eq cur-face new-face))
                             (eq (point) to))
                         (not (string-suffix-p "-clangd-inactive" (face-name cur-face))))

                (let* ((fg (face-foreground cur-face nil 'default))
                       (bg (face-background cur-face nil 'default))
                       (clangd-inactive-fg
                        (eglot-clangd-inactive-regions--color-blend
                         fg bg eglot-clangd-inactive-regions-opacity))
                       (clangd-inactive-face-name
                        (concat (face-name cur-face) "-clangd-inactive"))
                       (clangd-inactive-face (intern clangd-inactive-face-name))
                       (clangd-inactive-doc
                        (concat (face-documentation cur-face)
                                " (clangd inactive region darkened face)")))

                  (unless (facep clangd-inactive-face)
                    (eval `(defface ,clangd-inactive-face '((t nil)) ,clangd-inactive-doc)))

                  (set-face-foreground clangd-inactive-face clangd-inactive-fg)
                  (put-text-property beg (point) 'face clangd-inactive-face))
                (setq beg (point)))
              (setq cur-face new-face))
            (forward-char)))))
    (setq start to)))

(defun eglot-clangd-inactive-regions-refresh ()
  "Force a refresh of known inactive regions without waiting for a
 new notification from the server. Useful to update colors after a
 face or theme change."
  (let ((ranges (copy-tree eglot-clangd-inactive-regions-ranges)))
    (dolist (range ranges)
      (let ((beg (car range))
            (end (cdr range)))
        (with-silent-modifications
          (put-text-property beg end 'ecir-inactive t))
        (font-lock-flush beg end)))))


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
          (when eglot-clangd-inactive-regions-mode
            (eglot-clangd-inactive-regions-cleanup)
            (setq eglot-clangd-inactive-regions-ranges '())
            (cl-loop
             for r across regions
             for (beg . end) = (eglot--range-region r)
             do
             (push (cons beg end) eglot-clangd-inactive-regions-ranges))
            (eglot-clangd-inactive-regions-refresh)))))

(provide 'eglot-clangd-inactive-regions)

;;; eglot-clangd-inactive-regions.el ends here
