;;; clangd-inactive-regions.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Filippo Argiolas <filippo.argiolas@gmail.com>
;; Based on an example implementation from João Távora <joaotavora@gmail.com> (see bug#65418)

;; Author: Filippo Argiolas <filippo.argiolas@gmail.com>
;; Version: 0.2
;; URL: https://github.com/fargiolas/clangd-inactive-regions
;; Package-Requires: ((eglot))

;; clangd-inactive-regions.el is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; clangd-inactive-regions.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with clangd-inactive-regions.el.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends Eglot to enable clangd inactiveRegions LSP
;; protocol extension (introduced in clangd-17)
;; It listens to inactiveRegions server notifications and use them to
;; darken inactive code

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'color)
(require 'font-lock)

(defvar clangd-inactive-regions-opacity 0.55
  "Blending factor for the `darken-foreground' method. Used to mix
foreground and background color and apply to the foreground of
the inactive region. The lower the blending factor the more text
will look dim.")

(defvar clangd-inactive-regions-shading 0.9
  "Blending factor for the `shade-background' method. Used to mix
background and foreground and shade inactive region
background. The higher the less visible the shading will be.")

(defvar clangd-inactive-regions-method "darken-foreground"
  "Shading method to apply to the inactive code regions.
Allowed methods:
- darken-foreground: dim foreground color
- shade-background: shade background color
- shadow: apply shadow face.")

(defface clangd-inactive-regions-shadow-face
  '((t (:inherit shadow)))
  "Face used to inactive code with shadow method.")

(defface clangd-inactive-regions-shade-face
  '((t (:extend t)))
  "Face used to inactive code with shade-background method.")

(defvar-local clangd-inactive-regions--overlays '())
(defvar-local clangd-inactive-regions--ranges '())

(define-minor-mode clangd-inactive-regions-mode
  ""
  :global nil
  (cond (clangd-inactive-regions-mode
         (add-function :after (local 'font-lock-fontify-region-function)
                       #'clangd-inactive-regions--fontify))
        (t
         (remove-function (local 'font-lock-fontify-region-function)
                          #'clangd-inactive-regions--fontify)
         (clangd-inactive-regions-cleanup))))

(defun clangd-inactive-regions--color-blend (from-color to-color alpha)
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

(defun clangd-inactive-regions-cleanup ()
  "Clean up inactive regions."
  (mapc #'delete-overlay clangd-inactive-regions--overlays)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(ecir-inactive nil)))
  (font-lock-flush))

(defun clangd-inactive-regions--get-face (pos)
  (or (get-text-property pos 'face)
      'default))

(defun clangd-inactive-regions--fontify (start end &rest args)
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
          (setq cur-face (clangd-inactive-regions--get-face (point)))
          (while (<= (point) to)
            (let* ((new-face (clangd-inactive-regions--get-face (point))))
              ;; chunk to fontify ends either at a face change or at
              ;; TO region limit. Use face name to check if chunk is already fontified
              (when (and (or (not (eq cur-face new-face))
                             (eq (point) to))
                         (not (string-suffix-p "-clangd-inactive" (face-name cur-face))))

                (let* ((fg (face-foreground cur-face nil 'default))
                       (bg (face-background cur-face nil 'default))
                       (clangd-inactive-fg (clangd-inactive-regions--color-blend fg bg clangd-inactive-regions-opacity))
                       (clangd-inactive-face-name (concat (face-name cur-face) "-clangd-inactive"))
                       (clangd-inactive-face (intern clangd-inactive-face-name))
                       (clangd-inactive-doc (concat (face-documentation cur-face) " (clangd inactive region darkened face)")))

                  (unless (facep clangd-inactive-face)
                    (eval `(defface ,clangd-inactive-face '((t nil)) ,clangd-inactive-doc)))

                  (set-face-foreground clangd-inactive-face clangd-inactive-fg)
                  (put-text-property beg (point) 'face clangd-inactive-face))
                (setq beg (point)))
              (setq cur-face new-face))
            (forward-char)))))
    (setq start to)))

(defun clangd-inactive-regions-refresh ()
  "Force a refresh of known inactive regions without waiting for a
 new notification from the server. Useful to update colors after a
 face or theme change."
  (clangd-inactive-regions-cleanup)
  (when (string= clangd-inactive-regions-method "shade-background")
    (set-face-background 'clangd-inactive-regions-shade-face
     (clangd-inactive-regions--color-blend
      (face-background 'default)
      (face-foreground 'default)
      clangd-inactive-regions-shading)))
  (let ((ranges (copy-tree clangd-inactive-regions--ranges)))
    (dolist (range ranges)
      (let ((beg (car range))
            (end (cdr range)))
        (cond
         ((string= clangd-inactive-regions-method "darken-foreground")
          (with-silent-modifications
            (put-text-property beg end 'ecir-inactive t))
          (font-lock-flush beg end))
         ((string= clangd-inactive-regions-method "shadow")
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'clangd-inactive-regions-shadow-face)
            (push ov clangd-inactive-regions--overlays)))
         ((string= clangd-inactive-regions-method "shade-background")
          (let ((ov (make-overlay beg (1+ end))))
            (overlay-put ov 'face 'clangd-inactive-regions-shade-face)
            (push ov clangd-inactive-regions--overlays)))
         )
        ))))


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
          (when clangd-inactive-regions-mode
            (setq clangd-inactive-regions--ranges '())
            (cl-loop
             for r across regions
             for (beg . end) = (eglot--range-region r)
             do
             (push (cons beg end) clangd-inactive-regions--ranges))
            (clangd-inactive-regions-refresh)))))

(provide 'clangd-inactive-regions)

;;; clangd-inactive-regions.el ends here
