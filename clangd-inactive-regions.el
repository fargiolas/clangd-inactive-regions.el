;;; clangd-inactive-regions.el --- Highlight inactive code regions with clangd power   -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Filippo Argiolas <filippo.argiolas@gmail.com>
;; Based on an example implementation from João Távora <joaotavora@gmail.com> (see bug#65418)

;; Author: Filippo Argiolas <filippo.argiolas@gmail.com>
;; Version: 0.3
;; URL: https://github.com/fargiolas/clangd-inactive-regions
;; Package-Requires: ((emacs "29.1"))

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

(defvar-local clangd-inactive-regions--overlays '())
(defvar-local clangd-inactive-regions--ranges '())
(defvar clangd-inactive-regions--methods '("darken-foreground" "shade-background" "shadow"))

(defvar clangd-inactive-regions-opacity 0.55
  "Blending factor for the `darken-foreground' method.
Used to mix foreground and background color and apply to the
foreground of the inactive region.  The lower the blending factor
the more text will look dim.")

(defvar clangd-inactive-regions-shading 0.08
  "Blending factor for the `shade-background' method.
Used to mix background and foreground and shade inactive region
background.  The higher the less visible the shading will be.")

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

(define-minor-mode clangd-inactive-regions-mode
  "Minor mode to enable Eglot support for clangd inactiveRegions extension."
  :global nil
  (cond (clangd-inactive-regions-mode
         (clangd-inactive-regions--enable))
        (t
         (clangd-inactive-regions--disable))))

(defun clangd-inactive-regions-set-method (method)
"Interactively select a shading METHOD to render inactive code regions."
  (interactive
   (list (let ((completion-ignore-case t)
	       (prompt "Set inactive regions shading method: "))
	   (completing-read prompt clangd-inactive-regions--methods nil t nil))))
  (unless (member method clangd-inactive-regions--methods)
    (error "Unknown shading method: %s" method))
  (setq clangd-inactive-regions-method method)
  (when (clangd-inactive-regions-mode)
    (clangd-inactive-regions-refresh)))

(defun clangd-inactive-regions-set-opacity (opacity)
  "Interactively set a new OPACITY value for inactive regions.
Only applies when `darken-foreground' method is enabled."
  (interactive "nNew inactive region foreground color opacity [0-1.0]: ")
  (unless (and (>= opacity 0.0) (< opacity 1.0))
    (error "Opacity should be between 0.0 and 1.0"))
  (setq clangd-inactive-regions-opacity opacity)
  (when (clangd-inactive-regions-mode)
    (clangd-inactive-regions-refresh)))

(defun clangd-inactive-regions-set-shading (shading)
  "Interactively set a new SHADING value for inactive regions.
Only applies when `shade-background' method is enabled."
  (interactive "nNew inactive region background color shading [0-1.0]: ")
  (unless (and (>= shading 0.0) (< shading 1.0))
    (error "Shading factor should be between 0.0 and 1.0"))
  (setq clangd-inactive-regions-shading shading)
  (when (clangd-inactive-regions-mode)
    (clangd-inactive-regions-refresh)))

(defun clangd-inactive-regions--color-blend (from-color to-color alpha)
  "Linearly interpolate between two colors.
Blend colors FROM-COLOR and TO-COLOR with ALPHA interpolation
factor."
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
  (setq clangd-inactive-regions--overlays '())
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(clangd-inactive-region nil)))
  (font-lock-flush))

(defun clangd-inactive-regions--get-face (pos)
  "Get face at POS.
If no face is present return `default', if multiple faces are
present return the higher priority one."
  (let ((face-prop
         (or (get-text-property pos 'face) 'default)))
    (if (listp face-prop)
        (car face-prop)
      face-prop)))

(defun clangd-inactive-regions--make-darken-face (parent-face)
  "New face from PARENT-FACE with dimmed foreground.
If the correspondend \"clangd-inactive\" face doesn't not exist yet create it."
  (let* ((fg (face-foreground parent-face nil 'default))
         (bg (face-background parent-face nil 'default))
         (clangd-inactive-fg (clangd-inactive-regions--color-blend fg bg clangd-inactive-regions-opacity))
         (clangd-inactive-face-name (concat (face-name parent-face) "-clangd-inactive"))
         (clangd-inactive-face (intern clangd-inactive-face-name))
         (clangd-inactive-doc (concat (face-documentation parent-face) " (clangd inactive region darkened face)")))

    (unless (facep clangd-inactive-face)
      (eval `(defface ,clangd-inactive-face '((t nil)) ,clangd-inactive-doc)))

    (set-face-foreground clangd-inactive-face clangd-inactive-fg)

    clangd-inactive-face))

(defun clangd-inactive-regions--forward-face-or-whitespace ()
  "Forward to the next face change.
Some mode uses `default' face for both generic keywords and
whitespace while some other uses nil for whitespace.  Either way
we don't want to includ whitespace in fontification."
  (let* ((prev-face (get-text-property (point) 'face))
         (_ (forward-char))
         (next-face (get-text-property (point) 'face)))
    (while (and (eq prev-face next-face)
                (not (thing-at-point 'whitespace)))
      (setq prev-face (get-text-property (point) 'face))
      (forward-char)
      (setq next-face (get-text-property (point) 'face)))))

(defun clangd-inactive-regions--fontify (start end &optional verbose)
  "Fontify inactive region with semitransparent faces."
  ;; sometimes font lock fontifies in chunks and each fontification
  ;; functions takes care of extending the region to something
  ;; syntactically relevant... guess we need to do the same, extend to
  ;; cover whole lines seems to work with c modes
  (ignore verbose)
  (when clangd-inactive-regions-mode
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
      (let* ((from (or (text-property-any start end 'clangd-inactive-region t) end))
             (to (or (text-property-any from end 'clangd-inactive-region nil) end))
             (beg from))
        ;; the idea here is to iterate through the region by syntax
        ;; blocks, derive a new face from current one with dimmed
        ;; foreground and apply the new face with an overlay

        ;; there is some overlay duplication for regions extended by the
        ;; above code but they will only live until the next inactive
        ;; region update and don't seem to cause much issues... will keep
        ;; an eye on it while I find a solution
        (when (> to from)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char from)
              (while (<= (point) to)
                (clangd-inactive-regions--forward-face-or-whitespace)
                ;; no need to dim whitespace
                (unless (string-match-p "[[:blank:]\n]" (string (char-before)))
                  (let* ((cur-face (clangd-inactive-regions--get-face (1- (point))))
                         (clangd-inactive-face (clangd-inactive-regions--make-darken-face cur-face)))
                    (let* ((ov (make-overlay beg (point))))
                      (overlay-put ov 'face clangd-inactive-face)
                      (push ov clangd-inactive-regions--overlays))))
                (setq beg (point))))))
        (setq start to)))))

(defun clangd-inactive-regions-refresh ()
  "Force a refresh of known inactive regions.
Useful to update colors after a face or theme change."
  (clangd-inactive-regions-cleanup)
  (when (string= clangd-inactive-regions-method "shade-background")
    (set-face-background 'clangd-inactive-regions-shade-face
                         (clangd-inactive-regions--color-blend
                          (face-foreground 'default)
                          (face-background 'default)
                          clangd-inactive-regions-shading)))
  (dolist (range clangd-inactive-regions--ranges)
    (let ((beg (car range))
          (end (cdr range)))
      (cond
       ((string= clangd-inactive-regions-method "darken-foreground")
        (with-silent-modifications
          (put-text-property beg end 'clangd-inactive-region t))
        (font-lock-flush))
       ((string= clangd-inactive-regions-method "shadow")
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'face 'clangd-inactive-regions-shadow-face)
          (push ov clangd-inactive-regions--overlays)))
       ((string= clangd-inactive-regions-method "shade-background")
        (let ((ov (make-overlay beg (1+ end))))
          (overlay-put ov 'face 'clangd-inactive-regions-shade-face)
          (push ov clangd-inactive-regions--overlays)))))))

;; FIXME: cc-mode.el replaces local fontify-region-function with one
;; that extends it contextually to a syntactially relevant bigger
;; region. Then it calls the global fontify-region-function on the new
;; region boundaries. So if we want to get the extended region we need
;; to advice the global function while our mode is inherently a buffer
;; local one. Only limit this to c-mode and c++-mode while I look for
;; a better alternative.
(defun clangd-inactive-regions--enable ()
  "Helper method to enable inactive regions minor mode."
  (if (memq major-mode '(c-mode c++-mode))
      (add-function :after (default-value 'font-lock-fontify-region-function)
                    #'clangd-inactive-regions--fontify)
    (add-function :after (local 'font-lock-fontify-region-function)
                  #'clangd-inactive-regions--fontify))

  (cl-defmethod eglot-client-capabilities :around (server)
    (let ((base (cl-call-next-method)))
      (when (cl-find "clangd" (process-command (jsonrpc--process server))
                     :test #'string-match)
        (setf (cl-getf (cl-getf base :textDocument)
                       :inactiveRegionsCapabilities)
              '(:inactiveRegions t)))
      base)))

(defun clangd-inactive-regions--enabled-anywhere ()
  "Check if our mode is enabled in any classic C mode buffers."
  (let ((enabled nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (and (memq major-mode '(c-mode c++-mode))
             (clangd-inactive-regions-mode)
             (setq enabled t))))
    enabled))

;; FIXME: same as before, if we are hooking into the global
;; fontification function only remove the hook if we're the last
;; buffer using it.
(defun clangd-inactive-regions--disable ()
  "Helper method to enable inactive regions minor mode."
  (if (memq major-mode '(c-mode c++-mode))
      (unless (clangd-inactive-regions--enabled-anywhere)
        (remove-function (default-value 'font-lock-fontify-region-function)
                         #'clangd-inactive-regions--fontify))
    (remove-function (local 'font-lock-fontify-region-function)
                     #'clangd-inactive-regions--fontify))
  (clangd-inactive-regions-cleanup)
  (cl-defmethod eglot-client-capabilities :around (server)
    (cl-call-next-method server)))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql textDocument/inactiveRegions))
           &key regions textDocument &allow-other-keys)
  "Update inactive regions when clangd reports them."
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
