;;; eglot-inactive-regions.el --- Highlight inactive code regions with eglot power   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Filippo Argiolas <filippo.argiolas@gmail.com>
;; Based on an example implementation from João Távora
;; <joaotavora@gmail.com> (see bug#65418)

;; Author: Filippo Argiolas <filippo.argiolas@gmail.com>
;; Version: 0.5
;; URL: https://github.com/fargiolas/eglot-inactive-regions
;; Package-Requires: ((emacs "29.1"))

;; eglot-inactive-regions is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; eglot-inactive-regions.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with eglot-inactive-regions.el.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends Eglot to enable inactive code regions
;; detection and shading.
;;
;; LSP servers provide information about currently disabled
;; preprocessor branches with knowledge about build-time options and
;; defines.
;;
;; This mode provides visual feedback to quickly identify these disabled
;; code regions.  Supports three visualization styles:
;;  - `shadow-face' applies shadow face from current theme
;;  - `shade-background' makes the background slightly lighter or darker
;;  - `darken-foreground' dims foreground colors
;;
;; Currently supported servers:
;;  - `clangd' with inactiveRegions extension (needs clangd-17+)
;;  - `ccls' with skippedRegions extension

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'color)
(require 'font-lock)

(defgroup inactive-regions nil "Eglot Inactive Regions."
  :group 'tools
  :prefix "eglot-inactive-regions-")

(defun eglot-inactive-regions--set-and-refresh (sym val)
  "Set custom variable SYM to value VAL trigger a refresh of all active buffers."
  (set sym val)
  (when (fboundp 'eglot-inactive-regions-refresh-all)
    (eglot-inactive-regions-refresh-all)))

(defcustom eglot-inactive-regions-opacity 0.55
  "Blending factor for the `darken-foreground' method.
Used to mix foreground and background colors and apply to the foreground
face of the inactive region.  The lower the blending factor the more
text will look dim."
  :type '(float :tag "Opacity" :min 0.0 :max 1.0)
  :set #'eglot-inactive-regions--set-and-refresh
  :group 'inactive-regions)

(defcustom eglot-inactive-regions-shading 0.08
  "Blending factor for the `shade-background' method.
Used to mix background and foreground colors and shade inactive region
background face.  The higher the less visible the shading will be."
  :type '(float :tag "Opacity" :min 0.0 :max 1.0)
  :set #'eglot-inactive-regions--set-and-refresh
  :group 'inactive-regions)

(defcustom eglot-inactive-regions-method 'darken-foreground
    "Shading method to apply to the inactive code regions.
Allowed methods:
- darken-foreground: dim foreground color
- shade-background: shade background color
- shadow: apply shadow face."
  :type '(choice
          (const :tag "Darken foreground" darken-foreground)
          (const :tag "Shade background" shade-background)
          (const :tag "Shadow" shadow-face))
  :set #'eglot-inactive-regions--set-and-refresh
  :group 'inactive-regions)

(defface eglot-inactive-regions-shadow-face
  '((t (:inherit shadow)))
  "Base face used to fontify inactive code with `shadow' method.")

(defface eglot-inactive-regions-shade-face
  '((t (:extend t)))
  "Base face used to fontify inactive code with `shade-background' method.")

(defvar-local eglot-inactive-regions--overlays '())
(setq-default eglot-inactive-regions--overlays '())
(defvar-local eglot-inactive-regions--ranges '())
(setq-default eglot-inactive-regions--ranges '())
(defvar-local eglot-inactive-regions--active nil)
(setq-default eglot-inactive-regions--active nil)

(define-minor-mode eglot-inactive-regions-mode
  "Minor mode to enable Eglot support for clangd inactiveRegions extension."
  :global t
  :group 'inactive-regions
  (cond (eglot-inactive-regions-mode
         (eglot-inactive-regions--enable))
        (t
         (eglot-inactive-regions--disable))))

(defun eglot-inactive-regions--methods ()
  "Return a list of methods and method names."
  (let ((choices (get 'eglot-inactive-regions-method 'custom-type)))
    (mapcar (lambda (opt)
              (let ((symbol (car (last opt)))
                    (tag (plist-get (cdr opt) :tag)))
                (cons symbol tag)))
            (cdr choices))))

(defun eglot-inactive-regions-set-method (method)
  "Interactively select a shading METHOD to render inactive code regions."
  (interactive
   (let* ((methods (eglot-inactive-regions--methods))
          (names (mapcar #'cdr methods))
          (prompt "Set inactive regions shading method: ")
          (value (completing-read prompt names)))
     (list (car (rassoc value methods)))))
  (setq eglot-inactive-regions-method method)
  (eglot-inactive-regions-refresh-all))

(defun eglot-inactive-regions-set-opacity (opacity)
  "Interactively set a new OPACITY value for inactive regions.
Only applies to `darken-foreground' method."
  (interactive "nNew inactive region foreground color opacity [0-1.0]: ")
  (unless (and (>= opacity 0.0) (<= opacity 1.0))
    (error "Opacity should be between 0.0 and 1.0"))
  (setq eglot-inactive-regions-opacity opacity)
  (eglot-inactive-regions-refresh-all))

(defun eglot-inactive-regions-set-shading (shading)
  "Interactively set a new SHADING value for inactive regions.
Only applies to `shade-background' method."
  (interactive "nNew inactive region background color shading [0-1.0]: ")
  (unless (and (>= shading 0.0) (<= shading 1.0))
    (error "Shading factor should be between 0.0 and 1.0"))
  (setq eglot-inactive-regions-shading shading)
  (eglot-inactive-regions-refresh-all))

(defun eglot-inactive-regions--color-blend (from-color to-color alpha)
  "Linearly interpolate between two colors.
Blend colors FROM-COLOR and TO-COLOR with ALPHA interpolation
factor."
  (let ((from-rgb (color-name-to-rgb from-color))
        (to-rgb (color-name-to-rgb to-color))
        (alpha (min 1.0 (max 0.0 alpha))))
    (if (and from-rgb to-rgb)
      (apply 'color-rgb-to-hex
             (cl-mapcar #'(lambda (a b) (+ (* a alpha) (* b (- 1.0 alpha))))
                        from-rgb to-rgb))
      'unspecified)))

(defun eglot-inactive-regions-cleanup ()
  "Clean up inactive regions."
  (when eglot-inactive-regions--active
    (mapc #'delete-overlay eglot-inactive-regions--overlays)
    (setq eglot-inactive-regions--overlays '())
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(eglot-inactive-region nil)))
    (font-lock-flush)))

(defun eglot-inactive-regions--get-face (pos)
  "Get face at POS.
If no face is present return `default', if multiple faces are
present return the higher priority one."
  (let ((face-prop
         (or (get-text-property pos 'face) 'default)))
    (if (listp face-prop)
        (car face-prop)
      face-prop)))

(defun eglot-inactive-regions--make-darken-face (parent-face)
  "New face from PARENT-FACE with dimmed foreground.
If the correspondend \"clangd-inactive\" face doesn't not exist yet create it."
  (let* ((fg (face-foreground parent-face nil 'default))
         (bg (face-background parent-face nil 'default))
         (eglot-inactive-fg (eglot-inactive-regions--color-blend fg bg eglot-inactive-regions-opacity))
         (eglot-inactive-face-name (concat (face-name parent-face) "-clangd-inactive"))
         (eglot-inactive-face (intern eglot-inactive-face-name))
         (eglot-inactive-doc (concat (face-documentation parent-face) " (clangd inactive region darkened face)")))

    (unless (facep eglot-inactive-face)
      (eval `(defface ,eglot-inactive-face '((t nil)) ,eglot-inactive-doc)))

    (set-face-foreground eglot-inactive-face eglot-inactive-fg)

    eglot-inactive-face))

(defun eglot-inactive-regions--forward-face-or-whitespace ()
  "Forward to the next face change.
Some mode use `default' face for both generic keywords and
whitespace while some other uses nil for whitespace.  Either way
we don't want to include whitespace in fontification."
  (let* ((prev-face (get-text-property (point) 'face))
         (_ (forward-char))
         (next-face (get-text-property (point) 'face)))
    (while (and (eq prev-face next-face)
                (not (thing-at-point 'whitespace)))
      (setq prev-face (get-text-property (point) 'face))
      (forward-char)
      (setq next-face (get-text-property (point) 'face)))))

(defun eglot-inactive-regions--fontify (start end &optional verbose)
  "Fontify inactive region (START END) with semitransparent faces."
  ;; sometimes font lock fontifies in chunks and each fontification
  ;; functions takes care of extending the region to something
  ;; syntactically relevant... guess we need to do the same, extend to
  ;; cover whole lines seems to work with c modes
  (ignore verbose)
  (when (and eglot-inactive-regions-mode
             eglot-inactive-regions--active
             (eq eglot-inactive-regions-method 'darken-foreground))
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
      (let* ((from (or (text-property-any start end 'eglot-inactive-region t) end))
             (to (or (text-property-any from end 'eglot-inactive-region nil) end))
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
                (eglot-inactive-regions--forward-face-or-whitespace)
                ;; no need to dim whitespace
                (unless (string-match-p "[[:blank:]\n]" (string (char-before)))
                  (let* ((cur-face (eglot-inactive-regions--get-face (1- (point))))
                         (eglot-inactive-face (eglot-inactive-regions--make-darken-face cur-face)))
                    (let* ((ov (make-overlay beg (point))))
                      (overlay-put ov 'face eglot-inactive-face)
                      (push ov eglot-inactive-regions--overlays))))
                (setq beg (point))))))
        (setq start to)))))

(defun eglot-inactive-regions-refresh ()
  "Force a refresh of known inactive regions.
Useful to update colors after a face or theme change."
  (eglot-inactive-regions-cleanup)
  (when eglot-inactive-regions--active
    (when (eq eglot-inactive-regions-method 'shade-background)
      (set-face-background 'eglot-inactive-regions-shade-face
                           (eglot-inactive-regions--color-blend
                            (face-foreground 'default)
                            (face-background 'default)
                            eglot-inactive-regions-shading)))
    (dolist (range eglot-inactive-regions--ranges)
      (let ((beg (car range))
            (end (cdr range)))
        (cond
         ((eq eglot-inactive-regions-method 'darken-foreground)
          (with-silent-modifications
            (put-text-property beg end 'eglot-inactive-region t))
          (font-lock-flush))
         ((eq eglot-inactive-regions-method 'shadow-face)
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'eglot-inactive-regions-shadow-face)
            (push ov eglot-inactive-regions--overlays)))
         ((eq eglot-inactive-regions-method 'shade-background)
          (let ((ov (make-overlay beg (1+ end))))
            (overlay-put ov 'face 'eglot-inactive-regions-shade-face)
            (push ov eglot-inactive-regions--overlays))))))))

(defun eglot-inactive-regions-refresh-all ()
  "Refresh all buffers where this mode is enabled."
  (when eglot-inactive-regions-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (eglot-inactive-regions-refresh)))))

(defun eglot-inactive-regions--enable ()
  "Helper method to enable inactive regions minor mode."
  (add-function :after (default-value 'font-lock-fontify-region-function)
                #'eglot-inactive-regions--fontify)
  (add-hook 'change-major-mode-hook #'eglot-inactive-regions-cleanup))

(defun eglot-inactive-regions--disable ()
  "Helper method to enable inactive regions minor mode."
  (remove-function (default-value 'font-lock-fontify-region-function)
                   #'eglot-inactive-regions--fontify)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (eglot-inactive-regions-cleanup)
      (setq eglot-inactive-regions--ranges '())
      (setq eglot-inactive-regions--active nil)))
  (remove-hook 'change-major-mode-hook #'eglot-inactive-regions-cleanup))

(defun eglot-inactive-regions--handle-notification (uri regions)
  "Update inactive REGIONS for the buffer corresponding to URI."
  (if-let* ((path (expand-file-name (eglot-uri-to-path uri)))
            (buffer (find-buffer-visiting path)))
      (with-current-buffer buffer
        (when eglot-inactive-regions-mode
          (unless eglot-inactive-regions--active
            (setq eglot-inactive-regions--active t))
          (setq eglot-inactive-regions--ranges '())
          (cl-loop
           for r across regions
           for (beg . end) = (eglot-range-region r)
           do
           (push (cons beg end) eglot-inactive-regions--ranges))
          (eglot-inactive-regions-refresh)))))

(cl-defmethod eglot-client-capabilities :around (server)
  "Enable inactive code capabilities for SERVER."
  (let ((base (cl-call-next-method)))
    (when (cl-find "clangd" (process-command (jsonrpc--process server))
                   :test #'string-match)
      (setf (cl-getf (cl-getf base :textDocument)
                     :inactiveRegionsCapabilities)
            '(:inactiveRegions t)))
    base))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql $ccls/publishSkippedRanges))
           &key uri skippedRanges)
  "Listen to ccls skippedRanges notifications."
  (eglot-inactive-regions--handle-notification uri skippedRanges))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql textDocument/inactiveRegions))
           &key regions textDocument &allow-other-keys)
  "Listen to clangd inactiveRegions notifications."
  (if-let ((uri (cl-getf textDocument :uri)))
      (eglot-inactive-regions--handle-notification uri regions)))

(provide 'eglot-inactive-regions)

;;; eglot-inactive-regions.el ends here
