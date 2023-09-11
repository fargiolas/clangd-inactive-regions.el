# clangd-inactive-regions.el

## About

An eglot extension to support clangd inactiveRegions extension
introduced in clangd-17. Highlights inactive code in a LSP aware way.

Listens to inactiveRegions notifications and shades them with one of
the available-methods:

* `darken-foreground` makes inactive code semitransparent blending
  current foreground and background colors
* `shade-background` makes inactive code background slighly darker or
  lighter depending on current theme
* `shadow` applies the shadow face to inactive code

## Screenshots

#### `darken-foreground` method, gruvbox dark theme

![darken-foreground](./screenshots/darken-foreground-gruvbox-dark.png)

#### `shade-background` method, modus operandi theme

![shade-background](./screenshots/shade-background-modus-operandi.png)

## Installation

At the moment the package is little more than an experiment. If you
want to try it just copy the .el file somewhere in the load path.

Feedback, issues and pull requests more than welcome!

## Usage

```lisp
(use-package clangd-inactive-regions
  :ensure nil
  :init
  (setq clangd-inactive-regions-method "darken-foreground")
  (setq clangd-inactive-regions-opacity 0.55)
  (add-hook 'eglot-managed-mode-hook #'clangd-inactive-regions-mode))
```
