# clangd-inactive-regions.el

## About

Eglot extension to support the new clangd inactiveRegions LSP
capability introduced in clangd-17.

Highlights inactive code in a LSP aware way, taking into account
compile time includes and defines for current project.

Listens to inactiveRegions notifications and shades them with one of
the available-methods:

* `darken-foreground` makes inactive code semitransparent blending
  current foreground and background colors
* `shade-background` makes inactive code background slightly darker or
  lighter depending on current theme
* `shadow` applies the shadow face to inactive code

## Screenshots

#### `darken-foreground` method, gruvbox dark theme, 30% opacity

Inactive regions are provided by the language server so they will
honor your include paths and compile time defines. Here I'm looking at
my `emacs-29` branch configured to build on MacOS and you can see
Windows code is correctly disabled.

![darken-foreground](./screenshots/darken-foreground-gruvbox-dark.png)

#### `shade-background` method, modus operandi theme, 10% shading

![shade-background](./screenshots/shade-background-modus-operandi.png)

## Installation

```lisp
(unless (package-installed-p 'clangd-inactive-regions)
  (package-vc-install "https://github.com/fargiolas/clangd-inactive-regions.el"))
```

You will need at least emacs 29.1 and clangd-17.

## Usage

```lisp
(use-package clangd-inactive-regions
  :init
  (add-hook 'eglot-managed-mode-hook #'clangd-inactive-regions-mode)
  :config
  (clangd-inactive-regions-set-method "darken-foreground")
  (clangd-inactive-regions-set-opacity 0.55))
```


## Caveats

As far as I know Emacs doesn't have a way to set foreground text
opacity. Best would be a face attribute so that you can set it in an
overlay covering each inactive region and be done with it. Unfortunately
there is no attribute for this yet.

Hence `darken-foreground` method is a fragile and inefficient hack around
fontification: for each inactive region it looks for symbols with
different faces and applies to each of them a different overlay with a
dimmed foreground color.

It seems to work with cc and c-ts modes and I've been using it daily
for more than a year now, but could totally break other modes or
features I'm not aware of.

If you know a better way please do let me know.