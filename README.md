# eglot-inactive-regions

Emacs Eglot extension to visually style inactive preprocessor
branches.

Highlights inactive code in a LSP aware way, taking into account
compile time includes and defines for current project.

## Features

- **Visual indication of inactive code**: dimmed colors, shaded background or custom face can be used to quickly identify disabled code sections.
- **Automatic integration with eglot** when the `eglot-inactive-regions-mode` global minor mode is enabled.

### Styling methods
* `darken-foreground` dims inactive code foreground colors
* `shade-background` shades inactive code background (similar to eclipse style) 
* `shadow-face` applies the shadow face (or any face you like) to inactive code sections 

## Supported servers

- [clangd](https://clangd.llvm.org/) (since clangd-17) with [inactiveRegions](https://github.com/clangd/clangd/issues/132) extension
- [ccls](https://github.com/MaskRay/ccls) with skippedRanges extension. Still experimenting with this. Poorly documented, seems to emit notifications only on file save, feedback welcome!

## Screenshots

#### `darken-foreground` method, gruvbox dark theme, 30% opacity

Inactive regions are provided by the language server so they will
honor your include paths and compile time defines. Here I'm looking at
my `emacs-29` branch configured to build on MacOS and you can see
Windows code is correctly disabled.

![darken-foreground](./screenshots/darken-foreground-gruvbox-dark.png)

#### `shade-background` method, modus operandi theme, 10% shading

![shade-background](./screenshots/shade-background-modus-operandi.png)

## Requirements

- emacs 29.1+
- clangd with inactiveRegions support (clangd-17+)
- ccls with skippedRanges support

## Installation

```lisp
(unless (package-installed-p 'eglot-inactive-regions)
  (package-vc-install "https://github.com/fargiolas/eglot-inactive-regions"))
```

## Usage

```lisp
(use-package eglot-inactive-regions
  :custom
  (eglot-inactive-regions-method 'darken-foreground)
  (eglot-inactive-regions-opacity 0.4)
  :config
  (eglot-inactive-regions-mode 1))
```

## Customization

`M-x customize-group inactive-regions` can be used to select the
preferred shading style, customize text opacity and background shading
or customize the base faces.

## Caveats

As far as I know Emacs doesn't have a way to set foreground text
opacity. Best would be a face attribute so that you can set it in an
overlay covering each inactive region and be done with it. Unfortunately
there is no attribute for this yet.

Hence `darken-foreground` method is a probably fragile and inefficient
hack around fontification: for each inactive region it looks for
symbols with different faces and applies to each of them a different
overlay with a dimmed foreground color.

It seems to work with cc and c-ts modes and I've been using it daily
for more than a year now, but could totally break other modes or
features I'm not aware of.

If you know a better way please do let me know.
