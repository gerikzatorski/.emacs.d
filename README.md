# Emacs Configuration

## Setup

Need to install some ubuntu packages.

```sh
# company-irony
sudo apt install libclang-dev clang

# counsel / projectile
sudo apt install silversearcher-ag

# counsel-etags
sudo apt install universal-ctags

# markdown-mode
sudo apt install markdown
```

Also need to run some commands within emacs.

- `M-x irony-install-server`
- `M-x jedi:install-server`

## Packages

A list of some packages I use for emacs including useful links and commands.

### use-package

[GitHub](https://github.com/jwiegley/use-package)

A package configuration and loading tool.

### yasnippet

[GitHub](https://github.com/joaotavora/yasnippet) | [Documentation](http://joaotavora.github.io/yasnippet/)

Yet another templating system.

### yasnippet-snippets

[GitHub](https://github.com/AndreaCrotti/yasnippet-snippets)

A collection of snippets for yasnippet.

### neotree

[GitHub](https://github.com/jaypei/emacs-neotree)

- `neotree-toggle`

A package to diplay file tree structure in an emacs buffer.

### company

[GitHub](https://github.com/company-mode/company-mode)

A text completion framework for Emacs.

### company-irony

[GitHub](https://github.com/Sarcasm/company-irony)

A completion backend for company-mode with C, C++, and Objective-C languages.

### irony

[GitHub](https://github.com/Sarcasm/irony-mode)

A package to improve C, C++, and Objective-C editing experience.

### company-jedi

[GitHub](https://github.com/emacsorphanage/company-jedi)

A completion backend for company-mode with the Python language.

### exec-path-from-shell

[GitHub](https://github.com/purcell/exec-path-from-shell)

A package to ensure environment variables inside emacs look the same as in the user's shell.

### magit

[GitHub](https://github.com/magit/magit)

An emacs interface for git.

- `magit-status`

### ivy

[GitHub](https://github.com/abo-abo/swiper)

A generic completion package.

- `ivy-resume`

### counsel

[GitHub](https://github.com/abo-abo/swiper)

A collection of ivy-enhanced emacs commands.

- `counsel-M-x`
- `counsel-find-file`
- `counsel-imenu`
- `counsel-yank-pop`

### swiper

[GitHub](https://github.com/abo-abo/swiper)

An ivy-enhanced alternative to Isearch.

- `swiper`

### counsel-etags

[GitHub](https://github.com/redguardtoo/counsel-etags)

A code navigation package based on ctags/etags.

### avy

[GitHub](https://github.com/abo-abo/avy)

A code navigation package to jump to visible text.

- `avy-goto-char`
- `avy-goto-word-or-subword-1`

### ace-window

[GitHub](https://github.com/abo-abo/ace-window)

- `ace-window`

### projectile

[GitHub](https://github.com/bbatsov/projectile)

A project interaction library for Emacs.

For commands refer to counsel-projectile.

### counsel-projectile

[GitHub](https://github.com/ericdanan/counsel-projectile)

A package to provide further ivy integration with projectile (there is some built-in).

- `counsel-projectile-switch-project`
- `counsel-projectile-find-file-dwim`

### multiple-cursors

[GitHub](https://github.com/magnars/multiple-cursors.el)

A tool to edit with multiple cursors.

- `mc/edit-lines`
- `mc/mark-all-like-this`
- `mc/insert-letters`
- `mc/insert-numbers`

### expand-region

[GitHub](https://github.com/magnars/expand-region.el)

A tool to quickly modify the emacs mark/region.

- `er/expand-region`
- `er/contract-region`
