# git-spice.el

## Introduction

> [!WARNING]
> This is alpha quality.

This package provides some functionalty for Emacs for [git-spice](https://abhinav.github.io/git-spice/). It was originally a [snippet in my dotfiles](https://j-e-s-s-e.com/blog/magit-plugin-for-git-spice-for-stacked-pull-requests).

There's 2 core bits of functionality:

1. A section in Magit. It shows your stacks. You can click on a stack to check it out.
2. A transient menu for interacting wit git-spice.

Thank you to [Abhinav Gupta](https://github.com/abhinav) for creating [git-spice](https://abhinav.github.io/git-spice/). and [adding JSON output](https://github.com/abhinav/git-spice/pull/862), making this package possible.

## Setup

```emacs-lisp
(use-package git-spice
  :vc
  (:url "https://github.com/jesse-c/git-spice.el")
  :after magit
  :config
  (git-spice-setup-magit-section))
```

## Development

```bash
# Install mise
# https://mise.jdx.dev/

# Install dependencies (Eldev will be installed by mise automatically)
mise install

# Run linting
mise run lint

# Byte-compile
mise run compile
```

Run all tests (unit tests + integration tests):
```bash
mise run test
```

Run only unit tests:
```bash
mise run test-unit
```

Run only integration tests:
```bash
mise run test-integration
```
