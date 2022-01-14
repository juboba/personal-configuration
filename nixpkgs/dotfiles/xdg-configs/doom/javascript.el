;;; javascript.el -*- lexical-binding: t; -*-

(add-hook 'typescript-mode-hook 'prettify-symbols-mode)
(add-hook 'typescript-mode-hook 'my/add-pretty-lambda)

(add-hook 'rjsx-mode-hook 'editorconfig-mode)
(add-hook 'typescript-mode-hook 'editorconfig-mode)
