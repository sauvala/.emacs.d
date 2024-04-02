;;;; init-org.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package org
  :custom
  (org-hide-emphasis-markers t)
  (word-wrap t)
  (truncate-lines nil)
  :config
  (visual-wrap-prefix-mode))

(provide 'init-org)
