;;;; init-gpt.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-make-azure
    "UAI"
    :protocol "https"
    :host "openai-janne-sauvala.openai.azure.com"
    :endpoint
    "/openai/deployments/gpt-4-1106-preview/chat/completions?api-version=2024-02-01"
    :models '("gpt-4-1106-preview")
    :stream t)
  (gptel-make-azure
    "UAIO"
    :protocol "https"
    :host "openai-janne-sauvala-eus.openai.azure.com"
    :endpoint
    "/openai/deployments/got4o/chat/completions?api-version=2024-02-01"
    :models '("got4o")
    :stream t)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package gptai)

(defvar js/chatgpt-shell-install-dir "~/.emacs.d/modules/chatgpt-shell/")
(when (file-exists-p js/chatgpt-shell-install-dir)
  (use-package chatgpt-shell
    :load-path js/chatgpt-shell-install-dir)
  (require 'chatgpt-shell))

(provide 'init-gpt)
