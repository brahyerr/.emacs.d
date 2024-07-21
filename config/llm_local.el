;; -*- lexical-binding: t; -*-

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e"  ;; Overwrites previous-window-any-frame
          ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model (pcase (system-name)
			 ("nix-STATION" "llama3:8b-instruct-q8_0")
			 ("nixpad"      "codegemma:instruct"))
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
	   :chat-model (pcase (system-name)
			 ("nix-STATION" "llama3:8b-instruct-q8_0")
			 ("nixpad"      "codegemma:instruct"))

	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))

(provide 'llm_local)
