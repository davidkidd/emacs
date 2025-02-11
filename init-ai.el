;; ChatGPT
(use-package gptel
  :bind (("C-c c a" . gptel-menu)))

(use-package gptel-quick)

(gptel-make-ollama "Ollama"             ;Backend prefix
  :host "localhost:11434"               ;Check port number
  :stream t                             ;Stream responses
  :models '(deepseek-r1:8b
	    qwen2.5:7b))               ;list of pickable modles
