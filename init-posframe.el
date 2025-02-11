(use-package ivy-posframe
  :config
  (defun my-ivy-posframe-display-at-frame-top-center (str)
    (ivy-posframe--display str #'posframe-poshandler-frame-top-center-offset))

  (defun posframe-poshandler-frame-top-center-offset (info)
    (let ((parent-frame (plist-get info :parent-frame))
          (pos (posframe-poshandler-frame-top-center info)))
      (cons (car pos) (+ (cdr pos) (* 3 (frame-char-height parent-frame))))))

  ;; Prefer swiper in the minibuffer, all else in the top frame
  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-display-function-fallback)
	  (swiper-isearch . ivy-display-function-fallback)
	  (t . my-ivy-posframe-display-at-frame-top-center)))
)

;; Couldn't increase interior padding, so
;; I made the border larger and set it to the same
;; colour as the panel
(setq ivy-posframe-border-width 16)

(custom-set-faces
 '(ivy-posframe ((t (:background "#141414"))))
 '(ivy-posframe-border ((t (:background "#141414")))))

(setq ivy-posframe-width 120)
(setq ivy-rich-display-transformers-list
      '((counsel-M-x
         (:columns
          ((counsel-M-x-transformer (:width 30))
           (ivy-rich-counsel-function-docstring (:face font-lock-comment-face :width 30 :align left)))))))

(ivy-posframe-mode 1)
