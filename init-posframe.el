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
 '(ivy-posframe ((t (:background "grey15"))))
 '(ivy-posframe-border ((t (:background "grey15")))))

(ivy-posframe-mode 1)
