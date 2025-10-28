(use-package ivy-posframe
  :config
  (defun my-ivy-posframe-display-at-frame-top-center (str)
    ;; Display the posframe as usual
    (ivy-posframe--display str #'posframe-poshandler-frame-top-center-offset)

    ;; Display the current line and its short description in the minibuffer
    (let* ((current-line (ivy-state-current ivy-last))  ;; Get the current candidate
           (current-docstring (and current-line
                                    (if (fboundp (intern current-line))
                                        (documentation (intern current-line))
                                      nil)))  ;; Get docstring if it’s a function
           (short-description (and current-docstring
                                   (car (split-string current-docstring "\n" t))) ) ;; Get the first line
           ;; Get custom property, e.g., :gptel-help, or any other you expect
           (custom-description (and current-line (get-text-property 0 'gptel-help current-line))))  ;; Adjust based on the property you need
      (when current-line
        (message "%s%s"
                 current-line
                 (if custom-description (format " - %s" custom-description)
                   (if short-description (format " - %s" short-description) ""))))))  ;; Display in the minibuffer

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
;; Set border and background
(setq ivy-posframe-border-width 16)
(custom-set-faces
 '(ivy-posframe ((t (:background "#121212"))))
 '(ivy-posframe-border ((t (:background "#121212")))))

;; Make posframe width dynamic based on frame size
(setq ivy-posframe-width 78)  
(setq ivy-posframe-min-width 78)
(setq ivy-posframe-height 10)
;; (setq ivy-posframe-parameters
;;       '((internal-border-width . 
;;         (left-fringe . 8)
;;         (right-fringe . 8)))

(ivy-posframe-mode 1)
