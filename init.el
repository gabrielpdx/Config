;;;;;;;;;;;;;;;;;;;;
;;  Core Config   ;;
;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/local.el")
(shell "*shell*")

(custom-set-variables '(custom-enabled-themes (quote (misterioso)))
                      '(global-linum-mode t)
                      '(column-number-mode t)
                      '(debug-on-error t)
                      '(confirm-kill-emacs 'yes-or-no-p)
                      '(menu-bar-mode nil)
                      '(tool-bar-mode nil)
                      '(scroll-bar-mode nil)
                      '(backup-directory-alist '(("." . "~/.emacs-backups")))
                      '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                           ("melpa-stable" . "https://stable.melpa.org/packages/")))
                      '(indent-tabs-mode nil ["default"])
                      '(js-indent-level 2 ["default"]))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'\\|\\.org\\'\\|\\.md\\'" . visual-line-mode))

(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (window-system) (server-start)))


;;;;;;;;;;;;;;;;;;;;
;;  Presentation  ;;
;;;;;;;;;;;;;;;;;;;;

(let* ((height default-line-height))
  (require 'color)
  (defalias 'lighten (apply-partially 'color-lighten-name
                                      (face-attribute 'default :background)))
  (custom-set-faces `(default ((t(:family "Monospace" :height ,height))))
                    `(linum ((t (:foreground ,(lighten 30) :height ,(floor (* height .75))))))
                    `(company-tooltip ((t (:inherit default :background ,(lighten 5)))))
                    `(company-scrollbar-bg ((t (:background ,(lighten 10)))))
                    `(company-scrollbar-fg ((t (:background ,(lighten 5)))))
                    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
                    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


;;;;;;;;;;;;;;;;;;;;
;; Package Config ;;
;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook
          (lambda ()
            (global-company-mode)
            (helm-mode)
            (projectile-global-mode)
            (helm-projectile-on)))

(setq projectile-completion-system 'helm
      company-idle-delay 0
      company-minimum-prefix-length 1
      company-dabbrev-downcase nil    ;; dabbrev backend downcases by default XD
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
            (set (make-local-variable 'company-backends)
                 '((company-shell company-eshell-history)))))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x p") 'helm-projectile)
(global-set-key (kbd "C-x g") 'helm-projectile-grep)
(global-set-key (kbd "C-x e") 'helm-projectile-find-other-file)


;;;;;;;;;;;;;;;;;;;;
;;   Functions    ;;
;;;;;;;;;;;;;;;;;;;;

;; Rectangle (read-only) copy, from https://www.emacswiki.org/emacs/RectangleCommands
(defun copy-region-rectangle (start end)
  "Copy the region-rectangle (read-only kill-rectangle)"
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
(global-set-key (kbd "C-x r w") 'copy-region-rectangle)
