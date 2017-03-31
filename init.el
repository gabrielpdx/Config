(setq environment (shell-command-to-string "uname"))
(load "$HOME/.local-init.el")
(shell "*shell*")

(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

(setq toggle-scroll-bar nil
      debug-on-error t
      indent-tabs-mode nil
      js-indent-level 2
      column-number-mode t
      confirm-kill-emacs 'yes-or-no-p
      backup-directory-alist '(("." . "~/.emacs-backups"))
      package-list '(helm helm-config projectile helm-projectile org web-mode color)      
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-selected-packages (quote (less-css-mode
					web-mode
					js2-mode
					helm-projectile
					helm-ls-hg
					company)))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . c-mode))

(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24))
      (server-start)))


;;;;;;;;;;;;;;;;;;
;; Presentation ;;
;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :font "Monospace 12") ; 18
(global-linum-mode t)
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; word-wrap .txt, .org, etc.

(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


;;;;;;;;;;;;;;;;;;
;;   Packages   ;;
;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'helm-mode)
(add-hook 'after-init-hook 'projectile-global-mode)
(add-hook 'after-init-hook 'helm-projectile-on)
(setq projectile-completion-system 'helm
      company-idle-delay 0
      company-minimum-prefix-length 1
      company-dabbrev-downcase nil    ;; dabbrev backend downcases by default XD
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x p") 'helm-projectile)
(global-set-key (kbd "C-x g") 'helm-projectile-grep)
(global-set-key (kbd "C-x e") 'helm-projectile-find-other-file)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
            (set (make-local-variable 'company-backends)
                 '((company-shell company-eshell-history)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;     Functions      ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Rectangle (read-only) copy, from https://www.emacswiki.org/emacs/RectangleCommands
(defun my-copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
(global-set-key (kbd "C-x r w") 'my-copy-rectangle)
