(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (less-css-mode web-mode js2-mode helm-projectile helm-ls-hg company)))
 '(safe-local-variable-values (quote ((rsync-qk2 . t))))
 '(tool-bar-mode nil))


;; declare directory local variable for rsync hook
(defvar rsync-qk2 nil)
(add-hook 'after-save-hook
          (lambda () (when rsync-qk2 (shell-command (getenv "RSYNC_COMMAND")))))


(setq package-list '(helm helm-config projectile helm-projectile org web-mode color))
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
;(package-initialize)

;; (or (file-exists-p package-user-dir)
;;     (package-refresh-contents))

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (and (window-system)
           (>= emacs-major-version 24))
    (server-start)))

;; OSX ONLY
;; When opened in Finder, manually set $PATH and exec-path per .bashrc
(if (not (getenv "TERM_PROGRAM"))
   (setenv "PATH"
           (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))

;; stacktrace for errors
(setq debug-on-error t)

;; custom colors for company tooltip
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#ffffff"))))
 '(company-scrollbar-fg ((t (:background "#ffffff"))))
 '(company-tooltip ((t (:inherit default :background "#ffffff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

;; start in shell
(shell "*shell*")
(switch-to-buffer "*shell*")

;; use spaces, not tabs
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 2)
;; (setq-default js-indent-level 2)
;; (setq-default c-default-style "linux"
;;               c-basic-offset 4)

;; ask before closing emacs
(setq confirm-kill-emacs 'yes-or-no-p)


;; PRESENTATION
;; 12pt default font face
(set-face-attribute 'default nil :font "Monaco 14")
;; line numbers - display and fix size
(global-linum-mode t)
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))


;; PACKAGES
(add-to-list 'load-path "~/.emacs.d/elpa/less-css-mode-0.20")
(require 'less-css-mode)

(add-to-list 'load-path "~/.emacs.d/elpa/groovy-mode-1.0.1")
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))


;; Load MELPA packages
(setq package-enable-at-startup nil)
(package-initialize)

;; helm
(require 'helm)
(require 'helm-config)
;(require 'helm-ls-hg)
;; use helm for M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(helm-mode 1)

;; helm + projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key (kbd "C-x p") 'helm-projectile)
(global-set-key (kbd "C-x g") 'helm-projectile-grep)
(global-set-key (kbd "C-x e") 'helm-projectile-find-other-file)

;(setq projectile-globally-ignored-directories "-$HOME")


;; org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; web-mode
(add-to-list 'load-path "~/.emacs.d/elpa/web-mode-14")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; use c-mode for C#
(add-to-list 'auto-mode-alist '("\\.cs\\'" . c-mode))

;; company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
