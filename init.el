(setq environment (shell-command-to-string "uname"))

;; rectangle copy (read-only)
;; from https://www.emacswiki.org/emacs/RectangleCommands
(defun my-copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))
(global-set-key (kbd "C-x r w") 'my-copy-rectangle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (less-css-mode web-mode js2-mode helm-projectile helm-ls-hg company)))
 '(safe-local-variable-values
   (quote
    ((RSYNC_COMMAND . "rsync -rltv --delete --exclude=.hg --exclude=out --exclude=target --exclude=build /home/gabe/Projects/qk2/ /nfs/rkgd/qk2/ &")
     (rsync-qk2 . t))))
 '(tool-bar-mode nil)
 '(menu-bar-mode nil) 
 '(toggle-scroll-bar nil)
 )

(require 'package)

;; declare directory local variable for rsync hook
(defvar rsync-qk2 nil)
(add-hook 'after-save-hook
          (lambda () (when rsync-qk2 (shell-command RSYNC_COMMAND))))

(setq package-list '(helm helm-config projectile helm-projectile org web-mode color))
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq backup-directory-alist `(("." . "~/.emacs-backups")))

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
(delete-other-windows)

;; use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level 2)

;; ask before closing emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;;;;;;;;;;;;;;;;;;
;; PRESENTATION ;;
;;;;;;;;;;;;;;;;;;
;; 12pt default font face
(set-face-attribute 'default nil :font "Monospace 12") ; 18
;; line numbers - display and fix size
(global-linum-mode t)
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))
;; show column number at point
(setq column-number-mode t)

;; Word wrap for all text-mode files (org, etc.)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

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

;; helm in eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

;; helm + projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key (kbd "C-x p") 'helm-projectile)
(global-set-key (kbd "C-x g") 'helm-projectile-grep)
(global-set-key (kbd "C-x e") 'helm-projectile-find-other-file)

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
;; dabbrev backend downcases plaintext by default - BAD DABBREV
(setq company-dabbrev-downcase nil)
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-shell company-eshell-history)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Pomodoro technique ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq pomodori 0)
(defun pom ()
  "Use the pomodoro method."
  (interactive)

  (defun startMacProcess (timeToRun processName emoji)
    "Notify via Applescript"
    (async-start-process processName "/usr/bin/osascript" nil
                         "-e" (concat "display notification with title \""
                                      emoji
                                      "\" "
                                      "subtitle \""
                                      (format-time-string "%H:%M" timeToRun)
                                      (concat " + poms = " (number-to-string pomodori))
                                      "\"")))

  (when (string-equal environment "Darwin") (fset 'startProcess 'startMacProcess))
  
  (defun notify (timeToRun processName emoji accounting)
    "Notify with the emoji at the timeToRun"
    (run-at-time timeToRun
                 nil
                 'startMacProcess timeToRun processName emoji)
    (when accounting (run-at-time timeToRun nil 'accounting)))
  
  (let* ((focusFor (* 60 25))
         (relaxFor (cond ((= pomodori 3) (* 60  15))
                         (t (* 60  5))))
         (startAt  (current-time))
         (breakAt  (time-add startAt (seconds-to-time focusFor)))
         (resumeAt (time-add breakAt (seconds-to-time relaxFor))))

    (notify startAt "pomodoro" "🍅" nil)
    (notify breakAt "relax" "🙏" (setq pomodori (+ 1 pomodori)))
    (notify resumeAt "resume" "💪" (when (= 4 pomodori) (setq pomodori 0)))))
