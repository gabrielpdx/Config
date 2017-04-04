(setq safe-local-variable-values (quote ((rsync-qk2 . t))))
(setq default-line-height 100)
(defvar rsync-qk2 nil)
(add-hook 'after-save-hook
          (lambda () (when rsync-qk2 (start-process "qrs"
                                                    "qrs"
                                                    "rsync"
                                                    "-rltv"
                                                    "--delete"
                                                    "--exclude=.hg"
                                                    "--exclude=out"
                                                    "--exclude=target"
                                                    "--exclude=build"
                                                    "--exclude=node_modules"
                                                    "/home/gabe/Projects/qk2/"
                                                    "/nfs/rkgd/qk2/"))))
