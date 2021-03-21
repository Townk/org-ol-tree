((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "cask exec buttercup -L . -L tests"
                            projectile-test-cmd-map))))))
