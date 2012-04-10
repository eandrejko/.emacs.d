(setenv "AWS_ACCESS"
        (shell-command-to-string "source $HOME/.bashrc && printf $AWS_ACCESS"))
(setenv "AWS_SECRET"
        (shell-command-to-string "source $HOME/.bashrc && printf $AWS_SECRET"))

(provide 'hacks)
