alias p="sudo pacman"
alias y="yaourt"
alias yn="yaourt --noconfirm"
alias update="yaourt -Syua"
alias printer="system-config-printer"
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias mirrors="sudo pacman-mirrors -g"
alias r="ranger"
alias v="vim"
alias f="find . -name"
alias om="emacs -nw ~/org/index.org"

alias ta="task add"
alias ts="task sync"
alias tcw="task context work"
alias tch="task context home"
alias tcn="task context none"
alias taw="task add work"
alias tah="task add home"
alias tap="task add pers"

alias c="calcurse"
alias j="jrnl"
alias vw="vim ~/vimwiki/index.wiki"

alias m="neomutt"

alias tmx="tmuxinator"
alias tmxw="tmuxinator work"

alias dotfiles="cd ~/.config/dotfiles"

function gcoz() {
    git branch -a -vv --color=always | grep -v '/HEAD\s' |
        fzf --height 40% --ansi --multi --tac | sed 's/^..//' | awk '{print $1}' |
        sed 's#^remotes/[^/]*/##'
}
