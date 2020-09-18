alias p="sudo pacman"
alias y="yay"
alias yn="yay --noconfirm"
alias update="yay -Syua"
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

function gcmz() {
    git merge $(git branch | fzf --height 40% --ansi --multi --tac)
}

function gcof() {
    git checkout $(git branch | fzf --height 40% --ansi --multi --tac)
}

function gcpz() {
    git cherry-pick $(git log --pretty=format:'%h %s' | fzf --height 40% --ansi --multi | cut -d' ' -f1)
}

function gcoz() {
    git branch -a -vv --color=always | grep -v '/HEAD\s' |
        fzf --height 40% --ansi --multi --tac | sed 's/^..//' | awk '{print $1}' |
        sed 's#^remotes/[^/]*/##'
}

alias fin="vim ~/Documents/Finances/ledger/ledger.dat"
alias efin="vim ~/Documents/Finances/ledger/enko.dat"
alias le="ledger --no-pager -f ~/Documents/Finances/ledger/ledger.dat"

alias sydo="rsync -avzP --delete ~/Documents/ admin@thekzm.myqnapcloud.com:/share/Documents -e \"ssh -p 2201\""
alias sydi="rsync -avzP admin@thekzm.myqnapcloud.com:/share/Documents/ ~/Documents -e \"ssh -p 2201\""

alias cal="cal -m"
alias bm="bashmount"
