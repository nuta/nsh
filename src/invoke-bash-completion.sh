# Ubuntu
if [[ -r  /etc/profile.d/bash_completion.sh ]]; then
    .  /etc/profile.d/bash_completion.sh
fi

# macOS (Homebrew)
if [[ -r /usr/local/opt/bash-completion@2/share/bash-completion/bash_completion ]]; then
    . /usr/local/opt/bash-completion@2/share/bash-completion/bash_completion
fi

# Load and look for the completion function.
OLD_IFS="$IFS"
IFS="#"
COMP_WORDS=($COMP_WORDS)
IFS="$OLD_IFS"

CMD="${COMP_WORDS[0]}"
_completion_loader $CMD
func="$(complete -p $CMD | awk '{ print $(NF-1) }')"

if [[ -z "$func" ]]; then
    exit 1
fi

$func "${COMP_WORDS[@]}"
for line in "${COMPREPLY[@]}"; do
    echo $line
done
