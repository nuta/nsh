# Convert $COMP_WORDS into an array.
OLD_IFS="$IFS"
IFS="#"
COMP_WORDS=($COMP_WORDS)
IFS="$OLD_IFS"

CMD="${COMP_WORDS[0]}"

# Ubuntu
if [[ -r  /etc/profile.d/bash_completion.sh ]]; then
    .  /etc/profile.d/bash_completion.sh
fi

# macOS (Homebrew)
if [[ -r /usr/local/opt/bash-completion@2/share/bash-completion/bash_completion ]]; then
    # Load helper functions for completions.
    BASH_COMPLETION_COMPAT_DIR=/invalid-path-not-to-preload-any-completions
    . /usr/local/opt/bash-completion@2/share/bash-completion/bash_completion

    case $CMD in
    git) . /usr/local/etc/bash_completion.d/git-completion.bash ;;
    *)
        # Search bash-completion for a completion.
        COMP_DIR=/usr/local/opt/bash-completion@2/share/bash-completion/bash_completion
        if [[ -f $COMP_DIR/$CMD ]]; then
            . $COMP_DIR/$CMD
        fi
        ;;
    esac
fi

# Maybe the completion is already defined.
func="$(complete -p $CMD | awk '{ print $(NF-1) }')"
if [[ -z "$func" ]]; then
    # Try bash-completion.
    _completion_loader $CMD
    func="$(complete -p $CMD | awk '{ print $(NF-1) }')"
    if [[ -z "$func" ]]; then
        exit 1
    fi
fi

$func "${COMP_WORDS[@]}"
for line in "${COMPREPLY[@]}"; do
    echo $line
done
