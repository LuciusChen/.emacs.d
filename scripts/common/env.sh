#!/bin/zsh
# Generate the allowlisted shell environment used by macOS GUI/daemon Emacs.
# Run this script after changing .zshenv, .zprofile, or .zshrc.
set -euo pipefail

script_dir=${0:A:h}
emacs_dir=${script_dir:h:h}
output_file="${emacs_dir}/.env.local"
user_home=$HOME
user_name=$(/usr/bin/id -un)

# Keep this list deliberately narrow.  In particular, do not persist session
# identifiers, sockets, terminal metadata, or credentials.
allowed_vars=(
  ANDROID_HOME
  ANDROID_SDK_ROOT
  CARGO_HOME
  CPATH
  CPPFLAGS
  GEM_HOME
  GEM_PATH
  GOBIN
  GOPATH
  GRADLE_HOME
  HOMEBREW_CELLAR
  HOMEBREW_PREFIX
  HOMEBREW_REPOSITORY
  INFOPATH
  JAVA_HOME
  LANG
  LDFLAGS
  LIBRARY_PATH
  MANPATH
  MAVEN_HOME
  NVM_BIN
  NVM_DIR
  NVM_INC
  OBJC_DISABLE_INITIALIZE_FORK_SAFETY
  PATH
  PKG_CONFIG_PATH
  PNPM_HOME
  PYENV_ROOT
  RBENV_ROOT
  RUSTUP_HOME
  SDKMAN_DIR
  SHELL
  TOMCAT_HOME
)

filter_environment() {
  local line key value entry
  local -a path_entries deduped_path
  local -A seen_path

  while IFS= read -r line; do
    key=${line%%=*}
    if (( ${allowed_vars[(Ie)$key]} )) ||
       [[ $key == LC_* || $key == *_VM_OPTIONS ]]; then
      if [[ $key == PATH ]]; then
        value=${line#*=}
        path_entries=( ${(s/:/)value} )
        for entry in $path_entries; do
          if [[ -n $entry && -z ${seen_path[$entry]-} ]]; then
            seen_path[$entry]=1
            deduped_path+=( "$entry" )
          fi
        done
        print -r -- "PATH=${(j/:/)deduped_path}"
      else
        print -r -- "$line"
      fi
    fi
  done
}

temporary_file=$(/usr/bin/mktemp "${output_file}.XXXXXX")
cleanup() {
  [[ ! -e $temporary_file ]] || /bin/rm -f -- "$temporary_file"
}
trap cleanup EXIT INT TERM

# Start from a clean environment so the snapshot cannot inherit Kitty, Codex,
# direnv, SSH-agent, or other state from whichever terminal invokes the script.
/usr/bin/env -i \
  HOME="$user_home" \
  USER="$user_name" \
  LOGNAME="$user_name" \
  SHELL=/bin/zsh \
  LANG="${LANG:-en_US.UTF-8}" \
  TERM=xterm-256color \
  PATH=/usr/bin:/bin:/usr/sbin:/sbin \
  /bin/zsh -lic /usr/bin/env </dev/null |
  filter_environment |
  LC_ALL=C /usr/bin/sort -u >"$temporary_file"

/bin/mv -f -- "$temporary_file" "$output_file"
trap - EXIT INT TERM

variable_count=$(/usr/bin/wc -l <"$output_file" | /usr/bin/tr -d ' ')
print -r -- "Wrote ${variable_count} variables to ${output_file}"
