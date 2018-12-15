# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
source $OMF_PATH/init.fish

# emacs ansi-term support
if test -n "$EMACS"
  set -x TERM eterm-color
end

# this function may be required
function fish_title
  true
end


set -gx PATH /home/larionov/Software/flutter/bin $PATH

nvm use 10

alias config='/usr/bin/git --git-dir=/home/larionov/.cfg/ --work-tree=/home/larionov'