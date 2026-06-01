#!/bin/sh
printf '\033c\033]0;%s\a' JuicyVS
base_path="$(dirname "$(realpath "$0")")"
"$base_path/JuicyVS.x86_64" "$@"
