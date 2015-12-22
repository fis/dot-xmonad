#! /bin/bash

cmd="$(dmenu_path | dmenu -l 16)"
if [[ $? != 0 ]]; then exit 1; fi
if [[ "$cmd" =~ ^[a-z0-9/_-]*$ ]]; then exec "$cmd"; exit 1; fi
"$cmd"
