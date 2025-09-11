#!/bin/bash

RED='\033[1;91m'
GREEN='\033[1;92m'
BLUE='\033[1;94m'
NC='\033[0m'


tryit(){
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo -e "${RED}Error occurred with $1 ${NC}" >&2
        exit $status
    fi
    return $status
}


# run donjon as the default entrypoint, as donjon can run dragon
cd Donjon
tryit ./rdonjon $@
