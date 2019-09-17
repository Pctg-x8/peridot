#!/bin/bash

function find_package_names() {
    awk '
BEGIN { inpkg=0; }
tolower($0) ~ /^\[package\]/ { inpkg=1; }
/^\[/ && tolower($0) !~ /^\[package\]/ { inpkg=0; }
inpkg==1 && $0 ~ /^[ \t]*name[ \t]*=[ \t]*/ {
    split($0, parts, "=");
    sub(/[ \t"]*$/, "", parts[2]);
    sub(/^[ \t"]+/, "", parts[2]);
    print parts[2];
}' < $1
}

function gen_manifest() {
	sed -e "s/#%KERNEL_CRATE_NAME%/$1/g" -e "s/%KERNEL_CRATE_PATH%/${2//\//\\/}/g" $3
}
