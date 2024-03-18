#!/bin/bash -e

pattern="${1:?requires regex pattern as first argument}"

asl_dir="$(asli --aarch64-dir)"
output="$(echo ":coverage A64 $pattern" | asli)"

echo "$output" | sed "s#$asl_dir#.#g"
