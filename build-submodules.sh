#!/bin/sh

for submodule in basics transaction consensus; do
    sbt "project $submodule" "clean" "publishLocal"
done
