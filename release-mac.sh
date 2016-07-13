#!/bin/sh
set -ex

TAG=$(git describe)
BIN=$(stack path --local-install-root)/bin/fhue

stack build --force-dirty
upx -9 $BIN

github-release upload -u madjar -r fhue -t $TAG -n fhue-mac -f $BIN
