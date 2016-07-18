#!/bin/sh
set -ex

STACK_ROOT=~/.stack-split
STACK_WORK=.stack-split
DIST_DIR=dist
BIN=$DIST_DIR/fhue


TAG=$(git describe)
mkdir -p dist

stack --install-ghc --stack-root $STACK_ROOT --work-dir $STACK_WORK --local-bin-path $DIST_DIR install --force-dirty --split-objs
upx -9 $BIN

github-release upload -u madjar -r fhue -t $TAG -n fhue-mac -f $BIN
