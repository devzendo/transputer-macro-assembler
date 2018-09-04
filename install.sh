#!/bin/bash

PARACHUTEPREFIX=/opt/parachute
BINDIR=$PARACHUTEPREFIX/bin
LIBDIR=$PARACHUTEPREFIX/lib

sudo mkdir -p $PARACHUTEPREFIX
sudo mkdir -p $BINDIR
sudo mkdir -p $LIBDIR

if [ -f tma-osx/target/macosx/bin/tmasm ]; then
	sudo cp tma-osx/target/macosx/bin/tmasm $BINDIR
	sudo cp tma-osx/target/macosx/lib/* $LIBDIR
fi

chmod 755 $BINDIR/tmasm

