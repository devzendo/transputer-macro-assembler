#!/bin/bash

PARACHUTEPREFIX=/opt/parachute
BINDIR=$PARACHUTEPREFIX/bin
LIBDIR=$PARACHUTEPREFIX/lib
INCDIR=$PARACHUTEPREFIX/include

sudo mkdir -p $PARACHUTEPREFIX
sudo mkdir -p $BINDIR
sudo mkdir -p $LIBDIR
sudo mkdir -p $INCDIR

if [ -f tma-osx/target/macosx/bin/tmasm ]; then
	sudo cp tma-osx/target/macosx/bin/tmasm $BINDIR
	sudo cp tma-osx/target/macosx/lib/* $LIBDIR
	sudo cp -r tma-includes/target/classes/include/* $INCDIR
fi

chmod 755 $BINDIR/tmasm

