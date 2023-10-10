#!/bin/bash

PARACHUTEPREFIX=/opt/parachute
BINDIR=$PARACHUTEPREFIX/bin
LIBDIR=$PARACHUTEPREFIX/lib
INCDIR=$PARACHUTEPREFIX/include

sudo mkdir -p $PARACHUTEPREFIX
sudo mkdir -p $BINDIR
sudo mkdir -p $LIBDIR
sudo mkdir -p $INCDIR

if [ -f tma-linux/target/linux/bin/tmasm ]; then
	sudo cp tma-linux/target/linux/bin/tmasm $BINDIR
	sudo cp tma-linux/target/linux/lib/* $LIBDIR
	sudo cp -r tma-includes/target/classes/include/* $INCDIR
fi

sudo chmod 755 $BINDIR/tmasm

