#!/bin/sh

if [ -z $(which initdb) ] ; then
    echo "Postgres not found, you may need to add /usr/lib/postgresql/<version>/bin to PATH."
    exit 1
fi

initdb --locale en_US.UTF-8 datadir
