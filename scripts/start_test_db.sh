#!/bin/sh

if [ -z $(which postgres) ] ; then
    echo "Postgres not found, you may need to add /usr/lib/postgresql/<version>/bin to PATH."
    exit 1
fi

postgres -D datadir/ -p 10432 -k `pwd`/datadir/ &
# Sleep for a bit and wait for the DB to start
sleep 1
