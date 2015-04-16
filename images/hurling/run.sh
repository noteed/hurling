#! /bin/bash

DATA=/var/lib/postgresql/9.1/main

sudo -u postgres \
  /usr/lib/postgresql/9.1/bin/postgres \
    -c data_directory=$DATA \
    -c config_file=/etc/postgresql/9.1/main/postgresql.conf &

sleep 5 # TODO Use pgready

hurling --help

DB="dbname=docker user=docker password=docker host=127.0.0.1"
humming create --database-url "$DB"
humming enqueue --database-url "$DB" --queue hurling --method play --arguments '{}'
hurling work --database-url "$DB" --once
