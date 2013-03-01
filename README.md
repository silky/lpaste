## Database setup

    $ sudo su postgres --command 'createuser hpaste -P'
    $ sudo su postgres --command 'createdb hpaste -O hpaste'
    $ cat schema.sql | psql -U hpaste -h 127.0.0.1 -d hpaste
    $ cat data.sql | psql -U hpaste -h 127.0.0.1 -d hpaste

## Configuration & Running

    $ cp amelie.conf.sample amelie.conf

Edit amelie.conf.
 
    $ dist/build/amelie/amelie amelie.conf
