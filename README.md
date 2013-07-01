## Database setup

    $ sudo su postgres --command 'createuser hpaste -P'
    $ sudo su postgres --command 'createdb hpaste -O hpaste'
    $ cat sql/schema.sql | psql -U hpaste -h 127.0.0.1 -d hpaste

## Configuration & Running

    $ cp hpaste.conf.sample hpaste.conf

Edit hpaste.conf.
 
    $ dist/build/hpaste/hpaste hpaste.conf

