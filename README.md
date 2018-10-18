# big-data
Big Data

# Upload to BigQuery

bq load --replace --source_format=CSV --autodetect audiometry_tests.equipment_success ./notebooks/equipment_success.csv


# setup local db

Launch local postgres from docker, from the **root folder**:

~~~bash
docker-compose up
~~~

Download a dump from heroku

~~~bash
heroku pg:backups:download  --app audyx
~~~


Restore the dump in the local db

~~~bash
pg_restore --verbose --clean --no-acl --no-owner  -h localhost -p 5433 -d audyx --user me latest.dump
~~~
