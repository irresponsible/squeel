echo "CREATE DATABASE ${USER};" | psql -h 127.0.0.1 -p 10432 template1
psql -h 127.0.0.1 -p 10432 template1 < test/data/test_schema.sql
