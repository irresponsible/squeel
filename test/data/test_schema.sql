--
-- Adapted from epgsql tests
--
create user squeel_test;

drop database if exists squeel_test_db;
create database squeel_test_db with encoding 'UTF8';
grant all on database squeel_test_db to squeel_test;

\c squeel_test_db;

create table test_table (
  id integer primary key,
  value text
);
insert into test_table (id, value) values (1, 'one');
insert into test_table (id, value) values (2, 'two');
