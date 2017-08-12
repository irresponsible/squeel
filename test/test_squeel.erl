-module(test_squeel).
-include_lib("eunit/include/eunit.hrl").

-define(host, "localhost").
-define(port, 10432).
-define(user, "squeel_test").
-define(database, "squeel_test_db").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec_create_table_test() ->
  with_rollback(
   fun(C) ->
       {ok, []} = squeel:exec(C, "create table test_table2 (id integer primary key, value integer)")
   end).

exec_select_one_test() ->
  with_connection(
    fun(C) ->
        {ok, [Record]} = squeel:exec(C, "select * from test_table where id=1"),
        [{id, 1}, {value, <<"one">>}] = Record
    end).

exec_select_all_test() ->
  with_connection(
    fun(C) ->
        {ok, Records} = squeel:exec(C, "select * from test_table"),
        [[{id, 1}, {value, <<"one">>}],
         [{id, 2}, {value, <<"two">>}]] = Records
    end).

exec_select_count_test() ->
  with_connection(
   fun(C) ->
       {ok, [[{count, 2}]]} = squeel:exec(C, "select count(*) from test_table")
   end).

exec_select_large_query_test() ->
  with_connection(
   fun(C) ->
       {ok, [Result]} = squeel:exec(C, "select count(*) as id_count,
                                               max(id)  as id_max,
                                               min(id)  as id_min
                                          from test_table"),
       [{id_count, 2}, {id_max, 2}, {id_min, 1}] = Result
   end).

exec_insert_test() ->
  with_rollback(
    fun(C) ->
        {ok, 1} = squeel:exec(C, "insert into test_table (id, value) values (3, $1)", ["three"])
    end).

exec_insert_returning_test() ->
  with_rollback(
   fun(C) ->
       {ok, 1, [[{id, 3}]]} = squeel:exec(C, "insert into test_table (id, value)
                                                values (3, 'three') returning id")
   end).

exec_update_one_test() ->
  with_rollback(
   fun(C) ->
       {ok, 1} = squeel:exec(C, "update test_table set value='invalid' where id=2")
   end).

exec_update_multiple_test() ->
  with_rollback(
    fun(C) ->
        {ok, 2} = squeel:exec(C, "update test_table set value='invalid'")
    end).

exec_delete_one_test() ->
  with_rollback(
   fun(C) ->
       {ok, 1} = squeel:exec(C, "delete from test_table where id=2")
   end).

exec_delete_multiple_test() ->
  with_rollback(
    fun(C) ->
        {ok, 2} = squeel:exec(C, "delete from test_table where id=1 OR id=2")
    end).

exec_delete_none_test() ->
  with_rollback(
    fun(C) ->
        {ok, 0} = squeel:exec(C, "delete from test_table where id=3")
    end).

exec_error_test() ->
  with_connection(
   fun(C) ->
       {error, Error} = squeel:exec(C, "insert into test_table (id, value) values (2, 'two')"),
       {unique_violation, _, Details} = Error,
       <<"test_table">> = proplists:get_value(table_name, Details),
       <<"test_table_pkey">> = proplists:get_value(constraint_name, Details)
   end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

with_connection(F) ->
  with_connection(F, []).

with_connection(F, Args) ->
  Args2 = [{port, ?port}, {database, ?database} | Args],
  {ok, C} = epgsql:connect(?host, ?user, Args2),
  try
    F(C)
  after
    epgsql:close(C)
  end.

with_rollback(F) ->
  with_connection(
   fun(C) ->
       try
         epgsql:squery(C, "begin"),
         F(C)
       after
         epgsql:squery(C, "rollback")
       end
   end).
