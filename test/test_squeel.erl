-module(test_squeel).
-include_lib("eunit/include/eunit.hrl").

%% TODO: fetch from env
-define(host, "localhost").
-define(port, 10432).
-define(user, "squeel_test").
-define(database, "squeel_test_db").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec_select_test() ->
  void.

exec_insert_test() ->
  with_connection(
    fun(C) ->
        {ok, 1} = squeel:exec(C, "insert into test_table (id, value) values (3, $1)", ["three"])
    end).

exec_insert_returning_test() ->
  void.

exec_update_test() ->
  void.

exec_delete_test() ->
  void.

exec_error_test() ->
  void.

exec_transaction_test() ->
  void.

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
