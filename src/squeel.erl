-module(squeel).

-export([exec/2, exec/3]).

-type sql_error()  :: {error, {atom(), binary(), map()}}.
-type sql_result() :: {ok, [map()]}           |
                      {ok, number()}          |
                      {ok, number(), [map()]} |
                      sql_error().

-spec exec(pid(), string()) -> sql_result().

exec(Conn, Stmt) -> exec(Conn, Stmt, []).

-spec exec(pid(), string(), [term()]) -> sql_result().

exec(Conn, Stmt, Params) ->
  RawResult = epgsql:equery(Conn, Stmt, Params),
  parse_raw_result(RawResult).

parse_raw_result({ok, Columns, Rows}) ->
  {ok, result_to_proplist(Columns, Rows)};
parse_raw_result({ok, Count}) ->
  {ok, Count};
parse_raw_result({ok, Count, Columns, Rows}) ->
  {ok, Count, result_to_proplist(Columns, Rows)};
parse_raw_result({error, Error}) ->
  handle_sql_error(Error).

handle_sql_error({error, error, _, Reason, Message, Details}) ->
  {error, {Reason, Message, proplist_to_map(Details)}}.

column_names(Columns) ->
  lists:map(fun ({column, Name, _, _, _, _}) ->
                binary_to_list(Name)
            end, Columns).

result_to_proplist(Columns, Rows) ->
  ColNames = column_names(Columns),
  lists:map(fun(Val) ->
                maps:from_list(lists:zip(ColNames, tuple_to_list(Val)))
            end, Rows).

proplist_to_map(List) ->
  lists:foldl(fun({Key, Val}, Acc) ->
                  maps:put(atom_to_list(Key), Val, Acc)
              end, #{}, List).
