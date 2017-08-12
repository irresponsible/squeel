-module(squeel).

-export([exec/2, exec/3]).

-type proplist()   :: [{term(), term()}].
-type sql_error()  :: {error, {atom(), binary(), proplist()}}.
-type sql_result() :: {ok, proplist()}           |
                      {ok, number()}             |
                      {ok, number(), proplist()} |
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
  strip_error(Error).

strip_error({error, error, _, Reason, Message, Details}) ->
  {error, {Reason, Message, Details}}.

column_names(Columns) ->
  lists:map(fun ({column, Name, _, _, _, _}) ->
                binary_to_atom(Name, utf8)
            end, Columns).

result_to_proplist(Columns, Rows) ->
  ColNames = column_names(Columns),
  lists:map(fun(Val) ->
                lists:zip(ColNames, tuple_to_list(Val))
            end, Rows).
