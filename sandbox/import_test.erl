-module(import_test).
-behaviour(gen_server).

-export([init/1, run/0, start_link/0, handle_call/3, terminate/2]).
-export([import_line/1]).

init([]) ->
	application:start(sasl),
	application:start(ibrowse),
	%application:start(couchbeam),
	couchbeam:start(),
	Connection = couchbeam:server_connection(),
	{ok, Db} = couchbeam:open_or_create_db(Connection, "test2", []),
	{ok, Db}.

start_link() ->
	gen_server:start_link({local, import_test}, import_test, [], []).	

run() ->
	{ok, Binary} = file:read_file("msft.csv"),
	Lines = string:tokens(erlang:binary_to_list(Binary), "\n"),
	import_lines(Lines).

import_lines([H|T]) ->
	import_line(H),
	import_lines(T);

import_lines([]) -> done.

import_line(Line) ->
	gen_server:call(import_test, {import_line, Line}).	

handle_call({import_line, Line}, _From, Db) ->
	[Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
	Doc = {[
		{<<"Date">>, Date}, 
		{<<"Open">>, Open}
	]},
	{ok, DocResult} = couchbeam:save_doc(Db, Doc),
	{reply, Line, Db}.

terminate(_Reason, State) -> {ok, State}.
