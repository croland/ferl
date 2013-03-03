%% Copyright: Christopher Roland 2011
%% 
%% Description: Test file to determine how to import Yahoo finance data into CouchDB
%% Usage:
%% Call import_test:start_link() to start the gen_server
%% Call import_test:run() to kick everything off

-module(import_test).
-behaviour(gen_server).
-export([import_line/1]).
-export([init/0, run/0, start_link/0, handle_call/3, terminate/2]).

init() -> 
	application:start(sasl),
	application:start(ibrowse),
	application:start(couchbeam),
	Connection = couchbeam:server_connection("localhost", 5984, "", []),
	{ok, Db} = couchbeam:open_or_create_db(Connection, "test2", []),
	{ok, Db}.

start_link() -> gen_server:start_link({local, import_test}, import_test, [], []).	

run() -> {ok, Binary} = file:read_file("../data/msft.csv"),
	Lines = string:tokens(erlang:binary_to_list(Binary), "\n"),
	lists:map(fun(L) -> import_line(L) end, Lines).

import_line(Line) -> gen_server:call(import_test, {import_line, Line}).	

handle_call({import_line, Line}, _From, Db) ->
	[Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
	Doc = {[
		{<<"Date">>, iolist_to_binary(Date)}, 
		{<<"Open">>, list_to_float(Open)},
		{<<"High">>, list_to_float(High)},
		{<<"Low">>, list_to_float(Low)},
		{<<"Close">>, list_to_float(Close)},
		{<<"Volume">>, list_to_integer(Volume)},
		{<<"AdjClose">>, list_to_float(AdjClose)}
	]},
	{ok, DocResult} = couchbeam:save_doc(Db, Doc),
	{reply, Line, Db}.

terminate(_Reason, State) -> {ok, State}.
