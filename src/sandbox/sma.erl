%% Copyright: Christopher Roland 2011
%% 
%% Description: Test file to test run a simple moving average over imported Yahoo data 
%% ---- Usage ----
%% -- Setup database:
%% sma:install() to create mnesia schema 
%% sma:create_table() to create table
%% -- Then import data:
%% sma_test:start_link() to start the gen_server
%% sma_test:run() to kick it off

-module(sma).
-behaviour(gen_server).
-include("c:/Program\ Files\ (x86)/erl5.10/lib/stdlib-1.19/include/qlc.hrl").
-include("c:/Program\ Files\ (x86)/erl5.10/lib/eunit-2.2.4/include/eunit.hrl").

-export([import_line/1]).
-export([init/1, calc/1, install/0, create_table/0, run/0, getall/0, start_link/0, handle_call/3, terminate/2]).

-record(tick, {tick_date, tick_open, tick_high, tick_low, tick_close, tick_vol, tick_adjclose}).

start_link() -> gen_server:start_link({local, sma}, sma, [], []).	

run() -> {ok, Binary} = file:read_file("../../data/msft.csv"),
	Lines = string:tokens(erlang:binary_to_list(Binary), "\r\n"),
	lists:map(fun(L) -> import_line(L) end, Lines).

init([]) -> 
	mnesia:start(),
	{ok, []}.

install() ->
	ok = mnesia:create_schema([node()]),
	{ok, []}.

create_table() ->
	mnesia:create_table(tick, 
		[{attributes, record_info(fields, tick)}]),
	{ok, []}.

getall() ->
	Fun = fun() -> qlc:e(qlc:q([ X || X <- mnesia:table(tick), list_to_float(X#tick.tick_open) > 90 ])) end,
	{atomic, Results} = mnesia:transaction(Fun),
	Results.

calc([H|T]) ->
	io:fwrite("~w~n", [H]),
	calc(T);

calc([]) -> ok.

import_line(Line) -> gen_server:call(sma, {import_line, Line}).	

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
	Tick = #tick{tick_date=Date, tick_open=Open, tick_high=High, tick_low=Low, tick_close=Close, tick_vol=Volume, tick_adjclose=AdjClose},
	F = fun() -> mnesia:write(Tick) end,
	mnesia:transaction(F),
	{reply, Line, Db}.

terminate(_Reason, State) -> 
	{ok, State}.

some_test() ->
	?assert(length([1,2,3]) =:= 3).
