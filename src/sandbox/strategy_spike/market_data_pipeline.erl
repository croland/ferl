-module(market_data_pipeline).
-compile(export_all).
-record(market_event, {date, open, high, low, close, vol, adjclose}).

-include_lib("eunit/include/eunit.hrl").


process_message() ->
  receive
    {From, get_latest_bars} -> get_latest_bars(From) 
  end.

get_latest_bars(From) -> 
  MarketEvents = open_and_parse_file("goog-daily.csv"),
  [From ! {self(), Event} || Event <- MarketEvents],
  From ! done. 

open_and_parse_file(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Lines = string:tokens(erlang:binary_to_list(Binary), "\r\n"),
	lists:map(fun(L) -> parse_line_to_market_event(L) end, Lines).

parse_line_to_market_event(Line) ->
	[Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
  MarketEvent = #market_event{
    date=iolist_to_binary(Date), 
    open=list_to_float(Open), 
    high=list_to_float(High), 
    low=list_to_float(Low), 
    close=list_to_float(Close), 
    vol=list_to_integer(Volume), 
    adjclose=list_to_float(AdjClose)
  },
  MarketEvent.


