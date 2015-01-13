-module(backtest).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


run() ->
  Data = spawn(data_pipeline, process_message, []),
  Data ! {self(), get_latest_bars}.

process_message() ->
  receive
    {From, Event} -> io:format("~p~n", [Event])
  end.

