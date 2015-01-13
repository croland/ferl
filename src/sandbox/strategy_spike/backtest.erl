-module(backtest).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


run(Strategy) ->
  Data = spawn(data_pipeline, process_message, []),
  Data ! {self(), get_latest_bars},
  process_message([], Strategy).

process_message(Events, Strategy) ->
  receive
    {From, Event} -> process_message(Events ++ [Event], Strategy);
    done -> send(Events)
  end.

send(Events) -> io:format("~p~n", Events).

