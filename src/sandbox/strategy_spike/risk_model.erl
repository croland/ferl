-module(risk_model).
-compile(export_all).

-record(asset, {symbol, factor1, factor2, factor3}).
-record(factor, {name, exposure}).

-include_lib("eunit/include/eunit.hrl").


run() -> process_message([]).

process_message(Events) ->
  receive
    {From, Event} -> process_message(Events ++ [Event]) 
  end.

% Tests


should_determine_factor_loading_from_asset_factor_and_market_event_test() -> ok.

should_determine_factor_return_for_asset_factor_at_point_in_time_test() -> ok.



