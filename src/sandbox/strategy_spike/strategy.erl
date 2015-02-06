-module(strategy).
-compile(export_all).

-record(signal, {from}).
-record(moving_average, {event, mva}).

-include("common_lib.erl").
-include("common_test.erl").

process_message(Events, SmaEvents) ->
  receive
    {From, Event} -> 
      ProcessedEvents = process_events(Events, SmaEvents),
      process_message(Events ++ [Event], ProcessedEvents)
  end,
  ok.

process_events(MarketEvents, MVAEvents) ->
  ProcessedEvents = [calculate_10_day_sma(MarketEvents, MarketEvent) || MarketEvent <- MarketEvents],
  ProcessedEvents.

% Calculations that should be moved into a different module
calculate_10_day_sma(MarketEvents, MarketEvent) when length(MarketEvents) < 10 ->
  {MarketEvent, 0};

calculate_10_day_sma(MarketEvents, MarketEvent) when length(MarketEvents) >= 10 ->
  [0].

calculate_sma(MarketEvents) -> ok.

calculate_market_event_diff(PreviousEvent, CurrentEvent) ->
  CurrentEvent#market_event.adjclose - PreviousEvent#market_event.adjclose.

% Tests
calculate_market_event_diff_test() ->
  PreviousEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=30.0, vol=100111, adjclose=30.0},
  CurrentEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  Diff = calculate_market_event_diff(PreviousEvent, CurrentEvent),
  ?assert(Diff =:= 10.0).

should_calculate_zero_mva_for_all_events_when_there_are_less_than_10_days_total_of_market_events_test_for_asset() ->
  [MarketEvent | ReversedEvents] = lists:sublist(get_10_days_of_market_events(), 5),
  MarketEvents = lists:reverse(ReversedEvents),
  MVAEvents = calculate_10_day_sma(MarketEvents, MarketEvent),
  ?assert(length(MVAEvents) =:= 1).

should_calculate_10_day_mva_10_days_of_market_events_test() ->
  [MarketEvent | ReversedEvents] = lists:reverse(get_10_days_of_market_events()),
  MarketEvents = lists:reverse(ReversedEvents),
  MVAEvents = calculate_10_day_sma(MarketEvents, MarketEvent),
  ?assert(length(MVAEvents) =:= 1).

should_generate_moving_average_signal_test() ->
  MarketEvents = get_10_days_of_market_events(),
  From = self(),
  Signals = process_events(MarketEvents, []),
  ?assert(length(Signals) =:= 1).


