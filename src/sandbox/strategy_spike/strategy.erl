-module(strategy).
-compile(export_all).

-record(signal, {from}).
-record(moving_average, {event, mva}).
-record(market_event, {symbol, date, open, high, low, close, vol, adjclose}).

-include_lib("eunit/include/eunit.hrl").

process_message(Events, MVAEvents) ->
  receive
    {From, Event} -> 
      ProcessedEvents = process_events(Events, MVAEvents),
      process_message(Events ++ [Event], ProcessedEvents)
  end,
  ok.

process_events(MarketEvents, MVAEvents) ->
  ProcessedEvents = [calculate_10_day_sma(MarketEvents, MVAEvents, MarketEvent) || MarketEvent <- MarketEvents],
  ProcessedEvents.

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

% Calculations that should be moved into a different module
calculate_10_day_sma(MarketEvents, MVAEvents, MarketEvent) when length(MarketEvents) < 10 ->
  {MarketEvent, 0};

calculate_10_day_sma(MarketEvents, MVAEvents, MarketEvent) when length(MarketEvents) > 10 ->
  [0].

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
  MVAEvents = calculate_10_day_sma(MarketEvents, [], MarketEvent),
  ?assert(length(MVAEvents) =:= 1).

should_calculate_10_day_mva_10_days_of_market_events_test() ->
  [MarketEvent | ReversedEvents] = lists:reverse(get_10_days_of_market_events()),
  MarketEvents = lists:reverse(ReversedEvents),
  MVAEvents = calculate_10_day_sma(MarketEvents, [], MarketEvent),
  ?assert(length(MVAEvents) =:= 1).

should_generate_moving_average_signal_test() ->
  MarketEvents = get_10_days_of_market_events(),
  From = self(),
  Signals = process_events(MarketEvents, []),
  ?assert(length(Signals) =:= 1).

get_10_days_of_market_events() ->
  Lines = ["LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "LOL,2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27"],
  MarketEvents = [parse_line_to_market_event(Line) || Line <- Lines],
  MarketEvents.

