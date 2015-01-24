-module(strategy).
-compile(export_all).

-record(signal, {from}).
-record(moving_average, {event, mva}).
-record(market_event, {date, open, high, low, close, vol, adjclose}).

-include_lib("eunit/include/eunit.hrl").

process_message(Events, MVAEvents) ->
  receive
    {From, Event} -> 
      ProcessedEvents = process_events(Events, MVAEvents),
      process_message(Events ++ [Event], ProcessedEvents)
  end,
  ok.

process_events(MarketEvents, MVAEvents) ->
  ProcessedEvents = [calculate_15_day_moving_average(MarketEvents, MVAEvents, MarketEvent) || MarketEvent <- MarketEvents],
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
calculate_15_day_moving_average(MarketEvents, MVAEvents, MarketEvent) when length(MarketEvents) < 15 ->
  [0];

calculate_15_day_moving_average(MarketEvents, MVAEvents, MarketEvent) when length(MarketEvents) > 15 ->
  [0].

% Tests
should_calculate_zero_for_15_day_mva_with_less_than_15_days_of_market_events_test() ->
  [MarketEvent | ReversedEvents] = lists:sublist(get_15_days_of_market_events(), 5),
  MarketEvents = lists:reverse(ReversedEvents),
  MVAEvents = calculate_15_day_moving_average(MarketEvents, [], MarketEvent),
  ?assert(length(MVAEvents) =:= 1).

should_calculate_15_day_mva_15_days_of_market_events_test() ->
  [MarketEvent | ReversedEvents] = lists:reverse(get_15_days_of_market_events()),
  MarketEvents = lists:reverse(ReversedEvents),
  MVAEvents = calculate_15_day_moving_average(MarketEvents, [], MarketEvent),
  ?assert(length(MVAEvents) =:= 1).

should_generate_moving_average_signal_test() ->
  MarketEvents = get_15_days_of_market_events(),
  From = self(),
  Signals = process_events(MarketEvents, []),
  ?assert(length(Signals) =:= 1).

get_15_days_of_market_events() ->
  Lines = ["2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27",
    "2011-10-14,27.31,27.50,27.02,27.27,50947700,27.27"],
  MarketEvents = [parse_line_to_market_event(Line) || Line <- Lines],
  MarketEvents.

