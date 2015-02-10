-module(stock).
-compile(export_all).

-include("common_lib.erl").
-include("common_test.erl").

-record(stock, {symbol, factor_loadings, market_events, excess_return, specific_return}).

apply_market_event(Stock, MarketEvent) -> 
  ProcessedStock = calculate_specific_return(Stock, MarketEvent),
  track_market_event(ProcessedStock, MarketEvent).

track_market_event(Stock, MarketEvent) -> 
  #stock{ 
    symbol = Stock#stock.symbol,
    factor_loadings = Stock#stock.factor_loadings,
    market_events = Stock#stock.market_events ++ [MarketEvent],
    excess_return = Stock#stock.excess_return,
    specific_return = Stock#stock.specific_return
  }.

calculate_specific_return(Stock, _) when length(Stock#stock.market_events) =:= 0 -> 
  #stock{ 
    symbol = Stock#stock.symbol,
    factor_loadings = Stock#stock.factor_loadings,
    market_events = Stock#stock.market_events,
    excess_return = Stock#stock.excess_return,
    specific_return = 0.0 
  };

calculate_specific_return(Stock, CurrentMarketEvent) when length(Stock#stock.market_events) > 0 ->
  LastMarketEvent = lists:last(Stock#stock.market_events), 
  SpecificReturn = CurrentMarketEvent#market_event.close - LastMarketEvent#market_event.close,
  #stock{ 
    symbol = Stock#stock.symbol,
    factor_loadings = Stock#stock.factor_loadings,
    market_events = Stock#stock.market_events,
    excess_return = Stock#stock.excess_return,
    specific_return = SpecificReturn 
  }.


% Tests
should_track_a_market_event_as_it_is_are_applied_test() ->
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(Stock, MarketEvent),
  ?assert(length(ProcessedStock#stock.market_events) =:= 1).


should_track_market_events_as_they_are_applied_test() ->
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent1 = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  MarketEvent2 = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-25"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(apply_market_event(Stock, MarketEvent1), MarketEvent2),
  ?assert(length(ProcessedStock#stock.market_events) =:= 2).


should_have_a_zero_specific_return_when_applying_a_single_market_event_test() -> 
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(Stock, MarketEvent),
  ?assert(ProcessedStock#stock.specific_return =:= 0.0).


should_have_a_specific_return_of_twenty_when_applying_multiple_events_test() ->
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent1 = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=20.0, vol=100111, adjclose=40.0},
  MarketEvent2 = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-25"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(apply_market_event(Stock, MarketEvent1), MarketEvent2),
  ?assert(ProcessedStock#stock.specific_return =:= 20.0).



