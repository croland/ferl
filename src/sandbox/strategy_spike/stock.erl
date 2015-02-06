-module(stock).
-compile(export_all).

-include("common_lib.erl").
-include("common_test.erl").

-record(stock, {symbol, factor_loadings, market_events, excess_return, specific_return}).

apply_market_event(Stock, MarketEvent) -> 
  ProcessedStock = calculate_specific_return(Stock, MarketEvent, MarketEvent),
  track_market_event(Stock, MarketEvent).

track_market_event(Stock, MarketEvent) -> 
  MarketEvents = 
  #stock{ 
    symbol = Stock#stock.symbol,
    factor_loadings = Stock#stock.factor_loadings,
    market_events = Stock#stock.market_events ++ [MarketEvent],
    excess_return = Stock#stock.excess_return,
    specific_return = Stock#stock.specific_return
  }.

calculate_specific_return(Stock, CurrentMarketEvent, LastMarketEvent) -> 
  Stock.


% Tests
should_track_market_events_as_they_are_applied_test() ->
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(Stock, MarketEvent),
  ?assert(length(ProcessedStock#stock.market_events) =:= 1).

specific_should_be_0__when_applying_a_single_market_event_test() -> 
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(Stock, MarketEvent),
  ?assert(ProcessedStock#stock.specific_return =:= 0.0).

should_have_a_zero_specific_return_when_applying_a_single_market_event_test() -> 
  Stock = #stock{symbol="LOL", factor_loadings=[], market_events=[], excess_return=0.0, specific_return=0.0},
  MarketEvent = #market_event{symbol="LOL", date=iolist_to_binary("2015-01-24"), open=20.0, high=40.0, low=10.0, close=40.0, vol=100111, adjclose=40.0},
  ProcessedStock = apply_market_event(Stock, MarketEvent),
  ?assert(ProcessedStock#stock.specific_return =:= 0.0).

