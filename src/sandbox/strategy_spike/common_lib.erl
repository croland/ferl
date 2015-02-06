
-record(market_event, {symbol, date, open, high, low, close, vol, adjclose}).

parse_line_to_market_event(Line) ->
	[Symbol, Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
  MarketEvent = #market_event
  {
    symbol=Symbol,
    date=iolist_to_binary(Date), 
    open=list_to_float(Open), 
    high=list_to_float(High), 
    low=list_to_float(Low), 
    close=list_to_float(Close), 
    vol=list_to_integer(Volume), 
    adjclose=list_to_float(AdjClose)
  },
  MarketEvent.

