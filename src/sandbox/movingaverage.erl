-module(movingaverage).
-export([run/0]).

-record(tick, {date, open, high, low, close, vol, adjclose}).

run() ->
	Ticks = parse(),
	Trades = calculate(Ticks, equal, []),
	profitAndLoss(Trades, 1000),
	Trades.

parse() ->
	{ok, Binary} = file:read_file("../../data/msft.csv"),
	Lines = string:tokens(erlang:binary_to_list(Binary), "\r\n"),
	Ticks = lists:map(fun(L) -> parse_line(L) end, Lines),
	Ticks.

parse_line(Line) ->
	[Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
	Tick = #tick{date=iolist_to_binary(Date), open=list_to_float(Open), high=list_to_float(High), low=list_to_float(Low), close=list_to_float(Close), vol=list_to_integer(Volume), adjclose=list_to_float(AdjClose)},
	Tick.

calculate([H|T], Trend, Trades) -> 
	Ticks9Day = lists:sublist(T, 9),	
	Ticks18Day = lists:sublist(T, 18),	
	Avg9Day = avgTicks(Ticks9Day, 0) / getLength(Ticks9Day),
	Avg18Day = avgTicks(Ticks18Day, 0) / getLength(Ticks18Day),
	AvgDirection = avgDirection(Avg9Day, Avg18Day),
	TradeAction = tradeAction(Trend, AvgDirection),
	NewTrades = trade(H, TradeAction) ++ Trades,
	calculate(T, AvgDirection, NewTrades); 
calculate([], Trend, Trades) -> Trades.

avgDirection(Avg9Day, Avg18Day) when Avg9Day > Avg18Day -> above;
avgDirection(Avg9Day, Avg18Day) when Avg9Day < Avg18Day -> below;
avgDirection(Avg9Day, Avg18Day) when Avg9Day =:= Avg18Day -> equal.

trade(Trade, TradeAction) when TradeAction =:= buy -> 
	[{TradeAction, Trade#tick.date, Trade#tick.close}];
trade(Trade, TradeAction) when TradeAction =:= sell -> 
	[{TradeAction, Trade#tick.date, Trade#tick.close}];
trade(Trade, TradeAction) when TradeAction =:= hold -> []. 

tradeAction(Trend, TickTrend) when Trend =:= TickTrend -> hold; 
tradeAction(Trend, TickTrend) when Trend =:= below, TickTrend =:= equal -> hold;
tradeAction(Trend, TickTrend) when Trend =:= above, TickTrend =:= equal -> hold;
tradeAction(Trend, TickTrend) when Trend =:= equal, TickTrend =:= above -> buy;
tradeAction(Trend, TickTrend) when Trend =:= below, TickTrend =:= above -> buy;
tradeAction(Trend, TickTrend) when Trend =:= equal, TickTrend =:= below -> sell;
tradeAction(Trend, TickTrend) when Trend =:= above, TickTrend =:= below -> sell.

getLength([H|T]) -> 1 + length(T);
getLength([]) -> 1.

avgTicks([H|T], Acc) -> addTicks(T, H#tick.close + Acc);
avgTicks([], Acc) -> Acc.

addTicks([H|T], Acc) -> addTicks(T, H#tick.close + Acc);
addTicks([], Acc) -> Acc.

profitAndLoss([H|T], Total) -> 
	NewTotal = getTotal(H, Total),
	io:format("~f~n", [NewTotal]),
	profitAndLoss(T, NewTotal);
profitAndLoss([], Total) -> Total.

getTotal(Trade, Total) -> 
	case Trade of 
		{buy, Date, Close} -> Total - Close;
		{sell, Date, Close} -> Total + Close;
		_Else -> Total 
	end.


