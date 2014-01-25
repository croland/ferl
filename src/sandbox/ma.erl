-module(ma).
-export([run/1]).

-record(tick, {date, open, high, low, close, vol, adjclose}).
-record(account, {balance, name, transactions=[]}).

run(File) ->
	Ticks = parse(File),
	Trades = calculate(Ticks, equal, []),
	profitAndLoss(Trades, 1000),
	Trades.

parse(File) ->
	{ok, Binary} = file:read_file(lists:concat(["../../data/", File, ".csv"])),
	Lines = string:tokens(erlang:binary_to_list(Binary), "\r\n"),
	Ticks = lists:map(fun(L) -> parse_line(L) end, Lines),
	Ticks.

parse_line(Line) ->
	[Date, Open, High, Low, Close, Volume, AdjClose] = re:split(Line, "[,]", [{return, list}]),
	Tick = #tick{date=iolist_to_binary(Date), open=list_to_float(Open), high=list_to_float(High), low=list_to_float(Low), close=list_to_float(Close), vol=list_to_integer(Volume), adjclose=list_to_float(AdjClose)},
	Tick.

calculate([H|T], Trend, Trades) -> 
	TicksMa1Day = lists:sublist(T, 9),	
	TicksMa2Day = lists:sublist(T, 18),	

	AvgMa1Day = avgTicks(TicksMa1Day, 0) / getLength(TicksMa1Day),
	AvgMa2Day = avgTicks(TicksMa2Day, 0) / getLength(TicksMa2Day),

	AvgDirection = avgDirection(AvgMa1Day, AvgMa2Day),
	TradeAction = tradeAction(Trend, AvgDirection),

	NewTrades = trade(H, TradeAction) ++ Trades,

	calculate(T, AvgDirection, NewTrades); 

calculate([], Trend, Trades) -> Trades.

avgDirection(Avg1Day, Avg2Day) when Avg1Day > Avg2Day -> above;
avgDirection(Avg1Day, Avg2Day) when Avg1Day < Avg2Day -> below;
avgDirection(Avg1Day, Avg2Day) when Avg1Day =:= Avg2Day -> equal.

tradeAction(Trend, TickTrend) when Trend =:= TickTrend -> hold; 
tradeAction(Trend, TickTrend) when Trend =:= below, TickTrend =:= equal -> hold;
tradeAction(Trend, TickTrend) when Trend =:= above, TickTrend =:= equal -> hold;
tradeAction(Trend, TickTrend) when Trend =:= equal, TickTrend =:= above -> buy;
tradeAction(Trend, TickTrend) when Trend =:= below, TickTrend =:= above -> buy;
tradeAction(Trend, TickTrend) when Trend =:= equal, TickTrend =:= below -> sell;
tradeAction(Trend, TickTrend) when Trend =:= above, TickTrend =:= below -> sell.

trade(Trade, TradeAction) when TradeAction =:= buy -> 
	[{TradeAction, Trade#tick.date, Trade#tick.close}];
trade(Trade, TradeAction) when TradeAction =:= sell -> 
	[{TradeAction, Trade#tick.date, Trade#tick.close}];
trade(Trade, TradeAction) when TradeAction =:= hold -> []. 

getLength([H|T]) -> 1 + length(T);
getLength([]) -> 1.

avgTicks([H|T], Acc) -> addTicks(T, H#tick.close + Acc);
avgTicks([], Acc) -> Acc.

addTicks([H|T], Acc) -> addTicks(T, H#tick.close + Acc);
addTicks([], Acc) -> Acc.

profitAndLoss([H|T], Total) -> 
	[PrevTrade|Tail] = T,
	NewTotal = getTotal(H, PrevTrade, Total),
	io:format("~f~n", [NewTotal]),
	profitAndLoss(T, NewTotal);
profitAndLoss([], Total) -> Total.

getTotal(Trade, PrevTrade, Total) -> 
	{PrevAction, PrevDate, PrevClose} = PrevTrade,
	{Action, Date, Close} = Trade,
	(Total + (Close - PrevClose)).
	%% case Trade of 
	%%	{buy, Date, Close} -> Total - PrevClose - Close;
	%%		{sell, Date, Close} -> Total + PrevClose - Close;
	%%		_Else -> Total 
	%end.


