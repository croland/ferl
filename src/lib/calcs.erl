-module(calcs).
-export([cov/5]).

-include_lib("eunit/include/eunit.hrl").

cov([], Xtotal, Ytotal, XYtotal, Count) ->
	(XYtotal/Count)-((Xtotal/Count)*(Ytotal/Count));
cov([{X,Y}|T], Xtotal, Ytotal, XYtotal, Count) ->
	cov(T, Xtotal+X, Ytotal+Y, XYtotal+(X*Y), Count+1).

mean([], Total, Count) ->
	Total/Count;
mean([H|T], Total, Count) ->
	mean(T, Total+H, Count+1).
		
cov_test() ->
	?assert(cov([{1,2}, {3,4}], 0, 0, 0, 0) =:= 1.0).		
mean_test() ->
	?assert(mean([1,2], 0, 0) =:= 1.5).
