-module(calcs).
-export([cov/5]).

-include_lib("eunit/include/eunit.hrl").

cov([], Xtotal, Ytotal, XYtotal, Count) ->
	(XYtotal/Count)-((Xtotal/Count)*(Ytotal/Count));
cov([{X,Y}|T], Xtotal, Ytotal, XYtotal, Count) ->
	cov(T, Xtotal+X, Ytotal+Y, XYtotal+(X*Y), Count+1).

mean([], Total, Count) -> Total/Count;
mean([H|T], Total, Count) -> mean(T, Total+H, Count+1).

var(List) -> var_calc(List, mean(List, 0, 0), 0, 0).
var_calc([], _, Total, Count) -> Total/Count;
var_calc([H|T], Mean, Total, Count) ->
	var_calc(T, Mean, Total+math:pow(H-Mean, 2), Count+1).

std(List) -> math:sqrt(var(List)).

cov_test() -> ?assert(cov([{1,2}, {3,4}], 0, 0, 0, 0) =:= 1.0).		
mean_test() -> ?assert(mean([1,2], 0, 0) =:= 1.5).
var_test() -> ?assert(var([110,200,83]) =:= 2502.0).
std_test() -> ?assert(std([110,200,83]) =:= 50.0199960015992).
