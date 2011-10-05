-module(timevalue).
-export([run/3]).

run(IR, Inv, Periods) ->
	Inv * math:pow(IR + 1, Periods). 
