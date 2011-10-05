-module(timevalue).
-export([run/2]).

run(InterestRate, Investment) ->
	(InterestRate * Investment) + Investment.	
