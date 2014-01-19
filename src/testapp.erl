-module(testapp).
-include("c:/Program Files (x86)/Yaws-1.96/include/yaws_api.hrl").
-import(io, [format/2]).

-compile(export_all).

out(A) ->
	{html, otherapp:hello()}.
	
