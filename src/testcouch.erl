-module(testcouch).
-export([run/0]).


run() ->
	application:start(sasl),
	application:start(ibrowse),
	application:start(couchbeam),
	Connection = couchbeam:server_connection("localhost", 5984, "", []),
	Options = [],
	Db = couchbeam:open_db(Connection, "test2", Options).
