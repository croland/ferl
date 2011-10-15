-module(testcouch).
-export([run/0]).


run() ->
	application:start(sasl),
	application:start(ibrowse),
	application:start(couchbeam),
	Connection = couchbeam:server_connection(),
	Options = [],
	Db = couchbeam:create_db(Connection, "test2", Options).
