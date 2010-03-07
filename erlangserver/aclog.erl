%   Copyright 2010 Heinz Haeberle
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
-module(aclog).
-export([starthttp/0,starthttps/0,getdataashtml/3,getdataasjs/3,getdataasjson/3,logdata/3,initdb/3,initdbwithsamples/3,printdb/3,main/0,query_full_table/0]).

-include_lib("stdlib/include/qlc.hrl").
% systime and remote time are stored as milliseconds since the epoch (1970-1-1 0:0:0)
-record(aclog, {systime,remotetime,turns}).

-define(TURNSPERKWH,75). % how many turns per kWh. Is specific for the used meter
-define(HOURINMS, 3600000). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% management stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert the gregorigian calender seconds to seconds since the epoch (1970-01-01)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gregorianToEpoch(TimeInS) ->
	TimeInS-(719528*24*3600).

strToInt(Str) -> 	{Int, _} = string:to_integer(Str),Int.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parse a string in full date and time format and return the number of seconds
% since the epoch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stringToMillisecondsSinceEpoch(TimeString) ->
	[Y,M,D,H,Min,S] = string:tokens(TimeString,"-.:"),
	DateTime = {{strToInt(Y),strToInt(M),strToInt(D)},
				{strToInt(H),strToInt(Min),strToInt(S)}},
	io:format("DatTime=~p~n",[DateTime]),
	Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
	gregorianToEpoch(Seconds) * 1000.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the actual time in ms since the epoch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getActTimeUTC() ->
	1000*(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% each turn (= one turne of the ferraris meter for example) is a specific amount 
% of energy. Use the TURNSPERKWH (defined above) to calc the energy of one turn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcKiloWattHours(Turns) ->
	(Turns / ?TURNSPERKWH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calc th power (Energy/time) for the given turns and diff time in ms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcKiloWatts(DiffTimeInMS, Turns) ->
	KiloWattHours = calcKiloWattHours(Turns),
	Hours = DiffTimeInMS / (3600000),
	KiloWattHours / Hours.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set up the database and creta ethe table aclog 
% it will bestored on disk and without a replication
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setupDatabase() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:clear_table(aclog),
	io:format("setupDatabase", []),
	io:format("create_table()=~p~n",[mnesia:create_table(aclog, [ {disc_copies, [node()] },{attributes,record_info(fields,aclog)}])]),
	io:format("setupDatabase done", []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start database and connect to a - hopefully - existing table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initDatabase() ->
	mnesia:start(),
	io:format("mnesia:wait_for_tables()=~p~n",[mnesia:wait_for_tables([aclog], 20000)]),
	io:format("initDatabase done~n", []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create some sample data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fillTable(List, _Time, 0) -> List;
fillTable(List, Time, Elements) ->
	Time1 = Time  + 1 + random:uniform(150),
	Time2 = (Time1  * 1000) + random:uniform(1000),
	Entry = #aclog{systime=Time1*1000,remotetime=Time2,turns=1},
	List1 = List ++ [Entry],
	fillTable(List1,Time1, Elements-1).

createSampleData(Entries)->
	Dt = calendar:local_time(),
	GregorianSeconds = calendar:datetime_to_gregorian_seconds(Dt)-719528*24*3600,
	Table = fillTable([],GregorianSeconds,Entries),
	F = fun() -> lists:foreach(fun mnesia:write/1, Table) end,
	{atomic,_Result} = mnesia:transaction(F),
	nil.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adds a new entry. Usually called by the http 'servlet' handler used by the meter connected
% to the powerline or watches the ferraris meter's turns 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addEntry(Time, Turns)->
	io:format("addentry(~p,~p)~n",[Time, Turns]),
	Servertime=getActTimeUTC(),
%	1000*(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600),
	io:format("Servertime=~p~n",[Servertime]),
	Entry = #aclog{systime=Servertime,remotetime=Time,turns=Turns},
	io:format("Entry=~p~n",[Entry]),
	F = fun() -> mnesia:write(Entry) end,
	{atomic,_Result} = mnesia:transaction(F),
	io:format("Result: ~p~n",[_Result]),
	nil.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calc a list of differences between entry n and n+1 in the list. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcDiff(_PreviousTime, [], Accu) -> 
	Accu;

calcDiff(PreviousTime, List, Accu) ->
	[H|T] = List,
	ActTime = H#aclog.remotetime,
	DiffTime = ActTime - PreviousTime,
	if
		DiffTime < 2000 ->
			io:format("ActTime=~p  Diff: ~p~n",[ActTime,DiffTime]),
			calcDiff(ActTime, T, Accu);
		true ->
			AccuNew = Accu ++ [{DiffTime,ActTime,H#aclog.turns}],
			calcDiff(ActTime, T, AccuNew)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Make a Java Script (JSON) List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeJSList([], Accu) -> Accu;

makeJSList(DiffList, Accu) -> 
	[H|Rest]=DiffList,
	{Diff, Time,Turns} = H,
	DataPair = {Time,calcKiloWatts(Diff,Turns)},
	AccuNew = Accu++[DataPair],
 	makeJSList(Rest, AccuNew).

%sumEntries(Entry, {}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In case of to many datapoints for the web page (it does not help to send more data
% to the browser as there are horizontal pixels)
% this is an optimization not done yet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
reduceList(LongList) ->
	LongList.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given interval and return the date as a JSON struct/list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
queryDatabase(QueryStartTime, QueryEndTime) ->
	UnsortedList = do(qlc:q([X || X <- mnesia:table(aclog),(X#aclog.remotetime >= QueryStartTime), (X#aclog.remotetime < QueryEndTime) ])),
	LongList=lists:sort(fun(A, B) -> A#aclog.remotetime =< B#aclog.remotetime end, UnsortedList),
	List = reduceList(LongList),
	[H|T] = List,
	StartTime = H#aclog.remotetime,
	DiffList = calcDiff(StartTime, T,[]),
	DataList = makeJSList(DiffList,[]),
	[_|DataListJS] = lists:flatten([io_lib:format(",[~B,~f]",[X,Y]) || {X,Y}<-DataList]),
	io:format("[~s]",[DataListJS]),
	io_lib:format("[~s]",[DataListJS]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get all date
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_full_table() ->
	RandomList = do(qlc:q([X || X <- mnesia:table(aclog)])),
	List=lists:sort(fun(A, B) -> A#aclog.remotetime =< B#aclog.remotetime end, RandomList),
	io_lib:format("query result:~p~n",[List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main routine. Since i am used to do C++ it had to be that way.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main() ->
	initDatabase(),
	inets:start(),
	aclog:starthttp(),
	nil.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start up the http server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
starthttp() ->
 inets:start(httpd, [
	  {modules, [mod_esi,mod_get, mod_log, mod_disk_log,mod_auth]},
	  {port,8081},
	  {server_name,"aclog"},
	  {server_root,"log"},
	  {document_root,"www"},
	  {directory_index, ["index.hml", "index.htm"]},
	  {erl_script_alias, {"/erl", [aclog]}},
	  {error_log, "error.log"},
	  {security_log, "security.log"},
	  {transfer_log, "transfer.log"},
	  {mime_types,[ {"html","text/html"}, {"css","text/css"},
					{"js","application/x-javascript"},{"json","application/json"}]}
	 ]),
 	io:format("http start done~n", []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start up the https server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
starthttps() ->
  inets:start(httpd, [
	  {modules, [mod_esi,mod_get, mod_log, mod_disk_log]},
	  {port,8082},
	  {server_name,"aclog"},
	  {server_root,"logssl"},
	  {document_root,"www"},
	  {socket_type,ssl},
	  {ssl_verify_client, 0},
	  {ssl_certificate_key_file,"ssl.key"},
	  {ssl_certificate_file, "ssl.crt"},
	  {directory_index, ["index.hml", "index.htm"]},
	  {erl_script_alias, {"/erl", [aclog]}},
	  {error_log, "error.log"},
	  {security_log, "security.log"},
	  {transfer_log, "transfer.log"},
	  {mime_types,[ {"html","text/html"},{"css","text/css"},
					{"js","application/x-javascript"},{"json","application/json"}]}
	 ]),
 	io:format("https start done~n", []).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Servlet (erl_script_alias / mod_esi)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This does create a new database table. !!!Deletes all enties!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initdb(SessionID, _Env, _Input) ->
	setupDatabase(),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body>Database set up done</body></html>" 
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% creates 100 random sample data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createsamples(SessionID, _Env, _Input) ->
	createSampleData(100),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body>Database is filled with sample data now</body></html>" 
		]).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the url used by the meter - usually a small controller attached to a ferraris
% or other smart meter
%
% This should be changed to POST, because GET - according to CRUD - should not modify  
% anything on the server side (side effect free)
% Example URL: "http://localhost:8081/erl/aclog:logdata?systime=1&remotetime=2&turns=3" 
% Input: "logdata?remotetime=2&turns=3" 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logdata(SessionID, _Env, Input) ->
	["time",TimeString, "turns",TurnsString] = string:tokens(Input,"=&"),
	{Time,_} = string:to_integer(TimeString),
	{Turns,_} = string:to_integer(TurnsString),
	addEntry(Time,Turns),
	mod_esi:deliver(SessionID, 
		[   "Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>Entry added</pre></body></html>" 
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Does parse the 'get' parameters from the GUI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parseInput(Input) ->
	io:format("Input:~p~n",[Input]),
	Tokens = string:tokens(Input,"=&"),
	io:format("Tokens:~p~n",[Tokens]),
	case  Tokens of
		["from",FromStr, "to",ToStr] ->
			From = stringToMillisecondsSinceEpoch(FromStr),
			To   = stringToMillisecondsSinceEpoch(ToStr);
		["from",FromStr,_] ->
			From = stringToMillisecondsSinceEpoch(FromStr),
			To = From + (?HOURINMS * 24);
		[] -> 	
			To = getActTimeUTC(),
			From = To - (?HOURINMS * 24)
	end,
	io:format("From:~p  To:~p~n",[From,To]),
	{From,To}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given period as text/html (used for debugging purpose)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getdataashtml(SessionID, _Env, Input) ->
	{From,To} = parseInput(Input),
	Data = queryDatabase(From,To),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>",
			Data,
			"<pre></body></html>" 
		]).
	
% Example: "http://localhost:8081/erl/aclog:printdb"
printdb(SessionID, _Env, _Input) ->
	Data = query_full_table(),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>",
			"\r\nfunction GetData(){\r\n    return ",			
			Data,
			"\r\n}\r\n"			
			"<pre></body></html>" 
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given period as application/json. This is used via jQuery and flot
% to update the plot area only, without reloading the whole page. Known as AJAX (should be called AJAJ ;-))
% Example: "http://localhost:8081/erl/aclog:getdata?from=2009-12-01.17:00:00&to=2009-12-01.18:00:00"
% Input: "from=2009-12-01.17:00:00&to=2009-12-01.18:00:00"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getdataasjson(SessionID, _Env, Input) ->
	io:format("getdataasjson: Input = ~p  ~n",[Input]),
	{From,To} = parseInput(Input),
	Data = queryDatabase(From,To),
	io:format("Result of query:~p~n",[Data]),
	mod_esi:deliver(SessionID, 
		[   "Content-Type: application/json\r\n\r\n",
			"\r\{\r\n    data: ",
			Data,
			"\r\n}\r\n"	
		]).
