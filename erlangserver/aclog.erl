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
-export([starthttp/0,getdataasjson/3,vz/3,initdb/3,printdb/3,main/1,main/0,createsamples/3]). %query_full_table/0,
-export([getacdata/3,getgasdata/3,getacdataashtml/3,getgasdataashtml/3,getdataashtml/3]).
% I do not know why this is required. They are used in exported functions 
% but still get marked by the compiler as unused??
-export ([calcCubicMetersPerHour/2,calcKiloWatts/2]). 

-include_lib("stdlib/include/qlc.hrl").
% systime and remote time are stored as milliseconds since the epoch (1970-1-1 0:0:0)
-record(logstruct, {systime,remotetime,turns}).

-define(TURNSPERKWH,75). % how many turns per kWh. Is specific for the used meter
-define(TURNSPERCUBICMETER,100). % how many turns per cubic meter. Is specific for the used meter.
-define(HOURINMS, 3600000).
-define(MINUTESINMS, 60000).
 
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
% calc the power (Energy/time) for the given turns and diff time in ms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcKiloWatts(DiffTimeInMS, Turns) ->
	KiloWattHours = calcKiloWattHours(Turns),
	Hours = DiffTimeInMS / (3600000),
	KiloWattHours / Hours.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% each turn (= one turne of the ferraris meter for example) is a specific amount 
% of energy. Use the TURNSPERKWH (defined above) to calc the energy of one turn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % one turn is 0.01 m2 gas
calcCubicMeter(Turns) -> 
    (Turns / ?TURNSPERCUBICMETER).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calc the gas volume per time for the given turns and diff time in ms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calcCubicMetersPerHour(DiffTimeInMS, Turns) ->
	CubicMeter = calcCubicMeter(Turns),
	Hours = DiffTimeInMS / (3600000),
	CubicMeter / Hours.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set up the database and create the table aclog 
% it will be stored on disk and without a replication
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setupDatabase() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:clear_table(aclog),
    Result = mnesia:create_table(aclog, [ 
        {disc_copies, [node()] },
        {record_name, logstruct},
        {attributes,record_info(fields,logstruct)}
    ]),
	io:format("create_table()=~p~n",[Result]),
	io:format("setupDatabase done~n", []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add the gaslog table to the database 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addDatabaseTableGas() ->
	mnesia:clear_table(gaslog),
    Result = mnesia:create_table(gaslog, [ 
        {disc_copies, [node()] },
        {record_name, logstruct},
        {attributes,record_info(fields,logstruct)}
    ]),
	io:format("create_table(gaslog)=~p~n",[Result]),
	io:format("addDatabaseTableGas done", []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start database and connect to a - hopefully - existing table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initDatabase() ->
	mnesia:start(),
	TableInitState = mnesia:wait_for_tables([aclog], 5000),
	case  TableInitState of
		ok ->
			io:format("initDatabase aclog done~n", []);
		_  ->
			io:format("mnesia:wait_for_tables(aclog)=~p~nsetting up database ...",[TableInitState]),
			setupDatabase()
	end,
	TableInitStateGas = mnesia:wait_for_tables([gaslog], 5000),
	case  TableInitStateGas of
		ok ->
			io:format("initDatabase for gas done~n", []);
		_  ->
			io:format("mnesia:wait_for_tables(gaslog)=~p~nadding table to database ...",[TableInitState]),
			addDatabaseTableGas()
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create some sample data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fillTable(List, _Time, 0) -> List;
fillTable(List, Time, Elements) ->
	Time1 = Time  + 1 + random:uniform(150),
	Time2 = (Time1  * 1000) + random:uniform(1000),
	Entry = #logstruct{systime=Time1*1000,remotetime=Time2,turns=1},
	List1 = List ++ [Entry],
	fillTable(List1,Time1, Elements-1).

createSampleData(Entries)->
	Dt = calendar:local_time(),
	GregorianSeconds = calendar:datetime_to_gregorian_seconds(Dt)-719528*24*3600,
	Table = fillTable([],GregorianSeconds,Entries),
	F = fun() -> lists:foreach(fun mnesia:write/3, aclog, Table, write) end,
	{atomic,_Result} = mnesia:transaction(F),
	nil.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adds a new entry. Usually called by the http 'servlet' handler used by the meter connected
% to the powerline or watches the ferraris meter's turns 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addEntry(Time, Turns, Table)->
	io:format("addentry(~p,~p,~p)~n",[Time, Turns, Table]),
	Servertime=getActTimeUTC(),
	io:format("Servertime=~p~n",[Servertime]),
	Entry = #logstruct{systime=Servertime,remotetime=Time,turns=Turns},
	io:format("Entry=~p~n",[Entry]),
	F = fun() -> mnesia:write(Table, Entry, write) end,
    Res = mnesia:transaction(F),
	io:format("Res: ~p~n",[Res]),
	{atomic,Result} = Res,
	io:format("Result: ~p~n",[Result]),
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
	ActTime = H#logstruct.remotetime,
	DiffTime = ActTime - PreviousTime,
	if
		DiffTime < 2000 ->
%			io:format("ActTime=~p  Diff: ~p~n",[ActTime,DiffTime]),
			calcDiff(ActTime, T, Accu);
		true ->
			AccuNew = Accu ++ [{DiffTime,ActTime,H#logstruct.turns}],
			calcDiff(ActTime, T, AccuNew)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Make a Java Script (JSON) List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeJSList(_ConversionFunction, [], Accu) -> Accu;

makeJSList(ConversionFunction,DiffList, Accu) -> 
%    io:format("makeJSList ~p ~p ~p ~n",[ConversionFunction,DiffList, Accu]),
	[H|Rest]=DiffList,
%    io:format("H,Rest= ~p,~p ~n",[H,Rest]),
	{Diff, Time,Turns} = H,
%    io:format("Diff, Time,Turns= ~p,~p,~p ~n",[Diff, Time,Turns]),
	DataPair = {Time,ConversionFunction(Diff,Turns)},
%    io:format("DataPair= ~p ~n",[DataPair]),
	AccuNew = Accu++[DataPair],
%    io:format("in makeJSList ~p ~p ~p ~n",[ConversionFunction,Rest, AccuNew]),
 	makeJSList(ConversionFunction,Rest, AccuNew).

%sumEntries(Entry, {}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In case of too many datapoints for the web page (it does not help to send more data
% to the browser as there are horizontal pixels)
% this is an optimization not done yet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reduceList(LongList) ->
	LongList.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given interval and return the date as a JSON struct/list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
queryDatabase(QueryStartTime, QueryEndTime, Table, ConversionFunction) ->
    io:format("queryDatabase ~p ~p~n",[Table,ConversionFunction]),
	UnsortedList = do(qlc:q([X || X <- mnesia:table(Table),(X#logstruct.remotetime >= QueryStartTime), (X#logstruct.remotetime < QueryEndTime)])),
%    io:format("LUnsortedlist=~p~n",[LUnsortedList]),
%    {UnsortedList,_} = lists:split(10,LUnsortedList),
%    io:format("Unsortedlist=~p~n",[UnsortedList]),
	LongList=lists:sort(fun(A, B) -> A#logstruct.remotetime =< B#logstruct.remotetime end, UnsortedList),
%    io:format("LongList=~p~n",[LongList]),
	List = reduceList(LongList),
%	io:format("List=~p~n",[List]),
	[H|T] = List,
%    io:format("H=~p~n",[H]),
%    io:format("T=~p~n",[T]),
	StartTime = H#logstruct.remotetime,
	DiffList = calcDiff(StartTime, T,[]),
%    io:format("Difflist=~p~n",[DiffList]),
	DataList = makeJSList(ConversionFunction,DiffList,[]),
%    io:format("DataList=~p~n",[DataList]),
	[_|DataListJS] = lists:flatten([io_lib:format(",[~B,~f]",[X,Y]) || {X,Y}<-DataList]),
	io:format("[~s]",[DataListJS]),
	io_lib:format("[~s]",[DataListJS]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get all data of given table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_full_table(Table) ->
	RandomList = do(qlc:q([X || X <- mnesia:table(Table)])),
	List=lists:sort(fun(A, B) -> A#logstruct.remotetime =< B#logstruct.remotetime end, RandomList),
	io_lib:format("query result:~p~n",[List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main routine. Since i am used to do C++ it had to be that way.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main(_) ->
	initDatabase(),
	inets:start(),
	aclog:starthttp(),
	nil.

main() ->
	main(x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start up the http server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
starthttp() ->
    inets:start(httpd, [
        {modules, [mod_esi,mod_get, mod_log, mod_disk_log,mod_auth]},
        {port,4715},
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
    io:format("http 4715 start done~n", []).
 
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
% This is the url used by the volkszaehler meter
%
% Example URL: "http://localhost:8081/erl/aclog:vz?port=1&time=560" 
%
% This should be changed to POST, because GET - according to CRUD - should not modify  
% anything on the server side (side effect free)
% Example URL: "http://localhost:8081/erl/aclog:vz?port=0&time=1243" 
% Input: "logdata?remotetime=2&turns=3" 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vz(SessionID, _Env, Input) ->
	io:format("vz Input:~p~n",[Input]),
	["port",Port, "time",_TimeString] = string:tokens(Input,"=&"),
	io:format("vz Input:~p~n",[Input]),
	case  Port of
		"0" -> addEntry(getActTimeUTC(),1, aclog);
		"1" -> addEntry(getActTimeUTC(),1, gaslog)
	end,
    io:format("addentry done ~n",[]),
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
		["lasthours",LastHours] ->
			To = getActTimeUTC(),
            Hours = list_to_integer(LastHours),
			From = To - (?HOURINMS * Hours);
		["lastminutes",LastMinutes] ->
			To = getActTimeUTC(),
            Hours = list_to_integer(LastMinutes),
			From = To - (?MINUTESINMS * Hours);
		[] -> 	
			To = getActTimeUTC(),
			From = To - (?HOURINMS * 24)
	end,
	io:format("From:~p  To:~p~n",[From,To]),
	{From,To}.

% Example: "http://localhost:8081/erl/aclog:printdb"
printdb(SessionID, _Env, _Input) ->
	Datagas = query_full_table(gaslog),
	Dataac = query_full_table(qclog),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>",
			"\r\nTable GAS{\r\n    return ",			
			Datagas, 
			"\r\n}\r\n"			
			"\r\nTable aclog{\r\n    return ",			
			Dataac, 
			"\r\n}\r\n"			
			"<pre></body></html>" 
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given period as text/html (used for debugging purpose)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getdataashtml(SessionID, _Env, Input, Table, ConversionFunction) ->
	{From,To} = parseInput(Input),
	Data = queryDatabase(From,To, Table, ConversionFunction),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>",
			Data,
			"<pre></body></html>" 
		]).

getdataashtml(SessionID, Env, Input) ->
    getacdataashtml(SessionID, Env, Input).
    
getacdataashtml(SessionID, Env, Input) -> 
    getdataashtml(SessionID, Env, Input, aclog, calcKiloWatts).

getgasdataashtml(SessionID, Env, Input) -> 
    getdataashtml(SessionID, Env, Input, gaslog, calcCubicMetersPerHour).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given period as application/json. This is used via jQuery and flot
% to update the plot area only, without reloading the whole page. Known as AJAX (should be called AJAJ ;-))
% Example: "http://localhost:8081/erl/aclog:getdata?from=2009-12-01.17:00:00&to=2009-12-01.18:00:00"
% Input: "from=2009-12-01.17:00:00&to=2009-12-01.18:00:00"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getdataasjson(SessionID, _Env, Input, Table, ConversionFunction) ->
	io:format("getdataasjson: Input = ~p  ~n",[Input]),
	{From,To} = parseInput(Input),
	Data = queryDatabase(From,To,Table, ConversionFunction),
	io:format("Result of query:~p~n",[Data]),
	mod_esi:deliver(SessionID, 
		[   "Content-Type: application/json\r\n\r\n",
			"\r\{\r\n    \"data\": ",
			Data,
			"\r\n}\r\n"	
		]).

%deprecated. Use getacdata instead
getdataasjson(SessionID, Env, Input) -> 
    getacdata(SessionID, Env, Input).
        
getacdata(SessionID, Env, Input) ->
    getdataasjson(SessionID, Env, Input, aclog, fun calcKiloWatts/2).

getgasdata(SessionID, Env, Input) ->
    getdataasjson(SessionID, Env, Input, gaslog, fun calcCubicMetersPerHour/2).
