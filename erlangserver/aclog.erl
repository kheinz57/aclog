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
-export([qd/0]).

-include_lib("stdlib/include/qlc.hrl").
% systime and remote time are stored as milliseconds since the epoch (1970-1-1 0:0:0)
-record(aclog, {systime,remotetime,turns, port}).
-record(oldlog, {systime,remotetime,turns}).

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
        {attributes,record_info(fields,aclog)}
    ]),
	io:format("create_table()=~p~n",[Result]),
	io:format("setupDatabase done~n", []).

    
transformDatabase() ->
    Transformer = fun(X) when record(X,oldlog) ->
       Newrec = #aclog{systime=X#oldlog.systime,
                    remotetime=X#oldlog.remotetime,
                    turns=X#oldlog.turns, 
                    port=0},
       io:format("~p -> ~p ~n",[X,Newrec]),
       Newrec
    end,
    Result = mnesia:transform(aclog, Transformer, 
                record_info(fields,aclog),aclog),
    io:format("result transform= ~p ~n",[Result]).    
%    has to be {atomic,ok}    
    
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
	end.
%    transformDatabase().
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create some sample data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fillTable(List, _Time, 0) -> List;
fillTable(List, Time, Elements) ->
	Time1 = Time  + 1 + random:uniform(150),
	Time2 = (Time1  * 1000) + random:uniform(1000),
	Entry = #aclog{systime=Time1*1000,remotetime=Time2,turns=1, port=0},
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
addEntry(Time, Turns, Port)->
	io:format("addentry(~p,~p,~p)~n",[Time, Turns, Port]),
	Servertime=getActTimeUTC(),
	io:format("Servertime=~p~n",[Servertime]),
	Entry = #aclog{systime=Servertime,remotetime=Time,turns=Turns,port=Port},
	io:format("Entry=~p~n",[Entry]),
	F = fun() -> mnesia:write(Entry) end,
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
	ActTime = H#aclog.remotetime,
	DiffTime = ActTime - PreviousTime,
	if
		DiffTime < 2000 ->
%			io:format("ActTime=~p  Diff: ~p~n",[ActTime,DiffTime]),
			calcDiff(ActTime, T, Accu);
		true ->
			AccuNew = Accu ++ [{DiffTime,ActTime,H#aclog.turns}],
			calcDiff(ActTime, T, AccuNew)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Make a Java Script (JSON) List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeJSList(_ConversionFunction, [], Accu) -> Accu;

makeJSList(ConversionFunction,DiffList, Accu) -> 
	[H|Rest]=DiffList,
	{Diff, Time,Turns} = H,
	DataPair = {Time,ConversionFunction(Diff,Turns)},
	AccuNew = Accu++[DataPair],
 	makeJSList(ConversionFunction,Rest, AccuNew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In case of too many datapoints for the web page (it does not help to send more data
% to the browser as there are horizontal pixels)
% this is an optimization not done yet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reduceList([], Elements, Accu) ->
    io:format("reduceList([], ~p,~p)~n",[Elements, Accu]),
    Accu;
reduceList(LongList, Elements, Accu) when Elements > length(LongList) ->
    io:format("reduceList(~p, ~p,~p) when Elements > length(LongList)~n",[LongList,Elements, Accu]),
    reduceList(LongList, length(LongList), Accu);
    
reduceList(LongList, Elements, Accu) ->
    io:format("reduceList(~p,~p,~p)~n",[LongList, Elements, Accu]),
    {Head,NewLongList} = lists:split(Elements, LongList),
    io:format("Head=~p~n", [Head]),
    io:format("NewLongList=~p~n", [NewLongList]),
    Turns = lists:foldl(fun(X,Sum) -> X#aclog.turns + Sum end, 0, Head),
    io:format("turns=~p~n", [Turns]),
    
    [First|_] = Head,
    io:format("First=~p~n",[First]), 
    NewRecord = #aclog{systime=First#aclog.systime,
                    remotetime=First#aclog.remotetime,
                    turns=Turns, 
                    port=First#aclog.port},
    io:format("NewRecord=~p~n",[NewRecord]), 
%    reduceList(NewLongList, Elements, lists:append(Accu,[NewRecord])).
    reduceList(NewLongList, Elements, Accu ++ [NewRecord]).

% gets x-elements from the list, sum those and add the result to the return list
reduceList(LongList) ->
    {Head,_} = lists:split(7, LongList),
    reduceList(Head, 2, []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given interval and return the date as a JSON struct/list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
queryDatabase(QueryStartTime, QueryEndTime, Port, ConversionFunction) ->
    io:format("queryDatabase(~p,~p,~p) ~n",[QueryStartTime, QueryEndTime, Port]),
	UnsortedList = do(qlc:q([X || X <- mnesia:table(aclog),
            (X#aclog.remotetime >= QueryStartTime), 
            (X#aclog.remotetime < QueryEndTime),
            (X#aclog.port == Port)])
        ),
	LongList=lists:sort(fun(A, B) -> A#aclog.remotetime =< B#aclog.remotetime end, UnsortedList),
    io:format("LongList=~p~n",[LongList]),
	List = reduceList(LongList),
    io:format("    List=~p~n",[List]),
	[H|T] = List,
	StartTime = H#aclog.remotetime,
	DiffList = calcDiff(StartTime, T,[]),
	DataList = makeJSList(ConversionFunction,DiffList,[]),
	[_|DataListJS] = lists:flatten([io_lib:format(",[~B,~f]",[X,Y]) || {X,Y}<-DataList]),
	io:format("[~s]",[DataListJS]),
	io_lib:format("[~s]",[DataListJS]).

qd() ->
    queryDatabase(1299490962000, 1299526962000, 1, fun calcKiloWatts/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get all data of given table
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_full_table() ->
	RandomList = do(qlc:q([X || X <- mnesia:table(aclog)])),
	List=lists:sort(fun(A, B) -> A#aclog.remotetime =< B#aclog.remotetime end, RandomList),
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
	["port",PortStr, "time",_TimeString] = string:tokens(Input,"=&"),
	io:format("vz Input:~p~n",[Input]),
    Port = list_to_integer(PortStr),
    addEntry(getActTimeUTC(),1, Port),
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
	Data = query_full_table(),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>",
			"\r\n{\r\n    return ",			
			Data, 
			"\r\n}\r\n"			
			"<pre></body></html>" 
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given period as text/html (used for debugging purpose)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getdataashtml(SessionID, _Env, Input, Port, ConversionFunction) ->
	{From,To} = parseInput(Input),
	Data = queryDatabase(From,To, Port, ConversionFunction),
	mod_esi:deliver(SessionID, 
		[	"Content-Type: text/html\r\n\r\n", 
			"<html><body><pre>",
			Data,
			"<pre></body></html>" 
		]).

getdataashtml(SessionID, Env, Input) ->
    getacdataashtml(SessionID, Env, Input).
    
getacdataashtml(SessionID, Env, Input) -> 
    getdataashtml(SessionID, Env, Input, 0, calcKiloWatts).

getgasdataashtml(SessionID, Env, Input) -> 
    getdataashtml(SessionID, Env, Input, 1, calcCubicMetersPerHour).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the data for the given period as application/json. This is used via jQuery and flot
% to update the plot area only, without reloading the whole page. Known as AJAX (should be called AJAJ ;-))
% Example: "http://localhost:8081/erl/aclog:getdata?from=2009-12-01.17:00:00&to=2009-12-01.18:00:00"
% Input: "from=2009-12-01.17:00:00&to=2009-12-01.18:00:00"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getdataasjson(SessionID, _Env, Input, Port, ConversionFunction) ->
	io:format("getdataasjson: Input = ~p  ~n",[Input]),
	{From,To} = parseInput(Input),
	Data = queryDatabase(From,To,Port, ConversionFunction),
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
    getdataasjson(SessionID, Env, Input, 0, fun calcKiloWatts/2).

getgasdata(SessionID, Env, Input) ->
    getdataasjson(SessionID, Env, Input, 1, fun calcCubicMetersPerHour/2).
