%   bctree, concurrent b-tree:ish search, bucket test module
% 
%    Copyright (C) 2014 Peter Bartha <peitur@gmail.com>
%
%    This program is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module( bucket_test ).

-export([
		add/0,
		search/0, search2/0,
		clear/0
	]).

-define( INFILE0, "slim" ).
-define( INFILE1, "american" ).
-define( INFILE2, "english").
-define( INFILE3, "ameng").


fun0(E) -> E.

fun1(E) when erlang:length(E) > 2 -> string:sub_string(E, 1, 1);
fun1(_) -> undefined.

fun2(E) when erlang:length(E) > 3 -> string:sub_string(E, 2, 2);
fun2(_) -> undefined.

fun3(E) when erlang:length(E) > 4 -> string:sub_string(E, 3, 3);
fun3(_) -> undefined.

fun4(E) when erlang:length(E) > 5 -> string:sub_string(E, 4, 4);
fun4(_) -> undefined.

init( ) ->
	application:start( sasl ),
	application:start( bctree ).
%	bctree_bucket_sup:start_link([]),
%	bctree_item_sup:start_link([]).

add() -> 
	init(),
	FunList = [
		fun(E) -> fun1(E) end,
		fun(E) -> fun2(E) end
%		fun(E) -> fun3(E) end,
%		fun(E) -> fun4(E) end
	],

	case bctree_bucket:start_bucket( self(), test0, FunList, [] ) of
		{ok, Pid} ->

			List1 = load_file( ?INFILE2 ),
			io:format( "INFILE: ~p ~n", [erlang:length(List1)] ),
			[ bctree_bucket:add_item( Pid, E, 1 ) || E <- List1 ],

			Size = bctree_bucket:get_size( Pid ),
			io:format("STORE SIZE:::: ~p ~n", [Size]),

			bctree_bucket:stop( Pid ),

			ok;
		{error, Reason} -> {error, Reason}
	end.

search( ) -> 
	init(),
	FunList = [
		fun(E) -> fun1(E) end,
		fun(E) -> fun2(E) end,
		fun(E) -> fun3(E) end,
		fun(E) -> fun4(E) end
	],

	case bctree_bucket:start_bucket( self(), test0, FunList, [] ) of
		{ok, Pid} ->

			StartNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			List1 = load_file( ?INFILE2 ),
			List0 = load_file( ?INFILE1 ),

			io:format( "INFILE: ~p ~n", [erlang:length(List1)] ),
			io:format( "REFFILE: ~p ~n", [erlang:length(List0)] ),

			FileLoadedNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			[ bctree_bucket:add_item( Pid, E, 1 ) || E <- List1 ],

			LoadedNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),


			{ok, Size} = bctree_bucket:get_size( Pid ),
			io:format("STORE SIZE: ~p / ~p ~n", [ lists:sum( Size ), erlang:length( Size) ]),

			SizeNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			io:format("Searching for ~p items amoung ~p  ~n", [erlang:length( List0 ),erlang:length( List1 )]),		
			lists:foreach( fun( E ) -> 
							case bctree_bucket:get_item( Pid, E ) of
								{ok, false} -> io:format( "NotFound: ~p ~n", [E] ); 
								{ok, Data} -> ok;%io:format( "Found: ~p : ~p ~n", [E, Data] );
								{error, Reason} -> io:format("Error: ~p : ~p ~n", [E, Reason] );
								Other -> io:forma("Unknown: ~p : ~p ~n", [E, Other])
							end

						end,
					List0 ),

			SearchedNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			io:format("Searched for ~p items amoung ~p ~n", [erlang:length( List0 ), erlang:length(List1)]),		


			bctree_bucket:stop( Pid ),

			ExitNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			io:format("===================================================~n"),			
			io:format("Total time: ~p ~n", [ExitNow - StartNow]),
			io:format("Loaded time: ~p ~n", [FileLoadedNow - StartNow]),
			io:format("Size time: ~p ~n", [SizeNow - LoadedNow]),
			io:format("Search time: ~p ~n", [SearchedNow - SizeNow]),
			io:format("===================================================~n"),			


			ok;
		{error, Reason} -> {error, Reason}
	end.


search2( ) -> 
	init(),
	FunList = [
		fun(E) -> fun1(E) end,
		fun(E) -> fun2(E) end,
		fun(E) -> fun3(E) end,
		fun(E) -> fun4(E) end
	],

	case bctree_bucket:start_bucket( self(), test0, FunList, [] ) of
		{ok, Pid} ->

			StartNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			List0 = load_file( ?INFILE3 ),
			List1 = load_file( ?INFILE2 ),

			io:format( "INFILE: ~p ~n", [erlang:length(List1)] ),
			io:format( "REFFILE: ~p ~n", [erlang:length(List0)] ),

			FileLoadedNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			[ bctree_bucket:add_item( Pid, E, 1 ) || E <- List1 ],

			LoadedNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),


			{ok, Size} = bctree_bucket:get_size( Pid ),
			io:format("STORE SIZE: ~p / ~p ~n", [ lists:sum( Size ), erlang:length( Size) ]),

			SizeNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			io:format("Searching for ~p items amoung ~p  ~n", [erlang:length( List0 ),erlang:length( List1 )]),		
			Missing = lists:map( fun( E ) -> 
							case bctree_bucket:get_item( Pid, E ) of
								{ok, false} -> {E, missing};
								{ok, Data} -> {E, exists};
								{error, Reason} -> io:format("Error: ~p : ~p ~n", [E, Reason] );
								Other -> io:forma("Unknown: ~p : ~p ~n", [E, Other])
							end

						end,
					List0 ),

			SearchedNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			io:format("Searched for ~p items amoung ~p ~n", [erlang:length( List0 ), erlang:length(List1)]),		
			io:format("Number of missing items: ~p ~n",[erlang:length( missing(Missing) )]),

			bctree_bucket:stop( Pid ),

			ExitNow = calendar:datetime_to_gregorian_seconds( erlang:localtime() ),

			io:format("===================================================~n"),			
			io:format("Total time: ~p ~n", [ExitNow - StartNow]),
			io:format("Loaded time: ~p ~n", [FileLoadedNow - StartNow]),
			io:format("Size time: ~p ~n", [SizeNow - LoadedNow]),
			io:format("Search time: ~p ~n", [SearchedNow - SizeNow]),
			io:format("===================================================~n"),			


			ok;
		{error, Reason} -> {error, Reason}
	end.

missing( ElemList ) ->
	missing( ElemList, [] ).

missing( [], Res ) -> Res;

missing( [{E,exists}|L], Res ) ->
	missing( L, Res );

missing( [{E, missing}|L], Res ) ->
	missing( L, [E|Res]).	


clear() -> 
	ok.	

load_file( Filenmae ) ->
	case file:open( Filenmae, [read] ) of
		{ ok, Handle } -> read_file( Handle, [] );
		{error, Reason} -> {error, Reason}
	end.

read_file( Handle, Lines ) ->
	case file:read_line( Handle ) of
		eof -> Lines;
		{ok, Line0} -> 
			Line = string:strip( Line0, both, $\n ), 
			read_file( Handle, [Line|Lines] );
		{error, Reason} -> {error, Reason}
	end.