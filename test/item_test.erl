-module( item_test ).

-export([
	start/0,
	stop/0,
	add/0,
	del/0,
	get/0,
	search/0
]).

-define( INFILE, "/home/peter/pering/erlang/trees/bctree/test/filelist.txt" ).
-define( SEARCH, [
					"X11",
					"sudoers",
					"nothing"
				 ] ).
	
	
start() -> ok.

stop() -> ok.

add() ->

	List1 = load_file( ?INFILE ),
	bctree_item_sup:start_link([]),

	io:format( "1. Creating Item store for ~p elements ~n", [erlang:length(List1) ] ),
	
	case bctree_item:start_item( self(), [] ) of
		{ok, Pid} ->
			
			io:format( "2. Adding ~p elements to ~p ~n", [erlang:length(List1), Pid] ),
			lists:foreach( fun( E ) -> bctree_item:add( Pid, E, true ) end, List1 ),

			io:format( "2. Added ~p elements to ~p ~n", [erlang:length(List1), Pid] ),

			io:format( "10. Stopping element store ~p ~n", [Pid] ),
			bctree_item:stop( Pid );

		{error, Reason} -> {error, Reason}
	end.




del() -> ok.

get() ->
	List1 = load_file( ?INFILE ),
	List2 = ?SEARCH,

	bctree_item_sup:start_link([]),

	io:format( "1. Creating Item store for ~p elements ~n", [erlang:length(List1) ] ),
	
	case bctree_item:start_item( self(), [] ) of
		{ok, Pid} ->
			
			io:format( "2. Adding ~p elements to ~p ~n", [erlang:length(List1), Pid] ),
			lists:foreach( fun( E ) -> bctree_item:add( Pid, E, true ) end, List1 ),

			io:format( "2. Added ~p elements to ~p ~n", [erlang:length(List1), Pid] ),

			{ok, Size} = bctree_item:size( Pid ),

			io:format( "5. Searching in ~p [~p] elements for ~p items in ~p ~n", [erlang:length(List1), Size, erlang:length(List2), Pid] ),
			lists:foreach( fun( E ) -> 
						case bctree_item:get( Pid, E ) of
							false -> io:format("Elem not found: ~p ~n", [E] );
							{ok, Item} -> io:format("Found ~p : ~p ~n", [E,Item] )
						end
					end, List2 ),

			io:format( "10. Stopping element store ~p ~n", [Pid] ),
			bctree_item:stop( Pid );

		{error, Reason} -> {error, Reason}
	end.


search() -> ok.

load_file( Filenmae ) ->
	case file:open( Filenmae, [read] ) of
		{ ok, Handle } -> read_file( Handle, [] );
		{error, Reason} -> {error, Reason}
	end.

read_file( Handle, Lines ) ->
	case file:read_line( Handle ) of
		eof -> lists:reverse( Lines );
		{ok, Line0} -> 
			Line = string:strip( Line0, both, $\n ), 
			read_file( Handle, [Line|Lines] );
		{error, Reason} -> {error, Reason}
	end.