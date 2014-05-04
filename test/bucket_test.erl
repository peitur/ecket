-module( bukcet_test ).

-export([
		add/0,
		search/0,
		clear/0
	]).


add() -> 
	ok.

search( ) -> 
	ok-

clear() -> 
	ok.	

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