%   bctree, concurrent b-tree:ish search, item implementation
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


-module( bctree_item ).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-define( ITEM_TYPE, set ).




-export([
		start_link/1, start_link/2,
		start_item/1, start_item/2,
		stop/1,stop/2,
		exists/2,
		add/3,
		del/2,
		get/2,
		geta/2, geta/3,
		size/1
	]).


start_link( Parent ) -> start_link( Parent, [] ).
start_link( Parent, Options ) ->
	gen_server:start_link( ?MODULE, [Parent, Options], []  ).

start_item( Parent ) -> start_item( Parent, [] ).
start_item( Parent, Options ) -> bctree_item_sup:start_item( Parent, Options ).

stop( Pid ) -> stop( Pid, normal ).
stop( Pid, Reason ) -> gen_server:call( Pid, {stop, Reason}).


add( Pid, Key, Item ) when erlang:is_list( Key ) -> 
	case add( Pid, erlang:list_to_binary( Key ), Item) of
		{ok, K} -> {ok, binary_to_list( K ) };
		Other -> Other
	end;

add( Pid, Key, Item ) -> gen_server:call( Pid, {add, Key, Item} ).

del( Pid, Key ) when erlang:is_list( Key ) -> del( Pid, erlang:list_to_binary( Key) );
del( Pid, Key ) -> gen_server:call( Pid, {del, Key}).

get( Pid, Key ) when erlang:is_list( Key ) -> 
	case get( Pid, erlang:list_to_binary( Key) ) of
		false -> false;
		{ok, {Key, Item}} -> {ok, { Key, Item } };
		{ok, Item} -> {ok,  Item };
		Other -> Other
	end;

get( Pid, Key ) -> gen_server:call( Pid, {get, Key}).

geta( Pid, Key ) -> geta( Pid, Key, erlang:make_ref()).
geta( Pid, Key, Ref ) when erlang:is_list( Key ) -> geta( Pid, erlang:list_to_binary( Key), Ref );
geta( Pid, Key, Ref) -> gen_server:cast( Pid, {get, Key, Ref}),	{ok, Ref}. 

exists( Pid, Key ) -> 
	case get( Pid, Key ) of
		false -> false;
		{ok, Item} -> true
	end.

size( Pid ) -> 
	gen_server:call( Pid, {get, size} ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { parent, items = [], type = ?ITEM_TYPE, overwrite = false }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Parent, Options]) ->
	Type = proplists:get_value( type, Options, ?ITEM_TYPE ),
	Overwrite = proplists:get_value( overwrite, Options, false ),

    {ok, #state{ parent = Parent, items = [], type = Type, overwrite = Overwrite }}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().

%% ====================================================================
handle_call( {add, Key, Item}, _From,  State ) ->
	case x_add_data( Key, Item, State#state.type, State#state.items ) of
		{error, Reason} -> {reply, {error, Reason}, State };
		NewItems -> {reply, {ok, Key}, State#state{ items = NewItems} }
	end;


handle_call( {del, Key}, _From, State ) ->
	{reply, ok, State#state{ items = x_delete_data( Key, State#state.items ) }};

handle_call( {get, size}, _From, State ) ->
	{reply, {ok, erlang:length( State#state.items) }, State};

%% if special case get, it's a key,
handle_call( {get, Key}, _From, State ) ->
	case x_find_key(  Key, State#state.items ) of
		false -> {reply, false, State };
		{Key,Item} -> {reply, {ok, Item}, State }
	end;

handle_call( {stop, Reason}, _From, State ) ->
	{stop, Reason, State};

handle_call(Request, From, State) ->
    Reply = {error, not_implemented},
    {reply, Reply, State}.




%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast( {get, Key, Ref}, State ) ->
	case x_find_key( Key, State#state.items ) of
		false -> 
			State#state.parent ! {false, Ref},
			{noreply, State};
		Item -> 
			State#state.parent ! {ok, Item, Ref},
			{noreply, State}
	end;

handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

x_add_data( Key, Item, Type, ItemList ) ->
	x_add_data( Key, Item, Type, ItemList, true ).


x_add_data( Key, Item, set, ItemList, true ) -> 
	case x_find_key( Key, ItemList ) of
		false -> [{Key,Item}|ItemList];
		Item -> [{Key,Item}|x_delete_data( Key, ItemList )]
	end;

x_add_data( Key, Item, set, ItemList, false ) -> 
	case x_find_key( Key, ItemList ) of
		false -> [{Key,Item}|ItemList];
		Item -> ItemList
	end;

x_add_data( Key, Item, set, ItemList, BadBool ) -> 
	{error, badbool};

x_add_data( Key, Item, bag, ItemList, _ ) -> 
	[{Key, Item}|ItemList];

x_add_data( _, _, _OtherType, _, _ ) -> 
	{error, badtype}.


x_delete_data( Key, ItemList ) ->
	lists:keydelete( Key, 1, ItemList).

x_find_key( Key, ItemList ) ->
	case lists:keyfind(  Key, 1, ItemList ) of
		false -> false;
		Item -> Item
	end.
