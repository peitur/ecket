%   bctree_bucket, concurrent b-tree:ish search, bucket module implementation
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

-module( bctree_bucket ).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		start_link/4,
		start_bucket/4,
		clear_bucket/1,
		stop/1, stop/2,
		add_item/3,
		del_item/2,
		get_item/2,
		exists/2
	]).

-export([
		set_parent/2,
		get_parent/1,
		get_name/1,
		get_size/1,
		get_generator/1,
		set_generator/2,
		get_sub_generators/1
	]).

start_link( Parent, Name, GeneratorList, Options ) ->
	gen_server:start_link( ?MODULE, [Parent, Name, GeneratorList, Options], [] ).


start_bucket( Parent, Name, GeneratorList, Options ) ->
	bctree_bucket_sup:start_bucket( Parent, Name, GeneratorList, Options ).


stop( Pid ) -> stop(Pid, normal).
stop( Pid, Reason ) -> gen_server:call( Pid, {stop, Reason }).

clear_bucket( Bucket ) ->
	gen_server:call( Bucket, {clear}).

add_item( Bucket, Key, Item ) ->
	gen_server:call( Bucket, {add, Key, Item} ).

del_item( Bucket, Key ) ->
	gen_server:call( Bucket, {del, Key} ).

get_item( Bucket, Key ) ->
	gen_server:call( Bucket, {get, item, Key} ).

exists( Bucket, Key ) ->
	gen_server:call( Bucket, {exists, Key}).


set_parent( Bucket, Parent ) -> gen_server:call( Bucket, {set, parent, Parent}).
get_parent( Bucket ) -> gen_server:call( Bucket, {get, parent}).
get_name( Bucket ) -> gen_server:call( Bucket, {get, name} ).
get_size( Bucket ) -> gen_server:call( Bucket, {get, size} ).

get_generator( Bucket ) -> gen_server:call( Bucket, {get, generator}).
set_generator( Bucket, Gen ) -> gen_server:call( Bucket, {set, generator, Gen}).

get_sub_generators( Bucket ) -> gen_server:call( Bucket, {get, genlist}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
% { item, Pid }
% { bucket, Name, Pid }
-record(state, { parent, name, children = [], generator, sub_generators = [] }).

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
%% if generator == undefined, it's the end of the line, all children are item processes
%% if generator != undefined, all children are buckets created with sub_generator list
%% Gen = fun( E ) -> ... end. 
init([Parent, Name, GeneratorList, Options]) ->
	erlang:process_flag( trap_exit, true ),
	case GeneratorList of
		[] ->
		    {ok, #state{ parent = Parent, name = Name, generator = undefined, sub_generators = [], children = [] } };
		[Gen|List] ->     
			{ok, #state{ parent = Parent, name = Name, generator = Gen, sub_generators = List, children = [] } }
	end.


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


%% Item level, creating and adding the data 
handle_call( {add, Key, Item}, _From, #state{ generator = undefined, children = Children } = State) ->
	case Children of
		[] ->
			case bctree_item:start_item( self(), [] ) of
				{ok, Pid} ->
					bctree_item:add( Pid, Key, Item ),
					{reply, {ok, Key}, State#state{ children = [{item, Pid}|Children] } };
				{error, Reason} -> {reply, {error, Reason}, State}
			end;
		[{item, Pid}] ->
			bctree_item:add( Pid, Key, Item ),
			{reply, {ok, Key}, State };
		Other -> {reply, Other, State }
	end;

%% Creating bucket, not yet finished with all generators
handle_call( {add, Key, Item}, _From, #state{ generator = Generator, sub_generators = SubGen, children = Children } = State) ->
	NameKey = Generator( Key ),
	case lists:keyfind( NameKey, 2, Children ) of
		false ->

			case bctree_bucket:start_bucket( self(), NameKey, State#state.sub_generators, [] ) of
				{ok, Pid} -> 

					bctree_bucket:add_item( Pid, Key, Item ),
					{reply, {ok, Key}, State#state{ children = [{bucket, NameKey, Pid}|Children] } };

				{error, Reason} -> 
					{reply, {error, Reason}, State}
			end;
		
		{bucket, NameKey, Pid} ->
			bctree_bucket:add_item( Pid, Key, Item ),
			{reply, {ok, Key}, State}

	end;
	


handle_call( {del, Key, Item}, _From, State ) ->
	{reply, {ok, Key}, State};

handle_call( {exists, Key} , _From, #state{ generator = Generator } = State ) ->
	NameKey = Generator( Key ),
	{reply, {ok, NameKey}, State};


handle_call( {clear}, _From, State ) ->
	case x_teminate_children( State#state.children ) of
		ok -> {reply, ok, State#state{ children = [] } }
	end;


handle_call( {set, generator, Gen}, _From, State ) -> {reply, ok, State#state{ generator = Gen } };

handle_call( {get,generator}, _From, State ) -> {reply, {ok, State#state.generator}, State };
handle_call( {get,sub_generator}, _From, State ) -> {reply, {ok, State#state.sub_generators}, State };
handle_call( {get,parent}, _From, State ) -> {reply, {ok, State#state.parent}, State };
handle_call( {get,name}, _From, State ) -> {reply, {ok, State#state.name}, State };
handle_call( {get,size}, _From, State ) -> 
	case x_calc_size( State#state.children ) of
		Size -> {reply, {ok, Size }, State }
	end;

handle_call( {get, item, _}, _From, #state{ generator = undefined, children = []} = State ) ->
	{reply, {ok, false}, State};


handle_call( {get, item, Key}, _From, #state{ generator = undefined, children = Children} = State ) ->
	try lists:nth( 1, Children ) of
		{item, Pid} -> 
			case bctree_item:get( Pid, Key ) of
				false -> {reply, {ok, false}, State};
				{ok, Data} -> {reply, {ok, Data}, State};
				{error,Reason} -> {reply, {error, Reason}, State}
			end;
		_Other -> {reply, {ok, false}, State}
	catch 
		Error:Reason ->
			{reply, {ok, false}, State}
	end;
	

handle_call( {get, item, Key}, _From, #state{ generator = Generator, children = Children} = State ) ->
	NameKey = Generator( Key ),
	case lists:keyfind( NameKey, 2, Children ) of
		{bucket, NameKey, Pid} ->
			case bctree_bucket:get_item( Pid, Key ) of
				{ok, Data} -> {reply, {ok, Data}, State};
				{error, Reason} -> {reply, {error, Reason}, State}
			end;
		_Other ->
			{reply, {ok, false}, State}
	end;


handle_call( {stop, Reason}, _From, State ) ->
	
	{stop, Reason, ok, State};

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


handle_info( {'EXIT', Pid, Reason}, State ) ->
	{noreply, State};

handle_info( {'DOWN', MonitorReference, Process, Pid, Reason}, State ) ->
	{noreply, State};



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
   	x_teminate_children( State#state.children ).

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

x_teminate_children( [] ) -> ok;

x_teminate_children( [{item, Pid}|List] ) -> 
	bctree_item:stop( Pid ),
	x_teminate_children( List );

x_teminate_children( [{bucket, _Name, Pid}|List] ) -> 
	bctree_bucket:stop( Pid ),
	x_teminate_children( List );

x_teminate_children( [_|List] ) -> x_teminate_children( List ).


x_calc_size( Children ) -> x_calc_size( Children, [] ).

x_calc_size( [], Result ) -> lists:flatten( Result );

x_calc_size( [{item, Pid}|List], Result ) ->
	case bctree_item:size( Pid ) of
		{ok, Size} -> x_calc_size( List, [Size|Result] );
		{error, Reason} -> x_calc_size( List, Result )
	end;

x_calc_size( [{bucket, Name, Pid}|List], Result ) ->
	case bctree_bucket:get_size( Pid ) of
		{ok, Size} -> x_calc_size( List, [Size|Result] );
		{error, Reason} -> x_calc_size( List, Result )
	end;

x_calc_size( [Child|List], Result ) ->
	x_calc_size( List, Result ).


