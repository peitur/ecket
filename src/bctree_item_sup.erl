-module( bctree_item_sup ).
-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-define( SERVER, ?MODULE ).

-export([
		start_link/0,
		start_link/1,
		start_item/2
	]).

start_link( ) -> start_link( [] ).
start_link( Arg ) -> supervisor:start_link( {local, ?SERVER}, ?MODULE, Arg ).

start_item( Parent, Options ) ->
	supervisor:start_child( ?SERVER, [Parent, Options] ).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->

    Item = {'bctree_item',{'bctree_item',start_link,[]}, temporary,2000,worker,['bctree_item']},

    {ok,{{simple_one_for_one,0,1}, [Item]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
