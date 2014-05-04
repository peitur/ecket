-module( bctree_bucket_sup).
-behaviour(supervisor).

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start_link/1,
	stop/0, 
	start_bucket/4
]).

-define( SERVER, ?MODULE ).

start_link( Arg ) ->
	supervisor:start_link( {local,?SERVER}, ?MODULE, Arg ).

%% Name, name of bucket, unique identifier
%% GeneratorList, a list of generator functions, takes the first in the list and creates a new bukcket with rest of list
%% This means that there is a level per generator element, when bottom is reached, all will be placed in bctree_item processes
start_bucket( Parent, Name, GeneratorList, Options ) ->
	supervisor:start_child( ?SERVER, [Parent, Name, GeneratorList, Options ]).

stop( ) -> ok.

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

    Bukcet = {'bctree_bucket',{'bctree_bucket',start_link,[]}, temporary,2000,worker,['bctree_bucket']},

    {ok,{{simple_one_for_onel,0,1}, [Bukcet] }}.

%% ====================================================================
%% Internal functions
%% ====================================================================

