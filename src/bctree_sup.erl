-module( bctree_sup ).
-behaviour(supervisor).


-export([init/1]).

-define( SERVER, ?MODULE ).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, stop/0]).





start_link( Args ) ->
	supervisor:start_link( {local, ?SERVER}, ?MODULE, Args ).

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
    MemStore = {'bctree_memstore',{'bctree_memstore',start_link,[]}, permanent,2000,worker,['bctree_memstore']},

    BucketSup = {'bctree_bucket_sup',{'bctree_bucket_sup',start_link,[]}, permanent,2000,supervisor,['bctree_bucket_sup']},
    ItemSup = {'bctree_item_sup',{'bctree_item_sup',start_link,[]}, permanent,2000,supervisor,['bctree_item_sup']},

    Children = [MemStore, BucketSup],

    {ok,{{one_for_all,0,1}, Children }}.

%% ====================================================================
%% Internal functions
%% ====================================================================

