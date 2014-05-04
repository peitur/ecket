${file_comment}

-module(${module}).
-behaviour(supervisor_bridge).
-export([init/1, terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([${exports}]).

${exported_functions}

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1 
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor_bridge.html#Module:init-1">supervisor_bridge:init/1</a>
-spec init(Args :: term()) -> Result :: {ok, Pid :: pid(), State :: term()}
										| ignore
										| {error, Error :: term()}. 
%% ====================================================================
init([]) ->
    case 'AModule':start_link() of
	{ok, Pid} ->
	    {ok, Pid, #state{}};
	Error ->
	    Error
    end.

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor_bridge.html#Module:terminate-2">supervisor_bridge:termiante/2</a>
-spec terminate(Reason :: shutdown | term(), State :: term()) -> Any :: term().
%% ====================================================================
terminate(Reason, State) ->
    'AModule':stop(),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

${local_functions}