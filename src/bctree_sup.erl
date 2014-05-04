%   bctree, concurrent b-tree:ish search, main application supervisor
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

