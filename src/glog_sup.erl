%%%-------------------------------------------------------------------
%% @doc glog top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(glog_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_udp_transport/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_udp_transport(Name, Config) ->
    ChildSpec = #{
        id => Name,
        start => {glog_udp, start_link, [Name, Config]},
        restart => transient,
        shutdown => 300,
        type => worker,
        modules => [glog_udp]
    },
    
    supervisor:start_child(?SERVER, ChildSpec).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1
    },
    
    {ok, {SupFlags, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
