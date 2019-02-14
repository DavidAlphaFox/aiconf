-module(aiconf_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    Server = #{ id => ai_conf_server,
								start => {ai_conf_server, start_link, []},
								restart => transient,
								shutdown => 5000,
								type => worker,
								modules => [ai_conf_server]}, 
    {ok, {SupFlags,[Server]}}.
