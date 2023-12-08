%%%-------------------------------------------------------------------
%% @doc roundrobin top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(roundrobin_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional


% generate_spec(Module, Type, Name, {Identifiers, Node_name}=Args) ->
%     #{
%         id => Module,
%         start => {Module, start, [Identifiers, Node_name]},
%         restart => permanent,
%         shutdown => 2000,
%         type => Type,
%         modules => [Module]
%     }.

% generate_spec(rrobin_serv, worker, rrobin_serv, [['backend@142.93.16.138', 'backend@138.68.15.146'],'rrobin_serv@165.232.48.38'])


init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 500,
                 period => 10},
    ChildSpecs = [#{
                    id => rrobin_serv,
                    start => {rrobin_serv, start, [["backend2@157.230.128.7", "backend@104.248.176.39"]]},
                    restart => permanent,
                    shutdown => 2000,
                    type => worker,
                    modules => [rrobin_serv]
                }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
