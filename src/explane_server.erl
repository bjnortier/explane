-module(explane_server).
-behaviour(gen_server).
-export([start_link/0, state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Port} = application:get_env(explane, udp_listen_port),
    {ok, Socket} = gen_udp:open(Port, []),
    {ok, #state{socket = Socket}}.

state() ->
    gen_server:call(?MODULE, state).

%% callbacks
handle_call(state, _From, State) ->
    io:format("server state: ~p~n", [State]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
