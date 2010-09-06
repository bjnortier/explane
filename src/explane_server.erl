-module(explane_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([add_listener/1,
         remove_listener/1,
         send/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {recv_socket, send_socket, listeners=[]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, RecvPort} = application:get_env(explane, udp_receive_port),
    {ok, SendPort} = application:get_env(explane, udp_send_port),
    {ok, RecvSocket} = gen_udp:open(RecvPort, []),
    {ok, SendSocket} = gen_udp:open(SendPort, []),
    {ok, #state{send_socket = SendSocket, recv_socket = RecvSocket}}.

add_listener(ListenerPid) ->
    gen_server:call(?MODULE, {add_listener, ListenerPid}).

remove_listener(ListenerPid) ->
    gen_server:call(?MODULE, {remove_listener, ListenerPid}).

send(Props) ->
    gen_server:cast(?MODULE, {send, Props}).

%% callbacks
handle_call({add_listener, ListenerPid}, _From, State) ->
    Listeners = State#state.listeners,
    NewListeners = [ListenerPid|Listeners],
    NewState = State#state{listeners=NewListeners},
    {reply, ok, NewState};
handle_call({remove_listener, ListenerPid}, _From, State) ->
    Listeners = State#state.listeners,
    NewListeners = lists:delete(ListenerPid, Listeners),
    NewState = State#state{listeners=NewListeners},
    {reply, ok, NewState}.


handle_cast({send, Props}, State) ->
    Packet = output_packet(Props),
    Raw = explane_packet:to_raw(Packet),
    SendSocket = State#state.send_socket,
    ok = gen_udp:send(SendSocket, {127,0,0,1}, 49000, Raw),
    {noreply, State};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

%% In active mode (the default for a gen_udp socket), incoming packets
%% are received as messages
handle_info({udp, _Socket, _FromIP, _FromPort, Raw}, State) ->
    Packet = explane_packet:from_raw(Raw),
    Values = packet_to_values(Packet),
    Listeners = State#state.listeners,
    lists:map(fun(Listener) ->
                      Listener ! {explane, Values}
              end,
              Listeners),
    {noreply, State};
%% Discard other messages
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-ifdef(TEST).
packet_to_values_test() ->
    [
     ?assertMatch(
        [
         {altitude, -1.3111454248428345},
         {heading, 347.249755859375},
         {roll, 2.7012016773223877},
         {pitch, -0.8095226883888245},
         {speed, 5.157218933105469}],
        packet_to_values(
          {"DATA4",
           [{3,
             [5.157218933105469,6.044749736785889,6.047330856323242,7.419618129730225,
              -999.0,5.934821605682373,6.959144115447998,8.538344383239746]},
            {18,
             [-0.8095226883888245,2.7012016773223877,347.249755859375,350.6871032714844,
              350.0473327636719,-999.0,-999.0,3.437335729598999]},
            {20,
             [50.78217315673828,-0.44528934359550476,-1.3111454248428345,
              -4.337009429931641,0.0,-1.3572769165039063,50.0,-2.0]}]})),

     ?assertMatch(
        [
         {altitude, -1.3111454248428345},
         {heading, 347.249755859375},
         {roll, 2.7012016773223877},
         {pitch, -0.8095226883888245}
        ],
        packet_to_values(
          {"DATA4",
           [{18,
             [-0.8095226883888245,2.7012016773223877,347.249755859375,350.6871032714844,
              350.0473327636719,-999.0,-999.0,3.437335729598999]},
            {20,
             [50.78217315673828,-0.44528934359550476,-1.3111454248428345,
              -4.337009429931641,0.0,-1.3572769165039063,50.0,-2.0]}]}))
       ].
-endif.


packet_to_values({"DATA4", Rows}) ->
    packet_to_values(Rows, []).

packet_to_values([{3, [A,_,_,_,_,_,_,_]}|Rest], Acc) ->
    Acc1 = [{speed, A}|Acc],
    packet_to_values(Rest, Acc1);
packet_to_values([{18, [A,B,C,_,_,_,_,_]}|Rest], Acc) ->
    Acc1  = [{pitch, A}|Acc],
    Acc2 = [{roll, B}|Acc1],
    Acc3 = [{heading, C}|Acc2],
    packet_to_values(Rest, Acc3);
packet_to_values([{20, [_,_,C,_,_,_,_,_]}|Rest], Acc) ->
    Acc1 = [{altitude, C}|Acc],
    packet_to_values(Rest, Acc1);
packet_to_values([_|Rest], Acc) ->
    packet_to_values(Rest, Acc);
packet_to_values([], Acc) ->
    Acc.
    

output_packet(Props) ->
    Fns = [fun joystick/1, fun throttle/1],
    {"DATA0",
     lists:map(fun(Fun) ->
                       Fun(Props)
               end,
               Fns)}.
                        
-ifdef(TEST).
joystick_test() ->
    [
     ?assertMatch(
        {8,
         [
          -999.0, -999.0, -999.0, -999.0,
          -999.0, -999.0, -999.0, -999.0]},
        joystick(
          [{}])),
     ?assertMatch(
        {8,
         [
          1,2,3,-999.0,
          -999.0,-999.0,-999.0,-999.0]},
        joystick(
          [{elevator, 1},
           {aileron, 2},
           {rudder, 3}]))
    ].

-endif.

joystick(Props) ->
    Elev = prop_value(elevator, Props),
    Ailr = prop_value(aileron, Props),
    Rudder = prop_value(rudder, Props),
    {8, [
         Elev, Ailr, Rudder, -999.0,
         -999.0,-999.0,-999.0,-999.0]}.

throttle(Props) ->
    Throttle = prop_value(throttle, Props),
    {25, [Throttle, -999.0,-999.0,-999.0,
          -999.0,-999.0,-999.0,-999.0]}.

prop_value(Key, Props) ->
    case lists:keyfind(Key, 1, Props) of
        {Key, E} ->
            E;
        _ ->
            -999.0
    end.
    
    
                                                     





    
