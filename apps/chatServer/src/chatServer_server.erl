-module(chatServer_server).
-behaviou(gen_server).

-export([start/1, on_connection/1]).

-define(SERVER, ?MODULE).
-define(CONTROLLER, chatServer_controller).

start(Port) ->
    io:format("Server starting ~n"),
    ?CONTROLLER:start(),
    tcp_server:start(?MODULE, Port, {?MODULE, on_connection}).

on_connection(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            {Command, [_|Nick]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
            CleanNick = clean(Nick),
            io:format("Nick: ~p~n", [CleanNick]),
            case Command of
                "CONNECT" ->
                    try_connection(CleanNick, Socket);
                _ ->
                    gen_tcp:send(Socket, "Unknown command!\n"),
                    ok
            end;
        {error, closed} ->
            ok
    end.

try_connection(Nick, Socket) ->
    Response = gen_server:call(?CONTROLLER, {connect, Nick, Socket}),
    case Response of
        {ok, List} ->
            gen_tcp:send(Socket, "CONNECT:OK\n"),
            gen_server:cast(?CONTROLLER, {join, Nick}),
            handle_command(Nick, Socket);
        nick_in_use ->
            gen_tcp:send(Socket, "CONNECT:ERROR:Nick in use.\n"),
            ok
    end.

handle_command(Nick, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [binary_to_list(Data)]),
            Message = binary_to_list(Data),
            case lists:member($:, Message) of
                false ->
                    case clean(Message) of
                        "LIST" ->
                            list(Nick, Socket);
                        "QUIT" ->
                            quit(Nick, Socket)
                        end;
                true ->
                    {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
                    case Command of
                        "QUIT" ->
                            quit(Nick, Socket)
                    end
            end;
        {error, closed} ->
            ok
    end.    

list(Nick, Socket) ->
    Users = gen_server:call(?CONTROLLER,  {list_users}),
    gen_tcp:send(Socket, "Users list: "++ Users ++ "\n"),
    handle_command(Nick, Socket).

quit(Nick, Socket) ->
    Response = gen_server:call(?CONTROLLER, {disconnect, Nick}),
    case Response of
        ok ->
            gen_tcp:send(Socket, "Bye.\n"),
            gen_server:cast(?CONTROLLER, {left, Nick}),
            ok;
        user_not_found ->
            gen_tcp:send(Socket, "Bye with errors.\n"),
            ok
    end.

clean(Data) ->
    string:trim(Data).
 