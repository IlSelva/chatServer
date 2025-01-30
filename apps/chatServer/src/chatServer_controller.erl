-module(chatServer_controller).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

% users: a dictionary of Nickname:Socket
% rooms: a dictionary of RoomName:list of User in the room
%TODO rooms must contain also the room owner (creator's nickname). I could do rooms as a dictionari of
% RoomName:record(list of users, owner). So that is easy to check if a room exists and access it.
-record(state, {
    users,
    rooms}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% This is called when a connection is made to the server
init([]) ->
    State = #state{users = dict:new(), rooms = dict:new()},
    {ok, State#state.users}.

% handle_call is invoked in response to gen_server:call
handle_call({connect, Nick, Socket}, _From, State) ->
    Users = State#state.users,
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewState = State,
            nick_in_use;
        false ->
            NewUsers = dict:append(Nick, Socket, Users),
            NewState = State#state{users = NewUsers},
            {ok, user_list(NewUsers)}
    end,
    {reply, Response, NewState};

handle_call({disconnect, Nick}, _From, State) ->
    Users = State#state.users,
    Response = case dict:is_key(Nick, Users) of
        true ->
            NewUsers = dict:erase(Nick, Users),
            NewState = State#state{users = NewUsers},
            ok;
        false ->
            NewState = State,
            user_not_found
    end,
    {reply, Response, NewState};

handle_call({list_users}, _From, State) ->
    {reply, user_list(State#state.users), State};

handle_call({list_rooms}, _From, State) ->
    {reply, room_list(State#state.rooms), State};

handle_call({create_room, Nick, Name}, _From, State) ->
    Rooms = State#state.rooms,
    Response = case dict:is_key(Name, Rooms) of
                   true ->
                       NewState = State,
                       {error, "Room: "++ Name++ " already exists"};
                   false ->
                       NewRooms = dict:store(Name,[], Rooms),
                       NewState = State#state{rooms = NewRooms},
                       ok
               end,
    {reply, Response, NewState};

handle_call({destroy_room}, _From, State) ->
    %TODO add room owner (creator)
    {};

handle_call({join_room, Nick, Name}, _From, State) ->
    Rooms = State#state.rooms,
    Response = case dict:is_key(Name, Rooms) of
                 true ->
                   %check if Nick is already in room
                    NewRooms = dict:append(Name, Nick, Rooms),
                    NewState = State#state{rooms = NewRooms},
                   ok;
                 false ->
                   NewState = State,
                   {error, "Room: "++ Name ++ "not found"}
               end,
  {reply, Response, NewState};

handle_call(_Message, _From, State) ->
    {reply, error, State}.


% handle_cast is invoked in response to gen_server:cast

handle_cast({join, Nick}, Users) ->
    broadcast(Nick, "JOIN:" ++ Nick ++ "\n", Users),
    {noreply, Users};

handle_cast({left, Nick}, Users) ->
    broadcast(Nick, "LEFT:" ++ Nick ++ "\n", Users),
    {noreply, Users};

handle_cast(_Message, State) ->
    {noreply, State}.


% auxiliary functions
broadcast(Nick, Msg, Users) ->
    %%remove the calling user from the dictionary before converting it to list
    Sockets = lists:map(fun({_, [Value|_]}) -> Value end, dict:to_list(dict:erase(Nick, Users))),
    lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets).

user_list(Users) ->
    UserList = dict:fetch_keys(Users),
    string:join(UserList, ":").

% Definitions to avoid gen_server compile warnings
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.