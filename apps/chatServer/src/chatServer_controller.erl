-module(chatServer_controller).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).

-record(state, {
  users,          %users = Username : {Socket,currentRoom}
  rooms           %rooms = RoomName : {Owner, list(members)}
}).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% This is called when a connection is made to the server
init([]) ->
  Rooms = dict:store("Home", {"_", []}, dict:new()),
  State = #state{users = dict:new(), rooms = Rooms},

  {ok, State}.

% handle_call is invoked in response to gen_server:call
handle_call({connect, Nick, Socket}, _From, State) ->
  Users = State#state.users,
  Rooms = State#state.rooms,

  Response = case dict:is_key(Nick, Users) of
               true ->
                 NewState = State,
                 nick_in_use;
               false ->
                 NewUsers = dict:store(Nick, {Socket, "Home"}, Users),

                 %%add user to room members
                 {ok, {Owner, Members}} = dict:find("Home", Rooms),
                 NewMembers = [Nick | Members],
                 NewRooms = dict:store("Home", {Owner, NewMembers}, Rooms),

                 NewState = State#state{users = NewUsers, rooms = NewRooms},
                 ok
             end,
  {reply, Response, NewState};

handle_call({disconnect, Nick}, _From, State) ->
  Users = State#state.users,
  Response = case dict:is_key(Nick, Users) of
               true ->
                 %handle_cast({leave_room, Nick),
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
                 {error, "Room: " ++ Name ++ " already exists"};
               false ->
                 NewRooms = dict:store(Name, {Nick, []}, Rooms),
                 NewState = State#state{rooms = NewRooms},
                 ok
             end,
  {reply, Response, NewState};

handle_call({destroy_room, Nick, Name}, _From, State) ->
  Rooms = State#state.rooms,
  Response = case dict:find(Name, Rooms) of
               {ok, {Owner, _}} ->
                 case Nick =:= Owner of
                   true ->
                     %%TODO leve_room for every user in the room
                     %delete room
                     NewRooms = dict:erase(Name, Rooms),
                     NewState = State#state{rooms = NewRooms},
                     ok;
                   false ->
                     NewState = State,
                     {error, "You are not the owner of the room"}
                 end;
               error ->
                 NewState = State,
                 {error, "Room: does not exists"}
             end,
  {reply, Response, NewState};

handle_call({join_room, Nick, Name}, _From, State) ->
  Rooms = State#state.rooms,
  Users = State#state.users,
  %TODO check if Nick is already in room
  Response = case dict:is_key(Name, Rooms) of
               true ->
                 %%add user to room members
                 {ok, {Owner, Members}} = dict:find(Name, Rooms),
                 NewMembers = [Nick | Members],
                 NewRooms = dict:store(Name, {Owner, NewMembers}, Rooms),

                 %%update user's current room
                 {ok, {Socket, _}} = dict:find(Nick, Users),
                 NewUsers = dict:store(Nick, {Socket, Name}, Users), %% check if append overwrites the old value
                 NewState = State#state{users = NewUsers, rooms = NewRooms},
                 ok;
               false ->
                 NewState = State,
                 {error, "Room:" ++ Name ++ " not found"}
             end,
  {reply, Response, NewState};

handle_call({leaver_room, Nick}, _From, State) ->
  Rooms = State#state.rooms,
  Users = State#state.users,
  {ok,{_, CurrentRoom}} = dict:find(Nick, Users),
  Response = case dict:find(CurrentRoom, Rooms) of
               {ok, {Owner, RoomMembers}} ->
                 %check if nick is in room
                 %clear user's current room
                 {ok,{Socket, _}} = dict:find(Nick, Users),
                 NewUsers = dict:store(Nick, {Socket, null}, Users), %% check if append overwrites the old value

                 %remove user from room members
                 NewRoomMembers = list:delete(Nick, RoomMembers),
                 NewRooms = dict:store(Rooms, {Owner, NewRoomMembers}, Rooms),
                 NewState = State#state{users = NewUsers, rooms = NewRooms},
                 ok;
               error ->
                 NewState = State,
                 {error, "Room not found"}
             end,
  {reply, Response, NewState};

handle_call(_Message, _From, State) ->
  {reply, error, State}.

% handle_cast is invoked in response to gen_server:cast

handle_cast({join, Nick}, State) ->
  broadcast_room(Nick, "JOIN:" ++ Nick ++ "\n", State),
  {noreply, State};

handle_cast({left, Nick}, State) ->
  %%NOTIFY ONLY USERS IN THE SAME ROOM
  broadcast_room(Nick, "LEFT:" ++ Nick ++ "\n", State),
  {noreply, State};

handle_cast({say, Nick, Msg}, State) ->
  broadcast_room(Nick, "SAID:" ++ Nick ++ ":" ++ Msg ++ "\n", State),
  {noreply, State};

handle_cast(_Message, State) ->
  {noreply, State}.

broadcast_room(Nick, Msg, State) ->
  Users = State#state.users,
  Rooms = State#state.rooms,

  %get user curremt room
  {ok, {_Socket, CurrentRoom}} = dict:find(Nick, Users),
  %get room members
  {ok, {_Owner, RoomMembers}} = dict:find(CurrentRoom, Rooms),

  FilteredMembers = lists:delete(Nick, RoomMembers),
  case length(RoomMembers) > 1 of
    true ->
      Sockets = [Socket ||
        Nickname <- FilteredMembers,
        {ok, {Socket, _CurrentRoom}} <- [dict:find(Nickname, Users)]
      ],

      lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Msg) end, Sockets);
    _ ->
      ok
  end.

user_list(Users) ->
  UserList = dict:fetch_keys(Users),
  string:join(UserList, ":").

room_list(Rooms) ->
  RoomList = dict:fetch_keys(Rooms),
  string:join(RoomList, ":").

% Definitions to avoid gen_server compile warnings
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.