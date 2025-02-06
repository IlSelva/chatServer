# chatServer
simple erlang chat server

# How to run?
Compile: `rebar3 compile`

Run `rebar3 shell`

Run server: `chatServer_server:start(<PORT>).`

# How to interact
You can interact with the server sending TCP messages

You can use `telnet <HOST> <PORT>` to send requests to the server. 

## Known messages
`CONNECT:<NICKNAME>` try connecting with the server

`LIST` get a list of the connected user

`ROOMS` get a list of the rooms

`CREATE:<ROOM_NAME>` create a new room

`DESTROY:<ROOM_NAME>` destroy the room if the user created it

`JOIN:<ROOM_NAME>` join the room

`LEAVE:<ROOM_NAME>` leave the room

`SAY:<MESSAGE>` send a message in the current room

`QUIT` Disconnect from the server.