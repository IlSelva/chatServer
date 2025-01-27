# chatServer
simple erlang chat server

# How can i run?
Compile: `rebar3 compile`

Run `rebar3 shell`

Run server: `chatServer_server:start(<PORT>).`

# How to interact
You can interact with the server sending TCP messages

## Known messages
`CONNECT:<NICKNAME>` try connecting with the server

`LIST` get a list of the connected user

`QUIT` Disconnect from the server.