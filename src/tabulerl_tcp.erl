%% @doc Mostly just wraps around a handful of gen_tcp and inet functions

-module(tabulerl_tcp).

-export([connect/1, close/1]).

%% @doc Establish a connection to a SQL Server host. Takes a url, parses it using tabulerl_url:parse/1, and returns an inet:socket().
-spec connect(tabulerl_url:url())
    -> {ok, inet:socket()} | {error, string()}.
connect(Url) ->
    {Host, Port, Options} = tabulerl_url:parse(Url),
    gen_tcp:connect(Host, Port, Options, infinity).

%% @doc Send a packet on the socket Sock.
-spec send(inet:socket(), iolist())
    -> ok | {error, atom()}.
send(Sock, Packet) ->
    gen_tcp:send(Sock, Packet).

%% @doc Receive a packet from a socket Sock.
-spec recv(inet:socket(), non_neg_integer())
    -> {ok, string() | binary() | term()} | {error, atom() | inet:posix()}.
recv(Sock, Length) ->
    gen_tcp:recv(Sock, Length, infinity).

%% @doc Close a TCP socket.
-spec close(inet:socket())
    -> ok.
close(Sock) ->
    gen_tcp:close(Sock).

%% @doc Shuts down a TCP socket by signaling to the peer that no more data will be sent.
%% Socket can still be read from until peer closes connection.
-spec shutdown(inet:socket())
    -> ok | {error, inet:posix()}.
shutdown(Sock) ->
    gen_tcp:shutdown(Sock, write).

%% @doc Assign a new controlling process to a socket.
-spec controlling_process(inet:socket(), pid())
	-> ok | {error, atom()}.
controlling_process(Sock, Pid) ->
    gen_tcp:controlling_process(Sock, Pid).

%% @doc Get the port for a given socket.
-spec port(inet:socket())
    -> {ok, inet:port_number()} | {error, any()}.
port(Sock) ->
    inet:port(Sock).

%% @doc Get the address and port for a given socket connection.
-spec peername(inet:socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Sock) ->
    inet:peername(Sock).

%% @doc Get the local address and port for a given socket connection.
-spec localname(inet:socket())
    -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
localname(Sock) ->
    inet:sockname(Sock).
