-module(tabulerl_tcp).

-export([connect/1]).

-record(packet, {
          sock = null,
          usock = null,
          itcp = null,
          opts = null,
          state = ready,
          tail,
          pack_header,
          pack_data,
          result = null,
          query = null,
          transaction = null,
          env
         }).

%% @doc Establish a connection to a SQL Server host. Takes a url,
%% parses it using tabulerl_url:parse/1, and returns an inet:socket().
%% @see gen_tcp:connect/3
-spec connect(tabulerl_url:url())
    -> {ok, inet:socket()} | {error, string()}.
connect(Url) ->
    {Host, Port, Options} = tabulerl_url:parse(Url),
    gen_tcp:connect(Host, Port, Options).

