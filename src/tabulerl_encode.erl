-module(tabulerl_encode).

-export([]).

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

%% @doc Encodes login information.
login() ->
    ok.

query() ->
    ok.

pack(Message) ->
    ok.
