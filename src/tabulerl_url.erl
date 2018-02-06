-module(tabulerl_url).

-export([parse/1]).

-record(url, {
          host = ""     :: string(),
          port          :: integer(),
          path = <<>>   :: binary(),
          user = <<>>   :: binary(),
          pass = <<>>   :: binary()
         }).

parse(Url) ->
    ok.
