-module(tabulerl_url).

-export([]).

-record(url, {
          host = ""     :: string(),
          port          :: integer(),
          path = <<>>   :: binary(),
          user = <<>>   :: binary(),
          pass = <<>>   :: binary()
         }).
