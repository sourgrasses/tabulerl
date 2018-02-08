-module(tabulerl_encode).

-include("tabulerl.hrl").

-export([]).

encode(prelogin, Opts) ->
    Version = << 16#0B, 16#00, 16#0C, 16#38, 16#00, 16#00 >>,
    V_size = bytesize(Version),
    Offset = 16#06,
    V_meta = << 16#00, Offset:16, V_size:16 >>,
    Encryption = 16#00,
    Terminator = 16#FF,
    Data = << V_meta/binary, Terminator, Version/binary >>
    Size = byte_size(Data),
    Header = encode_header(?pack_prelogin, Size, Opts).

encode_header(Type, Size, Opts) ->
    Status = 1,
    Id = 1,
    << Type, Status, Size:16, ?spid, id, ?window >>.

encode_packs(Type, Packs) ->
    ok.
