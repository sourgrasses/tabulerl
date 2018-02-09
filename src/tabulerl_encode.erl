-module(tabulerl_encode).

-include("tabulerl.hrl").

-export([encode/2, encode_password/2]).

encode(prelogin, Opts) ->
    Version = <<16#0B, 16#00, 16#0C, 16#38, 16#00, 16#00>>,
    V_size = byte_size(Version),
    Offset = 16#06,
    V_meta = <<16#00, Offset:16, V_size:16>>,
    Encryption = 16#00,
    Terminator = 16#FF,
    Data = <<V_meta/binary, Terminator, Version/binary>>,
    encode_packs(?pack_prelogin, Data, []);

encode(login, {user=U, pass=P, host=Host, port=Port, database=Db}) ->
    Version = <<16#04, 16#00, 16#00, 16#74>>,
    User = unicode:characters_to_binary(U, utf16, {utf16, little}),
    Xord_pass = encode_password(unicode:characters_to_binary(P, utf16, {utf16, little}), []),
    Pass = list_to_bitstring(Xord_pass),
    Data = <<User/binary, Pass/binary>>,
    encode_packs(?pack_login, Data, []).

encode_header(Type, Data, Status) ->
    Id = 0,
    Size = byte_size(Data) + 8,
    <<Type, Status, Size:16, ?spid, Id, ?window>>.

%% recurse through the data, breaking it into chunks of 4088 bytes,
%% appending a header to each chunk, and returning a list of the packets,
%% then reverse the list so the packets are in the correct order.
encode_packs(_, <<>>, Packs) ->
    lists:reverse(Packs);

encode_packs(Type, <<Data:?pack_data_size/bitstring, Tail/binary>>, Packs) ->
    case byte_size(Tail) > 0 of
        true -> Status = 0;
        false -> Status = 1
    end,
    Header = encode_header(Type, Data, Status),
    encode_packs(Type, Tail, [<<Header/binary, Data/binary>> | [Packs]]);

encode_packs(Type, <<Data/binary>>, Packs) ->
    Header = encode_header(Type, Data, 1),
    encode_packs(Type, <<>>, [<<Header/binary, Data/binary>> | [Packs]]).

encode_password(<<>>, Pass) ->
    lists:reverse(Pass);

encode_password(<<Pass:8/bitstring, Tail/binary>>, Acc) ->
    <<A:4/bitstring, B:4/bitstring>> = Pass,
    <<C>> = <<B/bitstring, A/bitstring>>,
    P = C bxor 16#A5,
    encode_password(Tail, [<<P>> | Acc]).
