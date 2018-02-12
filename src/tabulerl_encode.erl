-module(tabulerl_encode).

-include("tabulerl.hrl").

-export([encode/2, encode_password/2, encode_ucs2/1]).

encode(prelogin, Opts) ->
    Version = <<16#0B, 16#00, 16#0C, 16#38, 16#00, 16#00>>,
    V_size = byte_size(Version),
    Offset = 16#06,
    V_meta = <<16#00, Offset:2/unit:8, V_size:2/unit:8>>,
    Encryption = 16#00,
    Terminator = 16#FF,
    Data = <<V_meta/binary, Terminator, Version/binary>>,
    encode_packs(?pack_prelogin, Data, []);

%% TODO: lots of flags and stuff to clean up here
encode(login, #{user := U, pass := P, host := Host, port := Port, database := Db}) ->
    Version = <<16#04, 16#00, 16#00, 16#74>>,
    Size = <<?pack_size:4/little-unit:8>>,
    Client_ver = <<16#00:4/unit:8>>,
    Client_pid = <<16#00, 16#10, 16#00, 16#00>>,
    Cnx_id = <<16#00:4/unit:8>>,
    Version_data = <<Version/binary, Size/binary, Client_ver/binary, Client_pid/binary, Cnx_id/binary>>,
    
    Opt_flags1 = 16#00,
    Opt_flags2 = 16#00,
    Type_flag = 16#01,
    Opt_flags3 = 16#00,
    Time_zone = <<16#00:4/little-unit:8>>,
    Lcid = <<16#00:4/little-unit:8>>,
    Flags_data = <<Opt_flags1, Opt_flags2, Type_flag, Opt_flags3, Time_zone/binary, Lcid/binary>>,

    User = encode_ucs2(U),
    Pass = encode_password(encode_ucs2(P), []),
    Client_lib = encode_ucs2("TERL"),
    Database = encode_ucs2(Db),
    Login_data = <<User/binary, Pass/binary, Client_lib/binary, Database/binary>>,

    %% TODO: unmess all these offset calculations
    %% calculate offset stuff starting with the size of the version and flags data at the beginning
    %% of the packet + 4 bytes + 58 bytes for the length of the offset data
    Offset_start = byte_size(Version_data) + byte_size(Flags_data) + 4 + 58,

    Ib_host = <<Offset_start:2/little-unit:8>>,
    Cch_host = <<16#00:2/little-unit:8>>,

    User_size = round(byte_size(User) / 2),
    Ib_user = <<Offset_start:2/little-unit:8>>,
    Cch_user = <<User_size:2/little-unit:8>>,

    Pass_size = round(byte_size(Pass) / 2),
    Pass_offset = Offset_start + byte_size(User),
    Ib_pass = <<Pass_offset:2/little-unit:8>>,
    Cch_pass = <<Pass_size:2/little-unit:8>>,

    Clilib_size = round(byte_size(Client_lib) / 2),
    Clilib_offset = Pass_offset + byte_size(Pass),
    Ib_clilib = <<Clilib_offset:2/little-unit:8>>,
    Cch_clilib = <<Clilib_size:2/little-unit:8>>,

    Database_size = round(byte_size(Database) / 2),
    Database_offset = Clilib_offset + byte_size(Client_lib),
    Ib_database = <<Database_offset:2/little-unit:8>>,
    Cch_database = <<Database_size:2/little-unit:8>>,

    Offset = <<
               Ib_host/binary,
               Cch_host/binary,
               Ib_user/binary,
               Cch_user/binary,
               Ib_pass/binary,
               Cch_pass/binary,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               Ib_clilib/binary,
               Cch_clilib/binary,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               Ib_database/binary,
               Cch_database/binary,
               16#00:6/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:2/little-unit:8,
               16#00:4/little-unit:8
             >>,

    Body_data = <<Version_data/binary, Flags_data/binary, Offset/binary, Login_data/binary>>,
    Body_size = byte_size(Body_data) + 4,
    Data = <<Body_size:4/little-unit:8, Body_data/binary>>,

    encode_packs(?pack_login, Data, []).

encode_header(Type, Data, Status) ->
    Id = 0,
    Size = byte_size(Data) + 8,
    <<Type, Status, Size:2/unit:8, ?spid, Id, ?window>>.

%% recurse through the data, breaking it into chunks of 4088 bytes,
%% appending a header to each chunk, and returning a list of the packets,
%% then reverse the list so the packets are in the correct order.
encode_packs(_, <<>>, Packs) ->
    Ordered_pass = lists:reverse(Packs),
    list_to_bitstring(Ordered_pass);

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

encode_password(<<>>, Pbits) ->
    Pass = lists:reverse(Pbits),
    list_to_bitstring(Pass);

encode_password(<<Pass:8/bitstring, Tail/binary>>, Acc) ->
    <<A:4/bitstring, B:4/bitstring>> = Pass,
    <<C>> = <<B/bitstring, A/bitstring>>,
    P = C bxor 16#A5,
    encode_password(Tail, [<<P>> | Acc]).

encode_ucs2(Data) ->
    L = xmerl_ucs:to_ucs2le(Data),
    list_to_binary(L).
