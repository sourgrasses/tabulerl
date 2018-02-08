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

%% some consts for packets
%% packets are 4KB in size, with 8 byte headers and 4,088 bytes of space for data
-define(pack_data_size, 4088).
-define(pack_header_size, 8).

%% types and statuses for packet headers
-define(pack_sqlbatch, 16#01).
-define(pack_oldlogin, 16#02).
-define(pack_rpc, 16#03).
-define(pack_result, 16#04).
-define(pack_attention, 16#06).
-define(pack_bulk_load, 16#07).
-define(pack_federated_token, 16#08).
-define(pack_manager_request, 16#0E).
-define(pack_login, 16#10).
-define(pack_sspi, 16#11).
-define(pack_prelogin, 16#12).

-define(status_normal, 16#00).
-define(status_eom, 16#01).
-define(status_reset, 16#08).
-define(status_reset_skip, 16#10).

-define(spid, 16#0000:16).

-define(window, 16#00).

%% data types
