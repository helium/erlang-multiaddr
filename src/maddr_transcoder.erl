%% Copyright 2017 Marc Nijdam <marc@helium.com>

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%%     http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.-module(maddr_protocol).

-module(maddr_transcoder).

-include("maddr_protocol.hrl").

-export([
    ip4/0
    ,ip6/0
    ,port/0
    ,p2p/0
    ,ipfs/0
    ,onion/0
    ,unix/0
    ,p2p_circuit/0
]).


-type transcoder() :: #transcoder{}.
-type encoder() :: fun((string()) -> binary()).
-type decoder() :: fun((binary()) -> string()).

-export_type([transcoder/0, encoder/0, decoder/0]).

ip4() ->
    mktr(encode_ip(8, fun inet:parse_ipv4_address/1), decode_ip(8)).

ip6() ->
    mktr(encode_ip(16, fun inet:parse_ipv6_address/1), decode_ip(16)).

port() ->
    mktr(fun encode_port/1, fun decode_port/1).


p2p() ->
    Encode = fun(Str) ->
                     Bin = case base58:check_base58(Str) of
                               true -> base58:base58_to_binary(Str);
                               _ -> throw({error, {invalid_address, Str}})
                           end,
                     <<(small_ints:encode_varint(byte_size(Bin)))/binary, Bin/binary>>
             end,
    Decode = fun(Bin) ->
                     base58:binary_to_base58(Bin)
             end,
    mktr(Encode, Decode).

ipfs() ->
    p2p().

unix() ->
    Encode = fun(Str) ->
                     case list_to_binary(Str) of
                         Bin when byte_size(Bin) > 0 ->
                             <<(small_ints:encode_varint(byte_size(Bin)))/binary, Bin/binary>>;
                         _ -> throw({error, {invalid_address, Str}})
                     end
             end,
    Decode = fun(Bin) ->
                     {Len, Tail} = small_ints:decode_varint(Bin),
                     case Len > 0 andalso byte_size(Tail) == Len of
                         true ->
                             Str = binary_to_list(Tail),
                             string:strip(Str, left, $/);
                         false ->
                             throw({error, {invalid_length, Len}})
                     end
             end,
    mktr(Encode, Decode).


onion() ->
    Encode = fun(Str) ->
                     case string:split(Str, ":", all) of
                         [AddrStr, PortStr] when length(AddrStr) == 16 ->
                             Addr = try
                                 base32:decode(string:uppercase(AddrStr))
                             catch
                                 error:_ -> throw({error, {invalid_address, Str}})
                             end,
                             << Addr/binary, (encode_port(PortStr))/binary>>;
                         _ -> throw({error, {invalid_address, Str}})
                     end
             end,
    Decode = fun(Bin) ->
                     case Bin of
                         <<Addr:10/binary, Port:16/big-unsigned-integer>> ->
                              EncodedAddr = try
                                                base32:encode(Addr, [lower, nopad])
                                            catch
                                                error:_ -> throw({error, invalid_address})
                                            end,
                             binary_to_list(EncodedAddr) ++ ":" ++ integer_to_list(Port);
                         _ -> throw({error, invalid_address})
                     end
             end,
        mktr(Encode, Decode).


p2p_circuit() ->
    unix().


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec mktr(encoder(), decoder()) -> transcoder().
mktr(Encode, Decode) ->
    #transcoder{encode=Encode, decode=Decode}.

encode_port(Str) ->
    case string:to_integer(Str) of
        {Int, []} when Int >= 0 andalso Int < 65536 -> <<Int:16/big-unsigned-integer>>;
        _ -> throw({error, {invalid_port, Str}})
    end.

decode_port(Bin) ->
    case Bin of
        <<Int:16/big-unsigned-integer>> -> integer_to_list(Int);
        _ -> throw({error, invalid_port})
    end.

encode_ip(Size, Parse) ->
    fun(Str) ->
            case Parse(Str) of
                {ok, Address} -> << <<B:Size/big-unsigned-integer>> || B <- tuple_to_list(Address) >>;
                _ -> throw({error, {invalid_address, Str}})
            end
    end.

decode_ip(Size) ->
    fun(Bin) ->
            BinList = [ B || << B:Size/big-unsigned-integer >> <= Bin ],
            case inet:ntoa(list_to_tuple((BinList))) of
                {error, einval} -> throw({error, invalid_address});
                Result ->  Result
            end
    end.
