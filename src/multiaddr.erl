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

-module(multiaddr).

-include("maddr_protocol.hrl").

-type multiaddr() :: binary().
-type protocol() :: {string(), string() | undefined}.

-export_type([multiaddr/0, protocol/0]).

-ignore_xref({new,1}).
-ignore_xref({to_string,1}).

-export([new/1, to_string/1, protocols/1]).

-spec new(string()) -> multiaddr().
new(Str) when is_list(Str) ->
    Trimmed = string:strip(Str, right, $/),
    try
        case string:split(Trimmed, "/", all) of
            [[] | Rest] ->  encode_string(Rest, <<>>);
            _ -> throw({error, missing_prefix})
        end
    catch
        throw:{error, _} -> error(bad_arg)
    end.

-spec to_string(multiaddr() | [protocol()]) -> string().
to_string(Addr) when is_binary(Addr) ->
    try
        decode_bytes(Addr, "")
    catch
        throw:{error, _} -> error(bad_arg)
    end;
to_string(Protocols) when is_list(Protocols) ->
    try
        decode_protocols(Protocols)
    catch
        throw:{error, _} -> error(bad_arg)
    end.

-spec protocols(multiaddr() | string()) -> [protocol()].
protocols(Addr) ->
    try
        protocols(Addr, [])
    catch
        throw:{error, _} -> error(bad_arg)
    end.

protocols(<<>>, Acc) ->
    Acc;
protocols(Bytes, Acc) when is_binary(Bytes) ->
    {Code, Tail} = small_ints:decode_varint(Bytes),
    case maddr_protocol:for_code(Code) of

        #protocol{name=Name, size=Size, path=Path,
                  transcoder=#transcoder{decode=Decode}} when not Path andalso Size > 0 ->
            <<EncodedAddress:Size/bitstring, Rest/binary>> = Tail,
            [{Name, Decode(EncodedAddress) } | protocols(Rest, Acc)];
        #protocol{name=Name, size=Size, path=Path,
                  transcoder=#transcoder{decode=Decode}} when not Path andalso Size < 0 ->
            {EncodedSize, Tail1} = small_ints:decode_varint(Tail),
            case Tail1 of
                <<EncodedAddress:EncodedSize/binary, Remainder/binary>> ->
                    [{Name, Decode(EncodedAddress)} | protocols(Remainder, Acc)];
                _ -> throw({error, {invalid_address_lengh, EncodedSize}})
            end;
        #protocol{name=Name, path=Path, transcoder=#transcoder{decode=Decode}} when Path ->
            [{Name, Decode(Tail)}];
        #protocol{name=Name, size=Size} when Size == 0 ->
            [{Name, undefined} | protocols(Tail, Acc)];
        {error, Error} ->
            throw({error, Error})
    end;
protocols(Str, Acc) ->
    protocols(multiaddr:new(Str), Acc).


encode_string([], Acc) ->
    Acc;
encode_string([Protocol | Tail], Acc) ->
    case maddr_protocol:for_name(Protocol) of
        #protocol{name=Name, code=Code, size=Size, path=Path,
                  transcoder=#transcoder{encode=Encode}} when not Path andalso Size /= 0 ->
            case Tail of
                [Address | Rest] ->
                    << (small_ints:encode_varint(Code))/binary, (Encode(Address))/binary,
                       (encode_string(Rest, Acc))/binary>>;
                _ -> throw({error, {missing_address, Name}})
            end;
        #protocol{code=Code, path=Path, transcoder=#transcoder{encode=Encode}} when Path ->
            Remainder = string:join(Tail, "/"),
            <<(small_ints:encode_varint(Code))/binary, (Encode(Remainder))/binary>>;
        #protocol{code=Code, size=Size} when Size == 0 ->
            <<(small_ints:encode_varint(Code))/binary, (encode_string(Tail, Acc))/binary>>;
        {error, Error} ->
            throw({error, Error})
    end.


decode_bytes(<<>>, Acc) ->
    Acc;
decode_bytes(Bytes, Acc) ->
    {Code, Tail} = small_ints:decode_varint(Bytes),
    case maddr_protocol:for_code(Code) of
        #protocol{name=Name, size=Size, path=Path,
                  transcoder=#transcoder{decode=Decode}} when not Path andalso Size > 0 ->
            <<EncodedAddress:Size/bitstring, Rest/binary>> = Tail,
            "/" ++ Name ++ "/" ++ Decode(EncodedAddress) ++ decode_bytes(Rest, Acc);
        #protocol{name=Name, size=Size, path=Path,
                  transcoder=#transcoder{decode=Decode}} when not Path andalso Size < 0 ->
            {EncodedSize, Tail1} = small_ints:decode_varint(Tail),
            case Tail1 of
                <<EncodedAddress:EncodedSize/binary, Remainder/binary>> ->
                    "/" ++ Name ++ "/" ++ Decode(EncodedAddress) ++ decode_bytes(Remainder, Acc);
                _ -> throw({error, {invalid_address_lengh, EncodedSize}})
            end;
        #protocol{name=Name, path=Path, transcoder=#transcoder{decode=Decode}} when Path ->
            "/" ++ Name ++ "/" ++ Decode(Tail);
        #protocol{name=Name, size=Size} when Size == 0 ->
            "/" ++ Name ++ decode_bytes(Tail, Acc);
        {error, Error} ->
            throw({error, Error})
    end.

decode_protocols([]) ->
    "";
decode_protocols([{Addr, undefined} | Rest]) ->
    "/" ++ Addr ++ decode_protocols(Rest);
decode_protocols([{Addr, Value} | Rest]) ->
    "/" ++ Addr ++ "/" ++ Value ++ decode_protocols(Rest).
