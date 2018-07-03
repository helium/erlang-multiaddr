%% Copyright 2017 Marc Nijdam <marc@helium.com>

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%%     http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(maddr_protocol).

-include("maddr_protocol.hrl").

-type protocol() :: #protocol{}.

-export_type([protocol/0]).

-ignore_xref({new,4}).

-export([new/4, for_name/1, for_code/1]).

-define(P_IP4  , 16#0004).
-define(P_TCP  , 16#0006).
-define(P_UDP  , 16#0111).
-define(P_DCCP , 16#0021).
-define(P_IP6  , 16#0029).
-define(P_QUIC , 16#01CC).
-define(P_SCTP , 16#0084).
-define(P_UDT  , 16#012D).
-define(P_UTP  , 16#012E).
-define(P_UNIX , 16#0190).
-define(P_P2P  , 16#01A4).
-define(P_IPFS , 16#01A5).
-define(P_HTTP , 16#01E0).
-define(P_HTTPS, 16#01BB).
-define(P_ONION, 16#01BC).
-define(P_P2P_CIRCUIT, 16#0122).

-spec new(string(), pos_integer(), pos_integer(), transoders:transcoder()) -> protocol().
new(Name, Code, Size, Transcoder) ->
    #protocol{name=Name, code=Code, size=Size, transcoder=Transcoder}.

-spec for_name(string()) -> protocol() | {error, term()}.
for_name(Name) ->
    for_name(Name, supported()).

-spec for_name(string(), [protocol()]) -> protocol() | {error, term()}.
for_name(Name, []) ->
    {error, {unsupported_protocol, Name}};
for_name(Name, [Protocol=#protocol{name=Name} | _]) ->
    Protocol;
for_name(Name, [_ | Tail]) ->
    for_name(Name, Tail).

-spec for_code(non_neg_integer()) -> protocol() | {error, term()}.
for_code(Code) ->
    for_code(Code, supported()).

-spec for_code(non_neg_integer(), [protocol()]) -> protocol() | {error, term()}.
for_code(Code, []) ->
    {error, {unsupported_protocol, Code}};
for_code(Code, [Protocol=#protocol{code=Code} | _]) ->
    Protocol;
for_code(Code, [_ | Tail]) ->
    for_code(Code, Tail).


-spec supported() -> [protocol()].
supported() ->
    [#protocol{name="ip4",   code=?P_IP4,    size=32,  transcoder=maddr_transcoder:ip4()},
     #protocol{name="tcp",   code=?P_TCP,    size=16,  transcoder=maddr_transcoder:port()},
     #protocol{name="udp",   code=?P_UDP,    size=16,  transcoder=maddr_transcoder:port()},
     #protocol{name="dccp",  code=?P_DCCP,   size=128, transcoder=maddr_transcoder:port()},
     #protocol{name="ip6",   code=?P_IP6,    size=128, transcoder=maddr_transcoder:ip6()},
     #protocol{name="sctp",  code=?P_SCTP,   size=16,  transcoder=maddr_transcoder:port()},
     #protocol{name="onion", code=?P_ONION,  size=96,  transcoder=maddr_transcoder:onion()},
     #protocol{name="utp",   code=?P_UTP,    size=0,   transcoder=undefined},
     #protocol{name="udt",   code=?P_UDT,    size=0,   transcoder=undefined},
     #protocol{name="quic",  code=?P_QUIC,   size=0,   transcoder=undefined},
     #protocol{name="http",  code=?P_HTTP,   size=0,   transcoder=undefined},
     #protocol{name="https", code=?P_HTTPS,  size=0,   transcoder=undefined},
     #protocol{name="p2p",   code=?P_P2P ,   size=-1,  transcoder=maddr_transcoder:p2p()},
     #protocol{name="ipfs",  code=?P_IPFS,   size=-1,  transcoder=maddr_transcoder:ipfs()},
     #protocol{name="unix",  code=?P_UNIX,   size=-1,  transcoder=maddr_transcoder:unix(), path=true},
     #protocol{name="p2p-circuit", code=?P_P2P_CIRCUIT, size=-1, transcoder=maddr_transcoder:p2p_circuit(), path=true}
    ].
