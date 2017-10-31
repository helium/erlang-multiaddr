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

-record(transcoder, {
          encode :: fun((string()) -> binary()),
          decode :: fun((binary()) -> string())
         }).

-record(protocol, {
          name :: string(),
          code :: pos_integer(),
          size :: integer(),
          path = false :: boolean(),
          transcoder :: #transcoder{} | undefined
         }).

