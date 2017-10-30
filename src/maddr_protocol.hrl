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

