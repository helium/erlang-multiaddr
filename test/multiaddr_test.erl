-module(multiaddr_test).

-include_lib("eunit/include/eunit.hrl").

encode_success_test() ->
    Cases = ["/ip4/1.2.3.4",
             "/ip4/0.0.0.0",
             "/ip6/::1",
             "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21",
             "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21/udp/1234/quic",
             "/onion/timaq4ygg2iegci7:1234",
             "/onion/timaq4ygg2iegci7:80/http",
             "/udp/0",
             "/tcp/0",
             "/sctp/0",
             "/udp/1234",
             "/tcp/1234",
             "/sctp/1234",
             "/udp/65535",
             "/tcp/65535",
             "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC",
             "/udp/1234/sctp/1234",
             "/udp/1234/udt",
             "/udp/1234/utp",
             "/tcp/1234/http",
             "/tcp/1234/https",
             "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234",
             "/ip4/127.0.0.1/udp/1234",
             "/ip4/127.0.0.1/udp/0",
             "/ip4/127.0.0.1/tcp/1234",
             "/ip4/127.0.0.1/tcp/1234/",
             "/ip4/127.0.0.1/udp/1234/quic",
             "/ip4/127.0.0.1/p2p/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC",
             "/ip4/127.0.0.1/p2p/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234",
             "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC",
             "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234",
             "/unix/a/b/c/d/e",
             "/unix/stdio",
             "/ip4/1.2.3.4/tcp/80/unix/a/b/c/d/e/f",
             "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234/unix/stdio"
             "/ip4/1.2.3.4/tcp/80/p2p-circuit/a/b/c/d/e/f",
             "/ip4/1.2.3.4/tcp/80/p2p-circuit/ip4/1.2.3.4/tcp/80"
            ],
    lists:map(fun(Case) ->
                      Address = multiaddr:new(Case),
                      ?assertNotMatch({error, _}, Address)
              end, Cases).

encode_fail_test() ->
    Cases = ["/ip4",
             "/ip4/::1",
             "/ip4/fdpsofodsajfdoisa",
             "/ip6",
             "/udp",
             "/tcp",
             "/sctp",
             "/udp/65536",
             "/tcp/65536",
             "/quic/65536",
             "/onion/9imaq4ygg2iegci7:80",
             "/onion/aaimaq4ygg2iegci7:80",
             "/onion/timaq4ygg2iegci7:065536",
             "/onion/timaq4ygg2iegci7:-1",
             "/onion/timaq4ygg2iegci7",
             "/onion/timaq4ygg2iegci@:666",
             "/udp/1234/sctp",
             "/udp/1234/udt/1234",
             "/udp/1234/utp/1234",
             "/ip4/127.0.0.1/udp/jfodsajfidosajfoidsa",
             "/ip4/127.0.0.1/udp",
             "/ip4/127.0.0.1/tcp/jfodsajfidosajfoidsa",
             "/ip4/127.0.0.1/tcp",
             "/ip4/127.0.0.1/quic/1234",
             "/ip4/127.0.0.1/p2p",
             "/ip4/127.0.0.1/ipfs",
             %% "/ip4/127.0.0.1/ipfs/tcp", %% "tcp" passes base58:base58_check
             "/unix",
             "/ip4/1.2.3.4/tcp/80/unix"
            ],
    lists:map(fun(Case) ->
                      ?assertError(bad_arg, multiaddr:new(Case))
              end, Cases).


encode_binary_test() ->
    Cases = [ {"/ip4/127.0.0.1/udp/1234", "047f000001910204d2"},
              {"/ip4/127.0.0.1/tcp/4321", "047f0000010610e1"},
              {"/ip4/127.0.0.1/udp/1234/ip4/127.0.0.1/tcp/4321", "047f000001910204d2047f0000010610e1"},
              {"/onion/aaimaq4ygg2iegci:80", "bc030010c0439831b48218480050"}
            ],
    lists:map(fun({Str, Enc}) ->
                      Address = multiaddr:new(Str),
                      Encoded = string:uppercase(Enc),
                      ?assertEqual(Encoded, bin_to_hexstr(Address)),
                      ?assertEqual(Address, multiaddr:new(multiaddr:to_string(hexstr_to_bin(Enc))))
              end, Cases).


protocols_test() ->
    Address = multiaddr:new("/ip4/127.0.0.1/udp/1234"),
    Protocols = multiaddr:protocols(Address),
    ?assertEqual(Protocols, multiaddr:protocols("/ip4/127.0.0.1/udp/1234")),
    ?assertEqual(2, length(Protocols)),
    ?assertMatch([{"ip4", "127.0.0.1"}, {"udp", "1234"}], Protocols),
    ?assertEqual(Address, multiaddr:new(multiaddr:to_string(Protocols))).


%% from http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).
