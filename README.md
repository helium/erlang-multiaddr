# erlang-multiaddr

An Lang implementation for [multiaddr](https://github.com/multiformats/multiaddr). 

## Usage

```
multiaddr:new("/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21")
```

The encoder is idempotent:

```
Str = "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21",
Str = multiaddr:to_string(multiaddr:new("/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21")).
```

