# exs64m: experimental mini repository for Xorshift64\* in Erlang and NIF

This is an experimental code of Xorshift64\* PRNG for Erlang/OTP 18 and later.

[rebar](https://github.com/rebar/rebar) is required to compile the code.

## How to run

```
./speed-test.sh
```

## Experimental result

On Mac Mini Late 2012 with Core i7, OS X 10.11.1, Erlang/OTP 18.1.5, and
Xcode 7.1.1, the average runtime for 100000 time execution of generating
1000 numbers of PRNG results in:

* ~38 seconds for `exs64m:next_list/2` without NIF
* ~1.5 seconds for `exs64m:nif_next_list/2` with NIF
* Speedup rate: ~x25

## License

MIT License.

## References

* [exs64](https://github.com/jj1bdx/exs64)
* [sfmt-erlang](https://github.com/jj1bdx/sfmt-erlang), for NIF programming
