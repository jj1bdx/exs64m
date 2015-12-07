%% (MIT License)
%%
%% Copyright (c) 2015 Kenji Rikitake. All rights reserved.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of
%% this software and associated documentation files (the "Software"), to deal in
%% the Software without restriction, including without limitation the rights to
%% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(exs64m_speed).

-export([test_speed/0]).

-define(REP, 100000).
-define(LEN, 1000).

repeat(0, _F) -> ok;
repeat(N, F) ->
    F(),
    repeat(N-1, F).

test_speed_next_list(N) ->
    _ = statistics(runtime),
    ok = repeat(?REP,
                    fun() -> exs64m:next_list(N, exs64m:seed0()) end),
    {_, T} = statistics(runtime),
    T.

test_speed_nif_next_list(N) ->
    _ = statistics(runtime),
    ok = repeat(?REP,
                    fun() -> exs64m:nif_next_list(N, exs64m:seed0()) end),
    {_, T} = statistics(runtime),
    T.
    
-spec test_speed() -> 'ok'.

test_speed() ->
    io:format("{next_list, nif_next_list}~n~p~n",
              [{test_speed_next_list(?LEN),
                test_speed_nif_next_list(?LEN)}]).
