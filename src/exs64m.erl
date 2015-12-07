%% @author Kenji Rikitake <kenji.rikitake@acm.org>
%% @copyright 2014-2015 Kenji Rikitake
%% @doc Xorshift64star for Erlang (minimal version)
%% @end
%% (MIT License)
%%
%% Copyright (c) 2014-2015 Kenji Rikitake. All rights reserved.
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
%%

-module(exs64m).

-on_load(load_nif/0).

-export([
     next/1,
     next_list/2,
     seed0/0,
     get_lib_refc/0,
     nif_next_list/2
 ]).

-export_type([
        state/0,
        uint64/0
    ]).

%% @type uint64(). 64bit unsigned integer type.

-type uint64() :: 0..16#ffffffffffffffff.

%% @type state(). Internal state data type for exs64.
%% Internally represented as the record <code>#state{}</code>,
%% of the 128bit seed.

-opaque state() :: uint64().

-define(UINT32MASK, 16#ffffffff).
-define(UINT39MASK, 16#0000007fffffffff).
-define(UINT64MASK, 16#ffffffffffffffff).

%% @doc Advance xorshift64star state for one step.
%% and generate 64bit unsigned integer from
%% the xorshift64star internal state.

-spec next(state()) -> {uint64(), state()}.

next(R) ->
    R1 = R bxor (R bsr 12),
    R2 = R1 bxor ((R1 band ?UINT39MASK) bsl 25),
    R3 = R2 bxor (R2 bsr 27),
    {(R3 * 2685821657736338717) band ?UINT64MASK, R3}.

-spec seed0() -> state().

%% @doc Set the default seed value to xorshift64star state
%% in the process directory (Compatible with random:seed0/0).

seed0() -> 1234567890123456789.

-spec next_list(pos_integer(), state()) -> {[uint64()], state()}.

next_list(N, S) when is_integer(N), N > 0 ->
    next_list(N, S, []).

next_list(0, S, A) -> {lists:reverse(A), S};
next_list(N, S, A) ->
    {V, S2} = next(S),
    next_list(N-1, S2, [V|A]).

%% NIF macros

-define(nif_stub, nif_stub_error(?LINE)).
-define(NIF_LOAD_INFO, 101).

nif_stub_error(Line) ->
        erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

%% @doc returns NIF library reference count.
%% (Note: NIFnized)

-spec get_lib_refc() -> integer().

get_lib_refc() -> ?nif_stub.

-spec nif_next_list(pos_integer(), state()) -> {[uint64()], state()}.

nif_next_list(_, _) -> ?nif_stub.

%% On-load callback

%% @doc Loading NIF shared library file, used at the on-load callback.

load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "exs64m_nif"), ?NIF_LOAD_INFO).
