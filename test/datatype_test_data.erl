%%% Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the copyright holders nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

-module(datatype_test_data).

-export([get_test_data/0]).


get_test_data() ->
    lists:append([
        integer_types(),
        big_integer_types(),
        float_types(),
        atom_types(),
        list_types(),
        improper_list_types(),
        tuple_types(),
        binary_types(),
        opaque_types()
        ]).

integer_types() ->
    [0, 1, -1, 100000, -100000].

big_integer_types() ->
    [1000000000000, -1000000000000, 2 bsl 2040, -2 bsl 2040].

float_types() ->
    [0.0, 0.5, -0.5, 3.1415926, -3.1415926].

atom_types() ->
    ['', test, 'TEST', true, false, undefined].

list_types() ->
    [[], [0], [1, 2, 3], [[]], [[], [], []], lists:seq(1, 10000), lists:seq(-10,10)].

improper_list_types() ->
    [[0 | 1], [1, 2, 3 | 4], [head | tail]].

tuple_types() ->
    [{}, {0}, {1, 2, 3}, {{}}, {{}, {}}, list_to_tuple(lists:seq(1, 10000))].

binary_types() ->
    [<<>>, <<"test">>, <<0, 1, 2, 3, 4, 5>>].

opaque_types() ->
    [self(), [{pid, self()}], <<1:2>>, make_ref(), fun erlang:is_atom/1,
        fun () -> true end].
