%%% @copyright 2012 Casey Rosenthal, All rights reserved. Open source, BSD License
%%% @version 0.1
%%%
%%%
%%% Copyright (c) 2012 Casey Rosenthal
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 3. Neither the name of the copyright holder nor the names of contributors
%%%    may be used to endorse or promote products derived from this software
%%%    without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%

-module(erl_hiredis).
-export([ping/0]).
-export([get/1]).
-export([put/5]).
-export([delete/4]).
-export([drop/0]).
-version(0.1).
-on_load(init/0).

%% @spec init() -> ok
%% @doc Initialize hiredis NIF.
init() ->
    SoName = filename:join(case code:priv_dir(?MODULE) of
                               {error, bad_name} ->
                                   %% this is here for testing purposes
                                   filename:join(
                                     [filename:dirname(
                                        code:which(?MODULE)),"..","priv"]);
                               Dir ->
                                   Dir
                           end, atom_to_list(?MODULE) ++ "_nif"),
    erlang:load_nif(SoName, 0),
    ok.

ping() ->
  {error, "Called Erlang instead of C."}.

get(Uid) ->
  {error, "Called Erlang instead of C."}.

put(Bucket, Key, World, Value, Uid) ->
  {error, "Called Erlang instead of C."}.

delete(Bucket, Key, World, Uid) ->
  {error, "Called Erlang instead of C."}.

drop() ->
  {error, "Called Erlang instead of C."}.
