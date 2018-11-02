%%% ___________________________________________________________________________Welcome
%%% @doc
%%% Encode/decode Erlang terms to the Intermediate protocol (TEIP).
%%% @end
%%% ---------------------------------------------------------------------------
%%% @author Carl Nordin (manisvart)
%%% @copyright (C) 2018,Carl Nordin (manisvart)
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% ---------------------------------------------------------------------------
-module(teip).
-author("Carl Nordin (manisvart)").

%%%____________________________________________________________________________Module API exports
-export([encode/1, decode/1]).

%% @doc Encode Erlang terms to the Intermediate protocol (TEIP)
%% ____________________________________________________________________________Function
-spec encode(any()) -> map().

encode(Integer) when is_integer(Integer) ->
  #{atom_to_binary('integer', 'unicode') => Integer};

encode(Float) when is_float(Float) ->
  #{atom_to_binary('float', 'unicode') => Float};

encode(Atom) when is_atom(Atom) ->
  #{atom_to_binary('atom', 'unicode') => atom_to_binary(Atom, 'unicode')};

encode(Tuple) when is_tuple(Tuple) ->
  TempList = tuple_to_list(Tuple),

  Elements = lists:foldl(
    fun(Element, Acc) ->
      lists:append(Acc, [encode(Element)])
    end,

    [], TempList),

  #{atom_to_binary('tuple', 'unicode') => Elements};

encode(Map) when is_map(Map) ->
  TempList = maps:fold(

    fun(K, V, Acc) ->
      lists:append(
        Acc,
        [#{atom_to_binary('key', 'unicode') => encode(K),
          atom_to_binary('value', 'unicode') => encode(V)
        }]
      )
    end,

    [], Map),

  #{atom_to_binary('map', 'unicode') => TempList};


encode(List) when is_list(List) ->
  % A list, or a string...
  list_encode(List, io_lib:latin1_char_list(List)).

list_encode(String, 'true') ->
  % A string
  #{atom_to_binary('string', 'unicode') => list_to_binary(String)};

list_encode(List, 'false') ->
  % A list
  Elements = lists:foldl(
    fun(Element, Acc) ->
      lists:append(Acc, [encode(Element)])
    end,

    [], List),

  #{atom_to_binary('list', 'unicode') => Elements}.


% Decode the Intermediate format (IF) to real Erlang terms

%% @doc Decode the Intermediate protocol (TEIP) to Erlang terms
%% ____________________________________________________________________________Function
-spec decode(any()) -> any().

decode(#{<<"integer">> := Data}) ->
  Data;

decode(#{<<"float">> := Data}) ->
  Data;

decode(#{<<"atom">> := Data}) ->
  binary_to_atom(Data, 'unicode');

decode(#{<<"string">> := Data}) ->
  binary_to_list(Data);


decode(#{<<"tuple">> := Data}) ->
  Tuple = lists:foldl(
    fun(Element, Acc) ->
      Acc ++ [decode(Element)]
    end,

    [], Data),

  list_to_tuple(Tuple);

decode(#{<<"list">> := Data}) ->
  List = lists:foldl(
    fun(Element, Acc) ->
      Acc ++ [decode(Element)]
    end,

    [], Data),

  List;

decode(#{<<"map">> := Data}) ->
  Map = lists:foldl(
    fun(Element, Acc) ->
      Key = maps:get(<<"key">>, Element),
      Value = maps:get(<<"value">>, Element),
      maps:put(decode(Key), decode(Value), Acc)
    end,

    #{}, Data),

  Map.


