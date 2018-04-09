%%%-------------------------------------------------------------------
%%% @author Carl Nordin (manisvart)
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2017 16:45
%%%-------------------------------------------------------------------
-module(teip).
-author("manisvart").

%% API

-export([encode/1, decode/1]).

% Encode Erlang terms to the Intermediate format (IF)

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

decode(#{<<"integer">> := Data}) ->
  Data;

decode(#{<<"float">> := Data}) ->
  Data;

decode(#{<<"atom">> := Data}) ->
  binary_to_existing_atom(Data, 'unicode');

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


