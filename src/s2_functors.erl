%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Functor Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_functors).

%%%_* Exports ==========================================================
-export([fmap/2]).

%%%_* Includes =========================================================
-include("prelude.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Types ============================================================
-type functor(A) :: maybe(A, _)   |
                    [A]           |
                    #{_ := A}     |
                    fun((_) -> A) |
                    {A}.

%%%_* Code =============================================================
-spec fmap(fun((A) -> B), functor(A)) -> functor(B).
%%@doc fmap(F, Functor) is the result of calling F on every value in Functor.
fmap(F, {ok, _} = Maybe)    when is_function(F, 1)                      -> s2_maybe:fmap(F, Maybe);
fmap(F, {error, _} = Maybe) when is_function(F, 1)                      -> s2_maybe:fmap(F, Maybe);
fmap(F, List)               when is_function(F, 1), is_list(List)       -> lists:map(F, List);
fmap(F, Map)                when is_function(F, 1), is_map(Map)         -> maps:map(fun(_K, V) -> F(V) end, Map);
fmap(F, Fun)                when is_function(F, 1), is_function(Fun, 1) -> s2_funs:o(F, Fun);
fmap(F, Tuple)              when is_function(F, 1), is_tuple(Tuple)     -> list_to_tuple(fmap(F, tuple_to_list(Tuple))).

-ifdef(TEST).

%%%_* Tests ============================================================
%% Add functors to this list to make sure that they satify the functor
%% laws. The laws should hold true for any `Value`.
functors(Value) ->
  [ {ok, Value}
  , [Value]
  , #{key => Value}
  , fun(arg) -> Value end
  , {Value}].

assertEqual(Expected, Actual) when is_function(Expected), is_function(Actual) ->
  assertEqual(Expected(arg), Actual(arg));
assertEqual(Expected, Actual) ->
  ?assertEqual(Expected, Actual).

preserve_identity_morphism_test() ->
  CheckLaw = fun(Functor) -> assertEqual(Functor, fmap(fun s2_funs:id/1, Functor)) end,
  lists:foreach(CheckLaw, functors({ok, 2})).

preserve_composition_of_morphisms_test() ->
  F        = fun({ok, Value}) -> {ok, Value - 1}                              end,
  G        = fun({ok, Value}) -> {ok, Value * 2}                              end,
  Composed = fun(Functor) -> fmap(s2_funs:o(F, G), Functor)                   end,
  Chained  = fun(Functor) -> fmap(F, fmap(G, Functor))                        end,
  CheckLaw = fun(Functor) -> assertEqual(Chained(Functor), Composed(Functor)) end,
  lists:foreach(CheckLaw, functors({ok, 2})).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
