%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Functor Type Class.
%%% The Functor type class represents types that can be mapped over. Type
%%% constructors can become part of the Functor type class by providing an
%%% implementation of the `fmap` function. See here for more details:
%%% https://wiki.haskell.org/Functor
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(s2_functors).

%%%_* Macros ==================================================================
-define(is_F(F), is_function(F, 1)).

%%%_* Exports =================================================================
-export([fmap/2]).
-export_type([functor/1]).

%%%_* Types ===================================================================
%% The following type constructors are instances of the `functor` type class.
%% Note that types must be without overlap for any arbitrary value of `A`.
%%
%% As a result, we have to choose between making either `list` or `dict` an
%% instance of functor. That's because a dict `[{ok, 2}, {error, 3}]` is
%% indiscernable from a list of `maybe(integer(), integer())`.
%%
%% Similarly, we have to choose between making `tuple` an instance of functor,
%% or implementing different clauses of `fmap` for tuple-based types such as
%% `maybe`.
-type functor(A) :: [A]
                    | #{_ := A}
                    | fun((_) -> A)
                    | s2_maybe:maybe(A, _).

%%%_* Code ====================================================================
-spec fmap(fun((A) -> B), functor(A)) -> functor(B).
%%@doc fmap(F, Functor) is the result of calling F on every value in Functor.
fmap(F, List)               when ?is_F(F), is_list(List) -> lists:map(F, List);
fmap(F, Map)                when ?is_F(F), is_map(Map)   -> maps:map(fun(_, V) -> F(V) end, Map);
fmap(F, Fun)                when ?is_F(F), ?is_F(Fun)    -> s2_funs:o(F, Fun);
fmap(F, {ok, _} = Maybe)    when ?is_F(F)                -> s2_maybe:fmap(F, Maybe);
fmap(F, {error, _} = Maybe) when ?is_F(F)                -> s2_maybe:fmap(F, Maybe).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%_* Functor Laws ------------------------------------------------------------
%% Add functors to the following list to make sure that their implementation of
%% `fmap` satisfies the functor laws. Note that if the tests pass this does not
%% guarantee that the laws hold true, but only that they aren't violated for
%% the particular test case.
%% 
%% The laws should hold true for any value of `A`. We're testing with
%% `A = {ok, 2}` to ensure that the tests cover the potential conflict
%% between `list` and `dict` described above.
functors(A) ->
  [ [A]
  , #{key => A}
  , fun(_) -> A end
  , {ok, A}
  , {error, reason}].

%% Law 1: Mapping the identity function over a functor does not change it
preserve_identity_morphism_test() ->
  CheckLaw = fun(Functor) -> assertEqual(Functor, fmap(fun s2_funs:id/1, Functor)) end,
  lists:foreach(CheckLaw, functors({ok, 2})).

%% Law 2: If two functions F and G are mapped over a functor sequentially, the
%% result must be the same as mapping it with a single function that is
%% equivalent to applying the first function to the result of the second.
preserve_composition_of_morphisms_test() ->
  F        = fun({ok, Value}) -> {ok, Value - 1}                              end,
  G        = fun({ok, Value}) -> {ok, Value * 2}                              end,
  Composed = fun(Functor) -> fmap(s2_funs:o(F, G), Functor)                   end,
  Chained  = fun(Functor) -> fmap(F, fmap(G, Functor))                        end,
  CheckLaw = fun(Functor) -> assertEqual(Chained(Functor), Composed(Functor)) end,
  lists:foreach(CheckLaw, functors({ok, 2})).

%%%_* Other Tests -------------------------------------------------------------
basic_test() ->
  F = fun(X) -> X + 1 end,
  ?assertEqual([2, 3],            fmap(F, [1, 2])),
  ?assertEqual(#{a => 2, b => 3}, fmap(F, #{a => 1, b => 2})),
  ?assertEqual(2,                 (fmap(F, fun(_) -> 1 end))(arg)).

%%%_* Test Helpers ------------------------------------------------------------
assertEqual(Expected, Actual) when is_function(Expected), is_function(Actual) ->
  assertEqual(Expected(arg), Actual(arg));
assertEqual(Expected, Actual) ->
  ?assertEqual(Expected, Actual).

-endif.

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
