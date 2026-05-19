%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Atoms.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_atoms).

%%%_* Exports ==========================================================
-export([catenate/1]).

%%%_* Includes =========================================================
-include("prelude.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Code =============================================================
-spec catenate([atom() | integer() | float() | string()]) -> atom().
%% @doc catenate(Args) is the concatenation of Args as an atom.
catenate(Args) ->
  ?l2a(lists:concat(lists:foldr(fun(X, Acc) ->
                                  [s2_lists:to_list(X) | Acc] end, [], Args))).

-ifdef(TEST).
catenate_test() ->
  'foo426.66000000000000000000e+02bar' = catenate([foo, 42, 666.0, "bar"]).
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
