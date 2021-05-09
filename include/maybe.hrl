%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Maybe Monads
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Types ============================================================
-type functor(A)    :: s2_functors:functor(A).
-type thunk(A)      :: fun(() -> A).
-type collection(A) :: [A] | #{_ := A}.

-type ok(A)       :: {ok, A}.
-type error(A)    :: {error, A}.
-type maybe(A, B) :: ok(A) | error(B).
-type whynot(A)   :: ok | error(A).

%%%_* Macros ===========================================================
-define(lift(E),   s2_maybe:lift(fun() -> E end)).
-define(unlift(E), s2_maybe:unlift(fun() -> E end)).

-define(do(F0, F1),
        s2_maybe:do([F0, F1])).
-define(do(F0, F1, F2),
        s2_maybe:do([F0, F1, F2])).
-define(do(F0, F1, F2, F3),
        s2_maybe:do([F0, F1, F2, F3])).
-define(do(F0, F1, F2, F3, F4),
        s2_maybe:do([F0, F1, F2, F3, F4])).
-define(do(F0, F1, F2, F3, F4, F5),
        s2_maybe:do([F0, F1, F2, F3, F4, F5])).
-define(do(F0, F1, F2, F3, F4, F5, F6),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7, F8),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7, F8])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7, F8, F9),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7, F8, F9])).

-define(fmap(F, Functor),
        s2_functors:fmap(F, Functor)).

-define(ido(F0, F1),
        ?ido([F0, F1])).
-define(ido(F0, F1, F2),
        ?ido([F0, F1, F2])).
-define(ido(F0, F1, F2, F3),
        ?ido([F0, F1, F2, F3])).
-define(ido(F0, F1, F2, F3, F4),
        ?ido([F0, F1, F2, F3, F4])).
-define(ido(F0, F1, F2, F3, F4, F5),
        ?ido([F0, F1, F2, F3, F4, F5])).
-define(ido(F0, F1, F2, F3, F4, F5, F6),
        ?ido([F0, F1, F2, F3, F4, F5, F6])).
-define(ido(F0, F1, F2, F3, F4, F5, F6, F7),
        ?ido([F0, F1, F2, F3, F4, F5, F6, F7])).
-define(ido(F0, F1, F2, F3, F4, F5, F6, F7, F8),
        ?ido([F0, F1, F2, F3, F4, F5, F6, F7, F8])).
-define(ido(F0, F1, F2, F3, F4, F5, F6, F7, F8, F9),
        ?ido([F0, F1, F2, F3, F4, F5, F6, F7, F8, F9])).

%% Instrumented do.
-define(ido(Fs),
        (case ?TIME(?FUNCTION, s2_maybe:do(Fs)) of
           {ok, ___Res} = ___Ok ->
             ?debug("~p: ok: ~p", [?FUNCTION, ___Res]),
             ?increment(?FUNCTION, ok),
             ___Ok;
           {error, ___Rsn} = ___Err ->
             ?error("~p: error: ~p", [?FUNCTION, ___Rsn]),
             ?increment(?FUNCTION, error),
             ___Err
         end)).

-define(thunk(E0),
        fun() -> E0 end).
-define(thunk(E0, E1),
        fun() -> E0, E1 end).
-define(thunk(E0, E1, E2),
        fun() -> E0, E1, E2 end).
-define(thunk(E0, E1, E2, E3),
        fun() -> E0, E1, E2, E3 end).
-define(thunk(E0, E1, E2, E3, E4),
        fun() -> E0, E1, E2, E3, E4 end).
-define(thunk(E0, E1, E2, E3, E4, E5),
        fun() -> E0, E1, E2, E3, E4, E5 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6),
        fun() -> E0, E1, E2, E3, E4, E5, E6 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7, E8),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7, E8 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7, E8, E9 end).

-define(liftm(F, A1),
        s2_maybe:liftm(F, [?thunk(A1)])).
-define(liftm(F, A1, A2),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2)])).
-define(liftm(F, A1, A2, A3),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3)])).
-define(liftm(F, A1, A2, A3, A4),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4)])).
-define(liftm(F, A1, A2, A3, A4, A5),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4),
                           ?thunk(A5)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4),
                           ?thunk(A5), ?thunk(A6)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4),
                           ?thunk(A5), ?thunk(A6), ?thunk(A7)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4),
                           ?thunk(A5), ?thunk(A6), ?thunk(A7), ?thunk(A8)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8, A9),
        s2_maybe:liftm(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4),
                           ?thunk(A5), ?thunk(A6), ?thunk(A7), ?thunk(A8),
                           ?thunk(A9)])).

%%%_* Guards ===========================================================
-define(is_thunk(X), is_function(X, 0)).

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
