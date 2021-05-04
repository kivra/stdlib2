%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Maybe Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_maybe).

%%%_* Exports ==========================================================
-export([ do/1
        , fmap/2
        , lift/1
        , lift/2
        , liftM/2
        , map/2
        , reduce/2
        , reduce/3
        , sequence/1
        , to_bool/1
        , unlift/1
        , unlift/2
        ]).

%%%_* Includes =========================================================
-include("prelude.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Code =============================================================
-spec do([fun()])                            -> maybe(_, _).
%% @doc doc(Fs) is the result of chaining Fs inside the maybe monad.
do([F|Fs])                                   -> do(Fs, lift(F)).
do([],     X)                                -> X;
do(_,      {error, Rsn})                     -> {error, Rsn};
do([F|Fs], {ok, _}) when is_function(F, 0)   -> do(Fs, lift(F));
do([F|Fs], {ok, Res}) when is_function(F, 1) -> do(Fs, lift(F, Res)).

-ifdef(TEST).
do_test() ->
  {ok, 42} =
    do([ fun()  -> 1        end
       , fun(X) -> X        end
       , fun()  -> 0        end
       , fun(0) -> {ok, 41} end
       , fun(X) -> X + 1    end
       ]),
  Exn = fun() -> throw(exn) end,
  catch Exn(), %cover
  {error, foo} =
    do([ fun()  -> foo        end
       , fun(X) -> {error, X} end
       , Exn
       ]).
-endif.

-spec fmap(fun((A) -> B), maybe(A, C)) -> maybe(B, C).
%%@doc fmap(F, Maybe) is the result of mapping F over Maybe.
fmap(F, {ok, Value})     when is_function(F, 1) -> {ok, F(Value)};
fmap(F, {error, Reason}) when is_function(F, 1) -> {error, Reason}.

-ifdef(TEST).
fmap_test() ->
  {ok, 2}               = fmap(fun(X)  -> X + 1            end, {ok, 1}),
  {ok, {ok, 2}}         = fmap(fun(X)  -> {ok, X + 1}      end, {ok, 1}),
  {error, reason}       = fmap(fun(X)  -> X + 1            end, {error, reason}),
  {ok, {error, reason}} = fmap(fun(_X) -> {error, reason}  end, {ok, 1}),
  {error, reason}       = fmap(fun(_X) -> {error, reason2} end, {error, reason}).
-endif.

-spec lift(fun()) -> maybe(_, _).
%% @doc lift(F) is the value of F() lifted into the maybe monad.
lift(F) ->
  try F() of
    {ok, Res}    -> {ok, Res};
    error        -> {error, error};
    {error, Rsn} -> {error, Rsn};
    Res          -> {ok, Res}
  catch
    throw:{error, Rsn}:_ -> {error, Rsn};
    _:Exn:ST             -> {error, {lifted_exn, Exn, ST}}
  end.


lift(F, X) -> lift(?thunk(F(X))).

-spec unlift(fun()) -> _.
%% @doc unlift(F) is the result of F() extracted from the maybe monad.
unlift(F) ->
  case F() of
    {ok, Res}    -> Res;
    error        -> throw({error, error});
    {error, Rsn} -> throw({error, Rsn});
    Res          -> Res
  end.

unlift(F, X) -> unlift(?thunk(F(X))).

-ifdef(TEST).
lift_unlift_test() ->
  {ok, ok}       = ?lift(?unlift(?lift(ok))),
  {ok, ok}       = ?lift(?unlift(?lift({ok, ok}))),
  ok             = ?unlift(?lift(?unlift(ok))),
  ok             = ?unlift(?lift(?unlift({ok, ok}))),
  {error, error} = ?lift(?unlift(?lift(error))),
  {error, error} = ?lift(?unlift(?lift(throw({error, error})))),
  {error, error} = (catch ?unlift(?lift(?unlift(error)))),
  {ok, ok}       = ?lift(ok),
  {ok, 42}       = lift(fun(X) -> X end, 42),
  42             = unlift(fun(X) -> {ok, X} end, 42).
-endif.

-spec liftM(fun(), [maybe(_, B)]) -> maybe(_, B).
%% @doc lift a function F into the Maybe monad.
liftM(F, Maybes) when is_list(Maybes) and is_function(F, length(Maybes)) ->
  ?fmap(fun(Args) -> apply(F, Args) end, sequence(Maybes)).

-ifdef(TEST).
liftM_test() ->
  Add3      = fun(A, B, C) -> A + B + C end,
  ValsOK    = [{ok, 1}, {ok, 2},         {ok, 3}], 
  ValsError = [{ok, 1}, {error, reason}, {ok, 3}],
  ?assertEqual({ok, 6},         liftM(Add3, ValsOK)),
  ?assertEqual({error, reason}, liftM(Add3, ValsError)),
  ?assertEqual({ok, 6},         ?liftM(Add3, {ok, 1}, {ok, 2}, {ok, 3})).
-endif.

-spec map(fun(), [_]) -> maybe(_, _).
%%@doc map(F, Xs) is the result of mapping F over Xs inside the maybe
%% monad.
map(F, Xs) -> ?lift([?unlift(F(X)) || X <- Xs]).

-ifdef(TEST).
map_test() ->
  {ok, [1, 2]} = map(fun(X) -> X + 1       end, [0, 1]),
  {ok, [1, 2]} = map(fun(X) -> {ok, X + 1} end, [0, 1]),
  {error, _}   = map(fun(X) -> X + 1       end, [0, foo]).
-endif.


-spec reduce(fun(), [_]) -> maybe(_, _).
%% @doc reduce(F, Xs) is the result of reducing Xs to F inside the maybe
%% monad.
reduce(F, [Acc0|Xs]) ->
  reduce(F, Acc0, Xs).
reduce(F, Acc0, Xs) ->
  ?lift(lists:foldl(fun(X, Acc) -> ?unlift(F(X, Acc)) end, Acc0, Xs)).

-ifdef(TEST).
reduce_test() ->
  {ok, 1}    = reduce(fun(X, Y) -> X + Y       end, [0, 1]),
  {ok, 1}    = reduce(fun(X, Y) -> {ok, X + Y} end, [0, 1]),
  {error, _} = reduce(fun(X, Y) -> X + Y       end, [0, foo]).
-endif.

-spec sequence([maybe(A, B)]) -> maybe([A], B).
sequence(Maybes) when is_list(Maybes) ->
  ?fmap(fun lists:reverse/1,
        ?lift(s2_lists:foldl_while(fun({ok, Val}, Acc)  -> {ok, [Val | Acc]};
                                      ({error, Val}, _) -> {stop, {error, Val}}
                                   end, [], Maybes))).

-ifdef(TEST).
sequence_test() ->
  ?assertEqual({ok, [1, 2, 3]}, sequence([{ok, 1}, {ok, 2},      {ok, 3}])),
  ?assertEqual({error, foo},    sequence([{ok, 1}, {error, foo}, {error, bar}])),
  ?assertEqual({ok, []},        sequence([])).
-endif.

-spec to_bool(maybe(_, _)) -> boolean().
%% @doc to_bool(X) is the boolean representation of the maybe-value X.
to_bool({ok, _})           -> true;
to_bool({error, _})        -> false.

-ifdef(TEST).
to_bool_test() ->
  true  = to_bool({ok, foo}),
  false = to_bool({error, foo}).
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
