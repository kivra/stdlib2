%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Standard Erlang Prelude.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Header ===========================================================
-ifndef(__PRELUDE_HRL).
-define(__PRELUDE_HRL, true).

-include_lib("stdlib/include/assert.hrl").
-ifdef(S2_USE_OTP_LOGGER).
-include_lib("kernel/include/logger.hrl").
-endif.

-include("maybe.hrl").

%%%_* Assertions =======================================================
-define(hence(A), ?assert(A)).

-define(given(A, B),
        (case ((not (A)) orelse (B)) of
           true -> ok;
           _    -> throw({error, {assert, {??A, '->', ??B}, ?FILE, ?LINE}})
         end)).

%%%_* Casts ============================================================
-define(a2b(X), erlang:atom_to_binary(X, utf8)).
-define(a2l(X), erlang:atom_to_list(X)).
-define(l2a(X), erlang:list_to_atom(X)).
-define(b2l(X), erlang:binary_to_list(X)).
-define(l2b(X), erlang:list_to_binary(X)).
-define(b2t(X), erlang:binary_to_term(X)).
-define(t2b(X), erlang:term_to_binary(X)).
-define(i2l(X), erlang:integer_to_list(X)).
-define(f2l(X), erlang:float_to_list(X)).
-define(l2i(X), erlang:list_to_integer(X)).
-define(i2b(X), erlang:integer_to_binary(X)).
-define(b2i(X), erlang:binary_to_integer(X)).
-define(l2t(X), erlang:list_to_tuple(X)).
-define(t2l(X), erlang:tuple_to_list(X)).

%%%_* Emacs ============================================================
-define(Q,  $\'). %'
-define(QQ, $\"). %"

%%%_* Guards ===========================================================
-define(is_string(X),
        (((X) =:= "") orelse (is_list(X) andalso is_integer(hd(X))))).

%%%_* Logging ==========================================================
-ifdef(S2_USE_OTP_LOGGER).

  -define(debug(StringOrReport),                 ?LOG_DEBUG(StringOrReport)).
  -define(debug(StringOrReport, ArgsOrMeta),     ?LOG_DEBUG(StringOrReport, ArgsOrMeta)).
  -define(debug(FunOrFormat, Args, Meta),        ?LOG_DEBUG(FunOrFormat, Args, Meta)).
  -define(info(StringOrReport),                  ?LOG_INFO(StringOrReport)).
  -define(info(StringOrReport, ArgsOrMeta),      ?LOG_INFO(StringOrReport, ArgsOrMeta)).
  -define(info(FunOrFormat, Args, Meta),         ?LOG_INFO(FunOrFormat, Args, Meta)).
  -define(notice(StringOrReport),                ?LOG_NOTICE(StringOrReport)).
  -define(notice(StringOrReport, ArgsOrMeta),    ?LOG_NOTICE(StringOrReport, ArgsOrMeta)).
  -define(notice(FunOrFormat, Args, Meta),       ?LOG_NOTICE(FunOrFormat, Args, Meta)).
  -define(warning(StringOrReport),               ?LOG_WARNING(StringOrReport)).
  -define(warning(StringOrReport, ArgsOrMeta),   ?LOG_WARNING(StringOrReport, ArgsOrMeta)).
  -define(warning(FunOrFormat, Args, Meta),      ?LOG_WARNING(FunOrFormat, Args, Meta)).
  -define(error(StringOrReport),                 ?LOG_ERROR(StringOrReport)).
  -define(error(StringOrReport, ArgsOrMeta),     ?LOG_ERROR(StringOrReport, ArgsOrMeta)).
  -define(error(FunOrFormat, Args, Meta),        ?LOG_ERROR(FunOrFormat, Args, Meta)).
  -define(critical(StringOrReport),              ?LOG_CRITICAL(StringOrReport)).
  -define(critical(StringOrReport, ArgsOrMeta),  ?LOG_CRITICAL(StringOrReport, ArgsOrMeta)).
  -define(critical(FunOrFormat, Args, Meta),     ?LOG_CRITICAL(FunOrFormat, Args, Meta)).
  -define(alert(StringOrReport),                 ?LOG_ALERT(StringOrReport)).
  -define(alert(StringOrReport, ArgsOrMeta),     ?LOG_ALERT(StringOrReport, ArgsOrMeta)).
  -define(alert(FunOrFormat, Args, Meta),        ?LOG_ALERT(FunOrFormat, Args, Meta)).
  -define(emergency(StringOrReport),             ?LOG_EMERGENCY(StringOrReport)).
  -define(emergency(StringOrReport, ArgsOrMeta), ?LOG_EMERGENCY(StringOrReport, ArgsOrMeta)).
  -define(emergency(FunOrFormat, Args, Meta),    ?LOG_EMERGENCY(FunOrFormat, Args, Meta)).

  -define(failed(Rsn, Extras),                   ?LOG_ERROR(fun s2_util:report_failed/1, [Rsn, Extras])).
  -define(failed(Rsn),                           ?failed(Rsn, #{})).

  -define(exception(C, R, S, Extras),            ?LOG_ERROR(fun s2_util:report_exception/1, [C, R, S, Extras])).
  -define(exception(Class, Reason, Stacktrace),  ?exception(Class, Reason, Stacktrace, #{})).

-else.

  -define(failed(Rsn, Extras), ?error( "Error: ~p"
                                     , [ {failed, Rsn
                                         , [ {function, ?FUNCTION_BIN}
                                           , {line,     ?LINE}
                                             | Extras
                                           ]}])).
  -define(failed(Rsn),         ?failed(Rsn, [])).

  -define(exception(Class, Reason, Stacktrace, Extras)
         , ?error( "Exception: ~p\n"
                   "Extras: ~p"
                 , [{{Class, Reason}, Stacktrace}, Extras])).

  -define(exception(Class, Reason, Stacktrace)
         , ?exception(Class, Reason, Stacktrace, [])).

  -ifdef(S2_USE_LAGER).

    -compile([{parse_transform, lager_transform}]).

    -define(debug(Format),           lager:debug(Format, [])).
    -define(debug(Format, Args),     lager:debug(Format, Args)).
    -define(info(Format),            lager:info(Format, [])).
    -define(info(Format, Args),      lager:info(Format, Args)).
    -define(notice(Format),          lager:notice(Format, [])).
    -define(notice(Format, Args),    lager:notice(Format, Args)).

    -ifdef(S2_USE_LAGER_BUT_NOT_FOR_ERRORS).

      -define(warning(Format),         error_logger:warning_msg(Format, [])).
      -define(warning(Format, Args),   error_logger:warning_msg(Format, Args)).
      -define(error(Format),           error_logger:error_msg(Format)).
      -define(error(Format, Args),     error_logger:error_msg(Format, Args)).
      -define(critical(Format),        error_logger:error_msg(Format)).
      -define(critical(Format, Args),  error_logger:error_msg(Format, Args)).
      -define(alert(Format),           error_logger:error_msg(Format)).
      -define(alert(Format, Args),     error_logger:error_msg(Format, Args)).
      -define(emergency(Format),       error_logger:error_msg(Format)).
      -define(emergency(Format, Args), error_logger:error_msg(Format, Args)).

    -else. %default

      -define(warning(Format),         lager:warning(Format, [])).
      -define(warning(Format, Args),   lager:warning(Format, Args)).
      -define(error(Format),           lager:error(Format, [])).
      -define(error(Format, Args),     lager:error(Format, Args)).
      -define(critical(Format),        lager:critical(Format, [])).
      -define(critical(Format, Args),  lager:critical(Format, Args)).
      -define(alert(Format),           lager:alert(Format, [])).
      -define(alert(Format, Args),     lager:alert(Format, Args)).
      -define(emergency(Format),       lager:emergency(Format, [])).
      -define(emergency(Format, Args), lager:emergency(Format, Args)).

    -endif. %S2_USE_LAGER_BUT_NOT_FOR_ERRORS

  -else.
    -ifdef(S2_DEBUG).
      -define(debug(Msg),            ?debug(Msg, [])).
      -define(debug(Fmt, As),        ?do_debug(unicode:characters_to_list(["~p:~s:~p: Debug: ", Fmt, "~n"]),
                                              [self(), ?FILE, ?LINE|As])).
    -else.
      -define(debug(Msg),            ok).
      -define(debug(Fmt, As),        ok).
    -endif. %S2_DEBUG

    -define(info(Msg),             ?info(Msg, [])).
    -define(info(Fmt, As),         ?do_info(unicode:characters_to_list(["~p:~s:~p: Info: ", Fmt, "~n"]),
                                            [self(), ?FILE, ?LINE|As])).
    -define(notice(Msg),           ?notice(Msg, [])).
    -define(notice(Fmt, As),       ?do_notice(unicode:characters_to_list(["~p:~s:~p: Notice: ", Fmt, "~n"]),
                                              [self(), ?FILE, ?LINE|As])).
    -define(warning(Msg),          ?warning(Msg, [])).
    -define(warning(Fmt, As),      ?do_warning(unicode:characters_to_list(["~p:~s:~p: Warning: ", Fmt, "~n"]),
                                              [self(), ?FILE, ?LINE|As])).
    -define(error(Msg),            ?error(Msg, [])).
    -define(error(Fmt, As),        ?do_error(unicode:characters_to_list(["~p:~s:~p: Error: ", Fmt, "~n"]),
                                            [self(), ?FILE, ?LINE|As])).
    -define(critical(Msg),         ?critical(Msg, [])).
    -define(critical(Fmt, As),     ?do_critical(unicode:characters_to_list(["~p:~s:~p: Critical: ", Fmt, "~n"]),
                                                [self(), ?FILE, ?LINE|As])).
    -define(alert(Msg),            ?alert(Msg, [])).
    -define(alert(Fmt, As),        ?do_alert(unicode:characters_to_list(["~p:~s:~p: Alert: ", Fmt, "~n"]),
                                            [self(), ?FILE, ?LINE|As])).
    -define(emergency(Msg),        ?emergency(Msg, [])).
    -define(emergency(Fmt, As),    ?do_emergency(unicode:characters_to_list(["~p:~s:~p: Emergency: ", Fmt, "~n"]),
                                                [self(), ?FILE, ?LINE|As])).

    -ifdef(S2_NOLOG).

      -define(do_debug(Fmt, As),     ok).
      -define(do_info(Fmt, As),      ok).
      -define(do_notice(Fmt, As),    ok).
      -define(do_warning(Fmt, As),   ok).
      -define(do_error(Fmt, As),     ok).
      -define(do_critical(Fmt, As),  ok).
      -define(do_alert(Fmt, As),     ok).
      -define(do_emergency(Fmt, As), ok).

    -else. %default

      -define(do_debug(Fmt, As),     error_logger:info_msg(Fmt, As)).
      -define(do_info(Fmt, As),      error_logger:info_msg(Fmt, As)).
      -define(do_notice(Fmt, As),    error_logger:info_msg(Fmt, As)).
      -define(do_warning(Fmt, As),   error_logger:warning_msg(Fmt, As)).
      -define(do_error(Fmt, As),     error_logger:warning_msg(Fmt, As)).
      -define(do_critical(Fmt, As),  error_logger:error_msg(Fmt, As)).
      -define(do_alert(Fmt, As),     error_logger:error_msg(Fmt, As)).
      -define(do_emergency(Fmt, As), error_logger:error_msg(Fmt, As)).

    -endif. %S2_NOLOG
  -endif. %S2_USE_LAGER
-endif. %S2_USE_OTP_LOGGER

%% Implementation from http://erlang.org/eeps/eep-0045.md
-define(FUNCTION_STRING,
        ?MODULE_STRING ++ ":" ++
          atom_to_list(?FUNCTION_NAME) ++ "/" ++
          integer_to_list(?FUNCTION_ARITY)).

-define(FUNCTION_BIN, iolist_to_binary(?FUNCTION_STRING)).

%% Structured warning
%%
%% * Good to have when sending warnings to Sentry
%% * In Sentry, logs will be grouped on Format and Args. Log data that differs
%%   but would still belong to the same Sentry issue should be in Extras.
%% * The other part of this hack is in raven_error_logger.erl
-define( warn(Format, Args, Extras)
       , ?warning( "Warning: ~p~n" ++ Format
                 , [ {extras, [ {function, ?FUNCTION_BIN}
                              , {line,     ?LINE}
                              | Extras
                              ]}
                   | Args]
                 )
       ).
-define(warn(Format, Args), ?warn(Format, Args, [])).
-define(warn(Format), ?warn(Format, [], [])).

%%%_* Metrics ==========================================================
%% Luke Gorrie's favourite profiling macro.
-define(TIME(Tag, Expr),
        (fun() ->
           %% NOTE: timer:tc/4 does an annoying 'catch' so we
           %% need to wrap the result in 'ok' to be able to
           %% detect an unhandled exception.
           {__TIME, __RESULT} =
             timer:tc(erlang, apply, [fun() -> {ok, Expr} end, []]),
             ?debug( "time(~s): ~wms ~999p~n"
                   , [?MODULE, __TIME/1000, Tag] ),
           case __RESULT of
             {ok, _}         -> element(2, __RESULT);
             {'EXIT', Error} -> exit(Error)
           end
         end)()).

-ifdef(S2_USE_KIVRA_METRICS).

-define(do_increment(__Name),
        (kivra_metrics:increase_counter([?APP | __Name]))).
-define(do_increment(__Fun, __Ret),
        ?do_increment([?MODULE, __Fun, __Ret])).
-define(do_time(__Name, __Expr),
        (kivra_metrics:time_mfa([?APP | __Name], ?thunk(__Expr)))).
-define(do_time_diff(__Name, __Time),
        (kivra_metrics:time_diff([?APP | __Name], __Time))).
-define(do_histogram(__Name, __Value),
        (kivra_metrics:histogram([?APP | __Name], __Value))).

-else.

-ifdef(S2_USE_FOLSOM).

-define(name(Xs), (s2_atoms:catenate(s2_lists:intersperse('_', Xs)))).

-define(do_increment(Name),
        (catch folsom_metrics:notify({?name(Name), 1}))).
-define(do_increment(Fun, Ret),
        ?do_increment([?APP, ?MODULE, Fun, Ret])).
-define(do_time(Name, Expr),
        (try case is_list(Name) of
           true ->
             folsom_metrics:histogram_timed_update(?name(Name),
                                                   ?thunk(Expr));
           false ->
             folsom_metrics:histogram_timed_update(?name([ ?MODULE
                                                         , ?APP
                                                         , Name]),
                                                   ?thunk(Expr))
         end catch _:_ -> Expr
         end)).
-define(do_time_diff(Name, Time),
        (catch case is_list(Name) of
           true ->
             folsom_metrics:histogram_timed_notify({?name(Name),
                                                   Time});
           false ->
             folsom_metrics:histogram_timed_notify({ ?name([ ?MODULE
                                                           , ?APP
                                                           , Name])
                                                   , Time})
         end)).

-else.

-ifdef(S2_USE_ESTATSD).

-define(name(Xs), (s2_atoms:catenate(s2_lists:intersperse('.', Xs)))).

-define(do_increment(__Name),
        (catch estatsd:increment(?name(__Name)))).
-define(do_increment(__Fun, __Ret),
        ?do_increment([?APP, ?MODULE, __Fun, __Ret])).
-define(do_time(__Name, Expr),
        (begin
           {__T, __Val} = timer:tc(?thunk(Expr)),
           case is_list(__Name) of
             true  -> estatsd:timing(?name(__Name), __T/1000);
             false -> estatsd:timing(?name([?MODULE, ?APP, __Name]), __T/1000)
           end,
           __Val
         end)).
-define(do_time_diff(__Name, __Time),
        (catch case is_list(__Name) of
           true ->
             estatsd:timing(?name(__Name), __Time);
           false ->
             estatsd:timing(?name([?MODULE, ?APP, __Name]), __Time)
         end)).

-else.

-ifdef(S2_RIEMANN).

-ifdef(S2_RIEMANN_USE_POOL).
-define(katja, katja_pool).
-else.
-define(katja, katja).
-endif.

-define(name(Xs), (?a2l(s2_atoms:catenate(s2_lists:intersperse('/', Xs))))).

-define(do_increment(__Name),
        (ok)).
-define(do_increment(__Fun, __Ret),
        ?do_increment([?APP, ?MODULE, __Fun, __Ret])).
-define(do_time(__Name, Expr),
        (begin
           {__T, __Val} = timer:tc(?thunk(Expr)),
           case is_list(__Name) of
             true  ->
               ?katja:send_event_async([ {service, ?name(__Name)}
                                       , {time,    s2_time:unix_epoch()}
                                       , {metric,  round(__T/1000)}]);
             false ->
               ?katja:send_event_async([ {service, ?name([?APP, ?MODULE])}
                                       , {time,    s2_time:unix_epoch()}
                                       , {metric,  round(__T/1000)}])
           end,
           __Val
         end)).
-define(do_time_diff(__Name, __Time),
        (catch case is_list(__Name) of
           true ->
               ?katja:send_event_async(
                  [ {service, ?name(__Name)}
                  , {time,    s2_time:unix_epoch()}
                  , {metric,  timer:now_diff(os:timestamp(), __Time)/1000}] );
           false ->
               ?katja:send_event_async(
                  [ {service, ?name([?APP, ?MODULE])}
                  , {time,    s2_time:unix_epoch()}
                  , {metric,  timer:now_diff(os:timestamp(), __Time)/1000}] )
           end)).

-else. %default

-define(name(Xs), (s2_atoms:catenate(s2_lists:intersperse('.', Xs)))).

-define(do_increment(Name),     ok).
-define(do_increment(Fun, Ret), Fun, ok).
-define(do_time(__Name, __Expr),
        (begin
           {__T, __Val} = timer:tc(?thunk(__Expr)),
           case is_list(__Name) of
             true  ->
               ?debug("time(~s): ~wms ~p~n", [?MODULE,__T/1000,?name(__Name)]);
             false ->
               ?debug( "time(~s): ~wms ~p~n", [?MODULE, __T/1000, __Name])
           end,
           __Val
         end)).
-define(do_time_diff(__Name, __Time),
       (catch case is_list(__Name) of
           true ->
             ?debug( "time(~s): ~wms ~p~n"
                   , [ ?MODULE
                     , timer:now_diff(os:timestamp(), __Time)/1000
                     , ?name(__Name) ] );
           false ->
             ?debug( "time(~s): ~wms ~p~n"
                   , [ ?MODULE
                     , timer:now_diff(os:timestamp(), __Time)/1000
                     , __Name ] )
           end)).

-endif. %S2_USE_ESTATSD

-endif. %S2_RIEMANN

-endif.

-endif.

-define(increment(Name),       ?do_increment(Name)).
-define(increment(Fun, Ret),   ?do_increment(Fun, Ret)).
-define(time(Name, Expr),      ?do_time(Name, Expr)).
-define(time_diff(Name, Time), ?do_time_diff(Name, Time)).
-define(histogram(Name, Value), ?do_histogram(Name, Value)).

%%%_* Misc =============================================================
-define(FUNCTION,
        (element(2, element(2, process_info(self(), current_function))))).

-define(UUID(), (s2_rand:int())).

%%%_* Types ============================================================
-type alist(A, B) :: [{A, B}].
-type fd()        :: file:io_device().
-type file()      :: string().

%%%_* Footer ===========================================================
-endif. %include guard

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
