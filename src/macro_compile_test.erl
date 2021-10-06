-module(macro_compile_test).

-export([all_macros/0]).

-define(S2_USE_OTP_LOGGER, true).

-include("prelude.hrl").

-dialyzer({nowarn_function, all_macros/0}).

all_macros() ->
  ?debug("foo"),
  ?debug("foo", []),
  ?debug("foo", [], #{}),
  ?info("foo"),
  ?info("foo", []),
  ?info("foo", [], #{}),
  ?warning(""),
  ?warning("foo", []),
  ?warning("foo", [], #{}),
  ?error(""),
  ?error("foo", []),
  ?error("foo", [], #{}),
  ?critical(""),
  ?critical("foo", []),
  ?critical("foo", [], #{}),
  ?alert(""),
  ?alert("foo", []),
  ?alert("foo", [], #{}),
  ?emergency(""),
  ?emergency("foo", []),
  ?emergency("foo", [], #{}),
  ?audit(""),
  ?audit("foo", []),
  ?audit("foo", [], #{}),
  ?failed("foo"),
  ?failed("foo", #{}),
  ?exception(throw, "foo", []),
  ?exception(throw, "foo", [], []).
