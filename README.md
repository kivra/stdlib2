                          _      _ _ _ _    ____
                      ___| |_ __| | (_) |__|___ \
                     / __| __/ _` | | | '_ \ __) |
                     \__ \ || (_| | | | |_) / __/
                     |___/\__\__,_|_|_|_.__/_____|
                       Erlang stdlib extensions

Overview
========
stdlib2 is a collection of useful functions, data structures, and
behaviours.

Installation
============
jakob@sleepy.primat.es:~/git/stdlib2$ gmake

jakob@sleepy.primat.es:~/git/stdlib2$ gmake test

Manifest
========
* include/:
    * prelude.hrl        -- Complete collection of macros and type aliases.
    * byte.hrl           -- Macros and type aliases for byte unit conversion.
    * maybe.hrl          -- Macros and type aliases for maybe monads.
    * time.hrl           -- Macros and type aliases related to time and date.
* src/:
    * s2_atoms.erl       -- Atom-related utilities.
    * s2_env.erl         -- Environment access and setup.
    * s2_functors.erl    -- The functor typeclass.
    * s2_funs.erl        -- Combinators.
    * s2_lists.erl       -- `lists' extensions.
    * s2_loop.erl        -- Higher-order functions for writing loops.
    * s2_maps.erl        -- Nested dictionaries.
    * s2_maybe.erl       -- The Maybe Monad.
    * s2_par.erl         -- Better pmap.
    * s2_procs.erl       -- `erlang' extensions.
    * s2_sh.erl          -- Unix commands.
    * s2_strats.erl      -- Sane supervision defaults.
    * s2_time.erl        -- Timestamps.

// eof
