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
    * prelude.hrl        -- Useful macros and type aliases.
* src/:
    * s2_atoms.erl       -- Atom-related utilities.
    * s2_csets.erl       -- Counting sets.
    * s2_env.erl         -- Environment access and setup.
    * s2_export.erl      -- Export unexported functions from the REPL.
    * s2_fs.erl          -- Filesystem-related utilities.
    * s2_funs.erl        -- Combinators.
    * s2_gen_db.erl      -- Behaviour for persistent state.
    * s2_gen_lattice.erl -- Behaviour for lattices.
    * s2_hex.erl         -- ASCII armor.
    * s2_lists.erl       -- `lists' extensions.
    * s2_loop.erl        -- Higher-order functions for writing loops.
    * s2_maps.erl        -- Nested dictionaries.
    * s2_maybe.erl       -- The Maybe Monad.
    * s2_par.erl         -- Better pmap.
    * s2_pn_counters.erl -- PN counters.
    * s2_procs.erl       -- `erlang' extensions.
    * s2_rand.erl        -- Randomness.
    * s2_sh.erl          -- Unix commands.
    * s2_strats.erl      -- Sane supervision defaults.
    * s2_time.erl        -- Timestamps.
    * s2_vclocks.erl     -- Vector clocks.

// eof
