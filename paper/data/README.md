data
====

Experimental results.
Data for each project is stored in a `.rktd` file.


Data Format
-----------

These `.rktd` files each contain a single 2-dimensional vector.
The outer vector has `2**N` entries, one for each typed/untyped variation in the project's lattice.
The inner vector at position `i` is the absolute results of running the `i`-th variation.

We order variations by:
- Ordering the modules in the project alphabetically
  (Just the modules that can be typed or untyped, not the base files common to all variations.)
- Generating a bitstring for each variation, where the `k`-th bit is high iff the `k`-th module
  in the alphabetical ordering is typed. (Zero is the left end of the bitstring.)
- Mapping each bitstring to the natural number it represents in binary.
So the fourth entry in the `.rktd` data representing a project with four modules
would correspond to the variation `0100`.

There is no metadata stored in these data files.
We track that here.


Machines
--------
- Galicia (4 phys. cores, Core i7-3770K 3.50GHz)
- "The cluster" (40 phys. cores)
- Desktop (4 phys. cores, Core i7-4790 3.60GHz)
- Lambda (12 phys. cores, Xeon E5-2630 2.30GHz)
- Sapporo (2 phys. cores, PPC970MP)


Files
-----

Run on Racket 6.2.

- `gregor-2015-06-30.rktd` Run on cluster, `-j 39`
- `kcfa-2015-06-25T13:48:52.rktd` Run on Lambda with `-j 11`
- `lnm-large-06-28.rktd` Run on Desktop, single core. Benchmarked on data for gregor.
- `morsecode-06-27.rktd` Run on Desktop, single core. Benchmarked on the entire list of frequently used words.
- `sieve-2015-06-28T14:08:55.rktd` Run on Lambda, single core

Benchmarks run on different-sized inputs
- `funkytown-mid-06-26.rktd` Run on Galicia, single core. Benchmarked on medium-size synth test (smoke on the water).
- `kcfa-small-06-27.rktd` Run on Desktop, single core. Benchmarked on a very small "standard example" test.
- `lnm-mid-06-22.rktd` Run on Desktop, single core. Benchmarked on data for suffixtree.
- `moresecode-small-06-27.rktd` Run on Desktop, single core. Benchmarked on a subset of the "frequently-used-words" file.

__NOTE__ all these files are _unofficial_, they were not run on Racket 6.2.
We need to update them all.

- `echo.rktd` Unknown origin.
- `funkytown.rktd`
- `gregor-05-11.rktd` Run on Galicia
- `gregor-05-24.rktd` Run on the cluster.
- `kcfa-06-01.rktd` Run on Galicia.
- `mbta-04-20.rktd` Original MBTA, run on Galicia.
- `mbta-04-25.rktd` Fixed MBTA, run on Galicia.
- `sieve-04-06.rktd` Run on Galicia
- `snake-04-10.rktd` Run on Galicia
- `suffixtree-06-10.rktd` Run on Galicia
- `tetris.rktd` Run on Galicia, date unknown
- `zordoz-04-09.rktd` Run on Galicia
