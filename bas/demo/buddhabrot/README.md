# Buddhabrot

<p align="center">
  <img src="buddhabrot.png" alt="Buddhabrot rendered by SedaiBasic: a seated figure in red and yellow on black" width="600">
</p>

<p align="center"><em>60 million orbits, 800×800, rendered by <code>buddhabrot.bas</code> in about 19 seconds.</em></p>

The usual Mandelbrot picture colours each point by how long it takes to escape. This one asks a
different question — **where do the escaping orbits go on their way out?** — and answers it by
throwing millions of random points at the plane, following the ones that escape, and counting how
often each pixel is passed through. The result is a density map. Turned a quarter turn, it looks like
a seated figure, which is where the name comes from.

The demo exists to make one thing visible: **how much faster SedaiBasic's compiled engines are than
its bytecode interpreter.** Every frame gets the same slice of wall-clock time whichever engine is
underneath, so the frame rate never changes — what changes is how much of the picture has appeared.

---

## Running it

```bash
./build.sh sb --window            # once: sb with the SDL2 window presenter
```

Then, from the repository root:

```bash
bin/x86_64-linux/sb --window bas/demo/buddhabrot/buddhabrot.bas                    # interpreter
bin/x86_64-linux/sb --window bas/demo/buddhabrot/buddhabrot.bas --jit label=JIT    # JIT
bin/x86_64-linux/sb --window bas/demo/buddhabrot/buddhabrot.bas --aot label=AOT    # AOT
```

Or all three at once, on the same seed, to watch them diverge:

```bash
bash bas/demo/buddhabrot/compare_engines.sh
```

Keys while it runs: **SPACE** pause · **R** restart · **S** save a still · **+ / −** halve or double
the iteration ceiling · **Q** quit.

### Without a window

```bash
bin/x86_64-linux/sb bas/demo/buddhabrot/buddhabrot.bas --aot still=20000000 out=/tmp/b.ppm
```

`still=` traces a fixed number of orbits, writes a binary PPM and exits. Every image on this page was
made that way. Run `... buddhabrot.bas help=1` for the full argument list.

## The three engines are one binary

There are no three executables to build. SedaiBasic ships a single `sb`, and `--jit` and `--aot`
select which engine runs the loaded program:

| build | size |
|---|---|
| `./build.sh sb` (headless, the regression target) | 4 134 024 bytes |
| `./build.sh sb --window` (adds the SDL2 presenter) | 4 143 016 bytes |

The engine is bound when the program is **loaded** — the JIT builds its native loops and the AOT
compiler compiles its functions before the first instruction runs — so it cannot be changed by a
keystroke half-way through a run. That is why the comparison is a script that launches three
processes rather than a key in the demo. `compare_engines.sh` gives all three the same seed, so the
three windows are computing an identical image and the only difference is how fast it arrives.

## What it actually does

Measured on this machine (AMD, Linux, single thread), tracing two million orbits with `still=`:

| engine | seconds | orbits per second | against the interpreter |
|---|---:|---:|---:|
| bytecode interpreter | 2.51 | 798 000 | — |
| JIT | 0.92 | 2 180 000 | 2.7× |
| AOT | 0.61 | 3 260 000 | **4.1×** |

Live, in a six-second run at the default 30 frames a second. The left-hand column is the point: it
does **not** move.

| engine | frames per second | orbits traced in 6 s |
|---|---:|---:|
| bytecode interpreter | 29 | ~4.3 million |
| JIT | 29 | ~11.7 million |
| AOT | 29 | ~12.9 million |

The demo prints both numbers when it exits, so this table can be reproduced rather than believed.
The orbit counts move by a percent or two between runs; the frame rate does not move at all, which
is the whole claim.

**The frame rate is a budget, not a limit.** With no sampling at all the same loop reaches 230 frames
a second interpreted, 269 under the JIT and 81 under AOT. `fps=` moves it, and moving it shows you
something the demo would rather not have to admit:

| `fps=` | interpreter orbits in 6 s | AOT orbits in 6 s | ratio |
|---:|---:|---:|---:|
| 20 | ~4.5 M | ~14.8 M | **3.31×** |
| 30 (default) | ~4.3 M | ~12.9 M | **2.97×** |
| 60 | ~4.0 M | ~7.3 M | **1.83×** |

The faster the frame rate, the worse the AOT engine looks — which is the opposite of what a demo
about compiled code should show. The next section says why.

The live ratio is smaller than 4.1×, and the reason is worth stating plainly because it works
*against* the demo's own headline. Painting the window is one `PSet` per pixel, and `PSet` costs
about **4 ns per call under the interpreter, about the same under the JIT, and about 60 ns under
AOT** — measured flat at 100×100, 200×200 and 400×400, so it is a per-call cost rather than a fixed
overhead per frame. A full 400×400 repaint is about 0.75 ms, 0.75 ms and 9.5 ms respectively.

The interpreter is quick here because `PSet` has an arm in the C hot dispatch loop. The AOT has **no
native lowering for it**, so every pixel becomes a runtime-helper call, and a helper call in this
engine flushes every allocated register to memory and reloads them all afterwards. Switch the C loop
off (`HOTC_OFF=1`) and the interpreter costs 32 ns per pixel — that is the same Pascal arm the AOT is
paying for, without the call around it.

So the painting is a tax on the sampling budget and it falls hardest on the fastest engine: this demo
**understates** how much quicker AOT is, and the faster you ask it to run, the more it understates.
It is written here rather than left out because a demo that quietly picked the flattering number
would not be worth showing.

This is a defect in the engine, not in the demo, and it is the kind this project has fixed three
times before — strings, records and `PRINT` each began as helper calls in a hot loop and each got a
native lowering once measured. `PSet` has not had that done to it yet.

## The same image, four ways of computing it

```bash
bash bas/demo/buddhabrot/verify_determinism.sh
```

Traces the same orbits under the interpreter, the JIT, the AOT compiler and the interpreter with the
optimiser switched off, then compares SHA-256 hashes of the four output files. They must be
identical, and they are.

This matters more than it sounds. The image depends on a floating-point comparison — *has this orbit
passed radius 2 yet?* — made tens of millions of times. If any engine rounded differently anywhere,
one orbit would cross a step early and the pictures would part company. The check is also why the
random numbers come from five lines of xorshift written out in the source rather than from the
runtime's `RND`.

## Reading the source

`buddhabrot.bas` is meant to be read, and it is the point of the demo as much as the picture is. The
whole algorithm is one subroutine, `TraceOneOrbit`, and it fits on a screen. Everything above it is
preparation, everything below it is presentation.

The header lists the five things that are easy to get wrong — the orbit that must be buffered before
it is accumulated, the two regions that never escape, the mirror symmetry, the iteration floor, and
the tone curve — and each one is called out again at the line where it matters. The optimisations
that were deliberately *not* made are listed at the foot of the file, with the reason each was
rejected.

## Credit

The technique is Melinda Green's, first described in 1993; the name is Lori Gardi's. This
implementation is independent — written from the mathematical description of the algorithm, with no
existing Buddhabrot source consulted. See [IMPLEMENTATIONS.md](IMPLEMENTATIONS.md) for the genealogy
of the idea and for how other languages express the same algorithm.

## Licence

GPL-3.0-or-later. See [LICENSE](LICENSE) in this directory.
