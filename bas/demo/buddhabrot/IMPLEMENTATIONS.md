# The Buddhabrot, expressed in different languages

This page is **not** a benchmark. There are no timings here, no rankings and no verdicts about which
language or which implementation is better — only the ways different languages say the same thing.
Anyone who wants to judge can open the sources; they are all linked.

---

## 1. Where the idea comes from

The technique is not ours and neither is the name.

* **Melinda Green** discovered the rendering method and described it in a 1993 post to the Usenet
  group `sci.fractals`. Her own account of it is at
  [superliminal.com/fractals/bbrot](https://superliminal.com/fractals/bbrot/bbrot.htm).
* **Lori Gardi** proposed the name *Buddhabrot* some years later, on seeing that the figure — turned
  a quarter turn — resembles a seated Buddha. Green adopted it and it stuck.
* **Paul Bourke** published the C reference implementation that most later versions start from:
  [paulbourke.net/fractals/buddhabrot](https://paulbourke.net/fractals/buddhabrot/).
* **Alexander Boswell** introduced Metropolis-Hastings sampling, which is what makes deep zooms
  practical: instead of drawing points uniformly, it mutates points already known to produce long
  orbits.

Background and the naming story: [Buddhabrot on Wikipedia](https://en.wikipedia.org/wiki/Buddhabrot),
and [Softology's write-up](https://softologyblog.wordpress.com/2011/06/26/buddhabrot-fractals/).

## 2. What ours is, and what it is not

**`buddhabrot.bas` is an independent implementation.** It was written from the mathematical
description of the algorithm — sample `c`, iterate `z(n+1) = z(n)² + c`, discard the orbits that stay
bounded, and count the pixels the escaping ones pass through — and from nothing else. No existing
Buddhabrot source was opened while writing it.

It is **not a port**. That word is used below only for implementations whose own authors describe
them that way.

> **Why there are no pinned line-range links to other people's code below.**
> Linking `#L42-L67` at a commit SHA is the right way to point at a specific passage of someone
> else's source — but choosing which lines to point at means reading them, and the whole basis of the
> claim above is that we did not. So the table links to repositories, not to passages, and the axes
> in section 4 describe **what each language offers or forces**, which is a fact about the language,
> rather than what any particular author chose, which would be a fact about their code.
> Our own source, on the other hand, is in this directory in full and in the clear.

## 3. The implementations

| Language | Author | Licence (checked 1 Sep 2026) | Sampling | Runs on | Link |
|---|---|---|---|---|---|
| SedaiBasic | Maurizio Cammalleri | GPL-3.0-or-later | uniform | CPU, single thread | [`buddhabrot.bas`](buddhabrot.bas) |
| C | Paul Bourke | not stated on the page | uniform | CPU | [paulbourke.net](https://paulbourke.net/fractals/buddhabrot/) |
| Go | karlek | Unlicense (public domain) | uniform | CPU, multi-threaded | [karlek/wasabi](https://github.com/karlek/wasabi) |
| JavaScript | Albert Lobo (llop) | MIT | uniform | browser | [llop/buddhabrot-4d-viewer-js](https://github.com/llop/buddhabrot-4d-viewer-js) |
| JavaScript | Frank Force (KilledByAPixel) | GPL-3.0 | uniform | browser | [KilledByAPixel/Buddhabrot](https://github.com/KilledByAPixel/Buddhabrot) |
| C# | nikvoronin | MIT | Metropolis-Hastings | GPU via OpenCL | [nikvoronin/BuddhabrotCL](https://github.com/nikvoronin/BuddhabrotCL) |
| Rust | Paul Grandperrin | MIT | uniform | browser via WebAssembly | [PaulGrandperrin/rustybrot-web](https://github.com/PaulGrandperrin/rustybrot-web) |
| CUDA | yalue | **none stated** | uniform | GPU | [yalue/cudabrot](https://github.com/yalue/cudabrot) |
| Odin | João Carvalho | **none stated** | uniform | CPU | [joaocarvalhoopen/Fractal-Buddhabrot-in-Odin](https://github.com/joaocarvalhoopen/Fractal-Buddhabrot-in-Odin) |

⚠️ **Two of these state no licence at all**, which under the Berne Convention means all rights
reserved, not public domain. They are listed because they exist and are interesting, and for no other
reason: nothing from them is reproduced here, and nothing should be reproduced from them without
asking their authors. The Odin one describes itself as the end of a chain of ports, C to Python to Go
to Odin, and credits that chain.

## 4. Four places where languages diverge

### Complex numbers: a type, or two variables?

`z² + c` is one line if the language has complex arithmetic and four if it does not.

* **Go** has `complex128` in the language itself, with `*` and `+` defined on it.
* **C#** has `System.Numerics.Complex` in the standard library.
* **C**, since C99, has `double _Complex`; CUDA supplies `cuDoubleComplex` with helper functions.
* **Rust**, **JavaScript** and **Odin** have no complex type at hand — a struct or a crate, or two
  separate `f64`s.

**Ours carries the real and imaginary parts as separate `Double` variables** and writes the
multiplication out. That is not reluctance: the squares `zRe²` and `zIm²` are each needed twice per
iteration, once to advance the orbit and once to test whether it has escaped, so keeping them in
named variables removes half the multiplications from the hottest loop in the program. A complex type
would hide exactly the sub-expression worth sharing.

### The accumulation buffer

Every version needs one counter per pixel, and the differences are about how the language lets you
say "a flat block of integers, indexed by row times width plus column".

* Languages with real two-dimensional arrays can index `[row][column]` directly.
* GPU versions must lay it flat regardless, because that is what device memory is, and must make the
  increment atomic — thousands of threads write to the same counters.

**Ours is a one-dimensional `LongInt` array indexed `row * imageSize + column`**, resized once at
startup with `ReDim` because the image size is an argument. Flat because the tone mapping and the
file writer both walk it in order, and one index is easier to follow than two.

### Where the randomness comes from

This is the axis where implementations differ most, and it decides whether the picture is
reproducible.

* Using the language runtime's generator — `math/rand`, `Math.random()`, `rand()` — is the shortest
  route and gives up reproducibility across languages, versions and platforms.
* GPU versions cannot use a host generator at all: each thread needs its own stream, so a small
  counter-based generator is seeded per thread.

**Ours has a five-line xorshift32 written out in the source.** The reason is specific to what this
demo is for: the same seed has to produce the same image under the interpreter, the JIT and the AOT
compiler, or the three-way comparison would be comparing three different pictures rather than three
speeds. `verify_determinism.sh` checks that with hashes.

### Parallelism

* **Go** has goroutines and channels in the language.
* **Rust** in the browser is limited to what WebAssembly and web workers allow.
* **JavaScript** in a page has one thread unless workers are set up explicitly.
* **CUDA** and **OpenCL** are the extreme: the algorithm is embarrassingly parallel — every orbit is
  independent of every other — so a GPU version launches one thread per sample and the only
  coordination left is the atomic increment on the histogram.

**Ours is deliberately single-threaded.** SedaiBasic has threads, and this algorithm would take them
gladly. It does not use them because the demo exists to compare three execution engines, and three
engines compared through a thread pool measure the thread pool. It is noted as a possible extension
at the foot of the source, where it belongs.

## 5. If you want to add a language

The comparison stays readable only while everyone is answering the same question. An implementation
belongs in the table above if it uses **uniform sampling**, accumulates **every escaping orbit** it
finds, and is **honest about its licence**. Metropolis-Hastings versions are welcome but should be
marked as such in the sampling column, as `BuddhabrotCL` is — the two converge differently, and
comparing them without saying so would mislead.
