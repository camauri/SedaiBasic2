# Crazy Alien — procedural death effects (prototype)

An exploratory prototype for a wave shooter with a comic tone: the enemies will be drawn sprites,
but their **deaths are entirely computed** — no hand-drawn animation. This program answers one
question only: do the effects hold up visually, at any speed, and inside the frame budget?

There is no game here: no movement, no weapon, no score. One alien in the middle of the screen —
five robotic sea creatures take turns: cuttlefish, octopus, crab, hermit crab, jellyfish, 24×24 —
and six ways for it to die.

## Running it

    sb --window bas/demo/crazy_alien/deaths.bas        # SedaiBasic (needs a build with --window)
    fbc bas/demo/crazy_alien/deaths.bas && ./deaths     # FreeBASIC, unchanged source

| key | |
|---|---|
| `1` … `6` | disintegration · tornado · freeze and melt · char · black hole · ghost |
| `N` | next species |
| `S` | twenty at once — every species, a random effect each |
| `F` | fullscreen on/off |
| `+` / `-` | death duration, 0.05 s to 2 s (shown on screen) |
| `Q` | quit |

Everything is drawn in a logical 320×240 and shown at `scale=2` (each logical pixel a 2×2 block);
`scale=1` gives the native size, `fullscreen=1` starts on the whole monitor.

The screen shows the frame time (median, p99, worst of the last 120 frames) and the live fragment count.

Headless runs, for measuring without a window:

    sb deaths.bas run=bench effect=3 many=1        # 240 frames at 60 Hz, twenty deaths of ONE effect; stats to bench.txt
    sb deaths.bas run=capture effect=5 dur=0.15    # stills at 20/45/70/95 % of one death, as PPM

## How it works

Five of the six effects share one road: the sprite's pixels become independent **fragments** —
position, velocity, colour — and each effect is a different force field over that list. The sprite
is data (16×16 here, its size is read from the data, so any sprite works). The **ghost** is the
exception: it keeps the sprite intact and transforms colour and opacity, with wings and a halo drawn
from the sprite's bounding box.

Every effect takes a **duration** and is written in normalised time (0 at the hit, 1 at the end), so
the same trajectory plays at any speed: in the game the duration will follow the wave's speed. The
source explains, at each force field, what it produces and what moving the key constants would do.

## What the prototype measured (7 September 2026, logical 320×240 at scale 2, 24×24 sprites, 60 Hz step)

Frame time in ms, one death / twenty simultaneous deaths of the same effect (11 520 fragments):

| effect | fbc median | fbc p99 | sb median | sb p99 |
|---|---:|---:|---:|---:|
| 1 disintegration | 0.12 / 1.17 | 0.25 / 1.79 | 0.95 / 6.40 | 1.15 / 7.87 |
| 2 tornado | 0.10 / 0.92 | 0.13 / 1.79 | 0.96 / 7.55 | 1.14 / 8.04 |
| 3 freeze and melt | 0.11 / 1.28 | 0.25 / 1.79 | 0.94 / 6.46 | 1.39 / 10.64 |
| 4 char | 0.13 / 1.36 | 0.17 / 1.73 | 1.00 / 7.18 | 1.43 / 9.05 |
| 5 black hole | 0.07 / 0.60 | 0.13 / 1.24 | 1.24 / **12.83** | 1.58 / **14.71** |
| 6 ghost | 0.12 / 1.54 | 0.53 / 6.15 | 1.02 / 6.31 | 1.28 / 9.26 |

`sb --aot` brings the black hole's twenty to 3.3 ms median; `--jit` does not help here (its loops
contain graphics opcodes it does not compile). Twenty deaths stay inside a 16.7 ms frame on every
engine; the black hole is the most expensive fragment effect (a square root, a division and a `Line`
per fragment — the same 12.5 ms at scale 1, so it is the arithmetic, not the pixels), the ghost the
most expensive per sprite (five plots per pixel plus three filled ellipses).

Shortest duration at which each effect still reads (from stills at 60 Hz):

| effect | floor | why |
|---|---:|---|
| disintegration | 0.05 s | a burst reads in two frames |
| ghost | 0.08 s | the rise needs four frames to read as floating |
| char | 0.10 s | the front must sweep for two frames before the crumble |
| freeze and melt | 0.10 s | two phases, each needs two frames |
| tornado | 0.12 s | half a turn has to be visible |
| black hole | 0.15 s | the pull ramps with p² and the collapse is the last 15 % |

Below those the effect degenerates into the disintegration, which is what the fastest waves use anyway.
