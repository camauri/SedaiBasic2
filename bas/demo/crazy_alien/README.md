# Crazy Alien — procedural death effects (prototype)

An exploratory prototype for a wave shooter with a comic tone: the enemies will be drawn sprites,
but their **deaths are entirely computed** — no hand-drawn animation. This program answers one
question only: do the effects hold up visually, at any speed, and inside the frame budget?

There is no game here: no movement, no weapon, no score. One test sprite in the middle of the
screen, and six ways for it to die.

## Running it

    sb --window bas/demo/crazy_alien/deaths.bas        # SedaiBasic (needs a build with --window)
    fbc bas/demo/crazy_alien/deaths.bas && ./deaths     # FreeBASIC, unchanged source

| key | |
|---|---|
| `1` … `6` | disintegration · tornado · freeze and melt · char · black hole · ghost |
| `+` / `-` | death duration, 0.05 s to 2 s (shown on screen) |
| `S` | twenty deaths at once — the worst case |
| `Q` | quit |

The screen shows the frame time (median, p99, worst of the last 120 frames) and the live fragment count.

Headless runs, for measuring without a window:

    sb deaths.bas run=bench effect=3 many=1        # 240 frames at 60 Hz, twenty deaths; stats to bench.txt
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

## What the prototype measured (7 September 2026, 640×480, 16×16 sprite, 60 Hz step)

Frame time in ms, one death / twenty simultaneous deaths:

| effect | fbc median | fbc p99 | sb median | sb p99 |
|---|---:|---:|---:|---:|
| 1 disintegration | 0.07 / 0.19 | 0.09 / 0.37 | 0.90 / 3.05 | 1.22 / 3.51 |
| 2 tornado | 0.07 / 0.22 | 0.09 / 0.31 | 0.90 / 3.69 | 1.09 / 4.21 |
| 3 freeze and melt | 0.06 / 0.25 | 0.09 / 0.39 | 0.84 / 3.28 | 1.12 / 5.42 |
| 4 char | 0.06 / 0.28 | 0.13 / 1.40 | 0.88 / 3.35 | 1.00 / 5.58 |
| 5 black hole | 0.07 / 0.26 | 0.10 / 0.47 | 0.93 / **5.76** | 1.15 / **6.52** |
| 6 ghost | 0.07 / 0.25 | 0.13 / 1.29 | 0.87 / 3.81 | 1.09 / 5.09 |

Twenty deaths cost under a third of a 16.7 ms frame on the interpreter and a fiftieth of it compiled.
The black hole is the most expensive fragment effect (a square root, a division and a `Line` per
fragment); the ghost is the most expensive per sprite (five `PSet` per pixel plus three filled
ellipses) but has no fragment list to grow.

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
