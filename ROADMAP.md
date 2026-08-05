# SedaiBasic Roadmap

Future directions and project goals for SedaiBasic.

> **In flight (6 Aug 2026): voxel-landscape demo, phase 1.** Public material. The plan, and the
> measured budget it rests on, are in `job/docs/PIANO_DEMO_VOXEL.md`. The English in-source comments
> are a primary deliverable, not documentation added at the end — that is what readers of the previous
> demos singled out. Implemented from first principles: no existing voxel source is ported or adapted,
> and every constant is chosen experimentally here.

---

## Tool Suite

The SedaiBasic suite consists of specialized tools with consistent `sb*` naming:

| Tool | Executable | Source | Status | Description |
|------|------------|--------|--------|-------------|
| **Interpreter** | `sb` | `SedaiBasicVM.lpr` | ✅ Working | Runs `.bas` and `.basc` from terminal |
| **Compiler** | `sbc` | `SedaiBasicCompiler.lpr` | ✅ Working | Compiles `.bas` → `.basc` (bytecode) |
| **Vision** | `sbv` | `SedaiVision.lpr` | ✅ Working | SDL2 graphical console (retro/modern) |
| **Disassembler** | `sbd` | `SedaiBasicDisassembler.lpr` | ✅ Working | Disassembles `.basc` to readable format |
| **Synth** | `sbs` | `SedaiSynth.lpr` | 📋 Planned | Audio synthesis and sound engine |
| **Profiler** | `sbp` | `SedaiProfiler.lpr` | 📋 Planned | Standalone profiler for performance analysis |

### Typical Workflows

```bash
# Rapid development (interpret directly)
sb program.bas

# Separate compilation
sbc program.bas -o program.basc
sb program.basc

# Debug and analysis
sbd program.basc              # Disassemble bytecode
sbp program.bas --flamegraph  # Profiling

# Retro graphical experience
sbv program.bas               # SDL2 console
sbv --mode=c64 program.bas    # C64 style emulation
```

---

## I/O System

### Current Architecture

```
┌─────────────┐
│     VM      │ uses IOutputDevice / IInputDevice
└──────┬──────┘
       │
┌──────┴──────┐
│  TIOManager │ Central factory for I/O
└──────┬──────┘
       │
       ├──► TTerminalController/Input  (no SDL2, pure terminal) - sb.exe
       │
       └──► TVideoController           (SDL2, graphics) - sbv.exe
```

### I/O Modes

| Mode | Description | SDL2 |
|------|-------------|------|
| `ioTerminal` | Pure console (cmd/bash) | No |
| `ioRetroText` | Retro text 40x25 | Yes |
| `ioRetroGfx` | Retro with graphics/sprites | Yes |
| `ioModernText` | Modern text 80x50 | Yes |
| `ioModernGfx` | Full resolution graphics | Yes |

### Related Files

- `SedaiOutputInterface.pas` - `IOutputDevice`, `IInputDevice` interfaces
- `SedaiTerminalIO.pas` - Pure terminal implementation
- `SedaiIOManager.pas` - Mode selection factory
- `SedaiSDL2GraphicsOutput.pas` - SDL2 implementation

---

## Console Behavior (Home Computer Presets)

Emulation of PRINT/INPUT behavior from various home computers:

| Preset | Style |
|--------|-------|
| C64 | Commodore 64 |
| VIC20 | VIC-20 |
| Spectrum | Sinclair ZX Spectrum |
| ZX81 | Sinclair ZX81 |
| MSX | Standard MSX |
| Atari800 | Atari 800/XL/XE |
| CPC | Amstrad CPC |
| AppleII | Apple II |
| BBC | BBC Micro |
| TRS80 | TRS-80 |

File: `SedaiConsoleBehavior.pas`

---

## Compiled Bytecode Format (.basc)

### Proposed Structure

```
┌────────────────────────────────┐
│ Header (32 bytes)              │
│  - Magic: "BASC"               │
│  - Version: u16                │
│  - Flags: u16                  │
│  - InstructionCount: u32       │
│  - StringConstCount: u32       │
│  - VariableCount: u32          │
│  - Checksum: u32               │
├────────────────────────────────┤
│ String Constants Table         │
│  - Length + UTF-8 data         │
├────────────────────────────────┤
│ Variable Metadata              │
│  - Names, types, indices       │
├────────────────────────────────┤
│ Bytecode Instructions          │
│  - Packed instruction stream   │
├────────────────────────────────┤
│ Debug Info (optional)          │
│  - Source line mapping         │
│  - Symbol names                │
└────────────────────────────────┘
```

---

## VM Optimizations

The bytecode compiler implements a sophisticated optimization pipeline running in sequence:

### Implemented (Pipeline Order)

1. ✅ **Constant Folding** - Evaluate constant expressions at compile time
2. ✅ **Dead Block Elimination (DBE)** - Remove unreachable code blocks
3. ✅ **Dominator Tree** - Build control flow dominance information
4. ✅ **Semi-Pruned SSA Construction** - PHI functions and variable versioning
5. ✅ **Global Value Numbering (GVN)** - Eliminate redundant computations
6. ✅ **Algebraic Simplification** - Simplify arithmetic expressions (x*1=x, x+0=x, etc.)
7. ✅ **Strength Reduction** - Replace expensive operations (x*2 → x+x, x/4 → x>>2)
8. ✅ **GOSUB Inlining** - Inline small subroutines
9. ✅ **Aggressive Constant Propagation** - Propagate known constant values
10. ✅ **Copy Propagation** - Eliminate redundant copy chains
11. ✅ **Loop-Invariant Code Motion (LICM)** - Hoist invariants out of loops
12. ✅ **Loop Unrolling** - Unroll small loops for better performance
13. ✅ **Dead Code Elimination (DCE)** - Remove unused instructions and dead PHI nodes
14. ✅ **PHI Elimination** - Convert PHI functions to copy instructions
15. ✅ **Copy Coalescing** - Remove redundant copy instructions
16. ✅ **Linear Scan Register Allocation** - Allocate physical registers
17. ✅ **Peephole Optimization (Pass 1)** - Local instruction patterns
18. ✅ **Superinstruction Fusion** - Combine instruction sequences (50 superinstructions)
19. ✅ **ArrayStoreConst Fusion** - Optimize array constant stores
20. ✅ **NOP Compaction** - Remove NOPs after fusion
21. ✅ **Peephole Optimization (Pass 2)** - Redundant jump elimination
22. ✅ **Register Compaction** - Minimize register file usage

### Planned

- 📋 Register Pre-allocation (Step 5 design ready)
- 📋 Inline caching for function calls

---

## Notes

This document evolves with the project. It does not represent commitments or deadlines, but exploratory directions.

*Last updated: 2025-12-12*
