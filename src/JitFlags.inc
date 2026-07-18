{ ============================================================================
  JitFlags.inc - compile-time flags for the hot-loop JIT (VM performance work).

  JIT_PROFILE: instrument the interpreter with back-edge counting so `sb --jit-profile`
  can report hot loops (milestone J1). OFF by default: a diagnostic must not touch the
  interpreter's hot path at all, so with this undefined the counter field, the per-branch
  increment, DumpHotLoops and the CLI flag compile to NOTHING.

  Uncomment to build a profiling `sb`, then run `sb --jit-profile program.bas`.
  ============================================================================ }

{.$DEFINE JIT_PROFILE}
