/* hotdisp.c - the interpreter's hot arithmetic/branch opcodes, compiled by a C compiler.
 *
 * WHY THIS EXISTS. Measured on 19 Aug 2026: the same dispatch loop - same 240 arms, same eleven
 * values live across every iteration - runs in 253 ms under gcc -O2 and 443 ms under FPC, on the
 * same machine, with no optimisation level of FPC closing any of it (-O1 454, -O2 455, -O3 448,
 * -O4 453). The gap is FPC's register allocator: gcc keeps the hot pointers in registers (5 frame
 * accesses in 1001 instructions) where FPC spills them (27). Inline assembly was tried and is
 * WORSE (+8%), because an asm block is opaque to the compiler and forces spills around itself.
 *
 * WHAT IT IS ALLOWED TO DO. Only opcodes whose whole effect is on the two register banks and the
 * program counter. Anything else - a division that can raise, a dialect-dependent conversion, an
 * array access with a bounds check, a string, a call, an I/O - RETURNS the PC and lets the Pascal
 * loop run that one instruction. So the cost of an unsupported opcode is one C call, paid once per
 * COLD instruction, never per hot one.
 *
 * ⛔ FREESTANDING ON PURPOSE: no libc, no globals, every operand arrives as a pointer. That is what
 * makes it link into an FPC program with a plain {$L hotdisp.o} and no C runtime, identically on
 * Linux and on win64 (where FPC's cdecl IS the Microsoft x64 ABI that MinGW-w64 emits).
 *
 * ⚠️ EVERY ARM IS TRANSCRIBED FROM RunTemplate.inc, and the two must not drift. An arm here that
 * disagrees with the Pascal one is a miscompile that only shows on the programs that reach it.
 */
#include <stdint.h>

typedef struct { uint16_t op, dest, s1, s2; int64_t imm; } SbInstr;

/* arrdesc: the JIT/AOT array-descriptor table, FOUR int64 per array - IntData pointer, FloatData
 * pointer, TotalSize, lower bound of dim 0 - built by RebuildJitArrDesc. Reused rather than
 * reinvented: a second copy of that knowledge is how this VM has been bitten before.
 *
 * modern_arrays: MODERN follows FreeBASIC, where an out-of-bounds read yields the default and an
 * out-of-bounds store is dropped - both expressible here. CLASSIC must RAISE Commodore's
 * ?BAD SUBSCRIPT, and so must --bounds-check, and this function cannot raise: when the flag is 0
 * the array arms hand the PC back and the Pascal loop does it. */
int sedai_hot_run(const SbInstr *prog, int64_t *ireg, double *freg,
                  int pc, int count, int64_t tv,
                  const int64_t *arrdesc, int modern_arrays)
{
  for (;;) {
    if (pc < 0 || pc >= count) return pc;
    const SbInstr *I = prog + pc;
    switch (I->op) {
    case 0x0000: ireg[I->dest] = I->imm;                                        pc++; break;   /* bcLoadConstInt */
    case 0x0003: ireg[I->dest] = ireg[I->s1];                                   pc++; break;   /* bcCopyInt */
    case 0x0008: ireg[I->dest] = ireg[I->s1] + ireg[I->s2];                     pc++; break;   /* bcAddInt */
    case 0x0009: ireg[I->dest] = ireg[I->s1] - ireg[I->s2];                     pc++; break;   /* bcSubInt */
    case 0x000A: ireg[I->dest] = ireg[I->s1] * ireg[I->s2];                     pc++; break;   /* bcMulInt */
    case 0x000D: ireg[I->dest] = -ireg[I->s1];                                  pc++; break;   /* bcNegInt */
    case 0x002A: ireg[I->dest] = ireg[I->s1] & ireg[I->s2];                     pc++; break;   /* bcBitwiseAnd */
    case 0x002B: ireg[I->dest] = ireg[I->s1] | ireg[I->s2];                     pc++; break;   /* bcBitwiseOr */
    case 0x002C: ireg[I->dest] = ireg[I->s1] ^ ireg[I->s2];                     pc++; break;   /* bcBitwiseXor */
    case 0x002D: ireg[I->dest] = ~ireg[I->s1];                                  pc++; break;   /* bcBitwiseNot */
    case 0x0001: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = d; } pc++; break;   /* bcLoadConstFloat */
    case 0x0004: freg[I->dest] = freg[I->s1];                                   pc++; break;   /* bcCopyFloat */
    case 0x000E: freg[I->dest] = freg[I->s1] + freg[I->s2];                     pc++; break;   /* bcAddFloat */
    case 0x000F: freg[I->dest] = freg[I->s1] - freg[I->s2];                     pc++; break;   /* bcSubFloat */
    case 0x0010: freg[I->dest] = freg[I->s1] * freg[I->s2];                     pc++; break;   /* bcMulFloat */
    case 0x0013: freg[I->dest] = -freg[I->s1];                                  pc++; break;   /* bcNegFloat */
    case 0x001A: ireg[I->dest] = (ireg[I->s1] == ireg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpEqInt */
    case 0x001B: ireg[I->dest] = (ireg[I->s1] != ireg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpNeInt */
    case 0x001C: ireg[I->dest] = (ireg[I->s1] <  ireg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpLtInt */
    case 0x001D: ireg[I->dest] = (ireg[I->s1] >  ireg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpGtInt */
    case 0x001E: ireg[I->dest] = (ireg[I->s1] <= ireg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpLeInt */
    case 0x001F: ireg[I->dest] = (ireg[I->s1] >= ireg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpGeInt */
    case 0x0020: ireg[I->dest] = (freg[I->s1] == freg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpEqFloat */
    case 0x0021: ireg[I->dest] = (freg[I->s1] != freg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpNeFloat */
    case 0x0022: ireg[I->dest] = (freg[I->s1] <  freg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpLtFloat */
    case 0x0023: ireg[I->dest] = (freg[I->s1] >  freg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpGtFloat */
    case 0x0024: ireg[I->dest] = (freg[I->s1] <= freg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpLeFloat */
    case 0x0025: ireg[I->dest] = (freg[I->s1] >= freg[I->s2]) ? tv : 0;         pc++; break;   /* bcCmpGeFloat */
    case 0xC80C: ireg[I->dest] = ireg[I->dest] + ireg[I->s1];                   pc++; break;   /* bcAddIntTo */
    case 0xC80D: ireg[I->dest] = ireg[I->dest] - ireg[I->s1];                   pc++; break;   /* bcSubIntTo */
    case 0xC80E: ireg[I->dest] = ireg[I->dest] * ireg[I->s1];                   pc++; break;   /* bcMulIntTo */
    case 0xC80F: freg[I->dest] = freg[I->dest] + freg[I->s1];                   pc++; break;   /* bcAddFloatTo */
    case 0xC810: freg[I->dest] = freg[I->dest] - freg[I->s1];                   pc++; break;   /* bcSubFloatTo */
    case 0xC811: freg[I->dest] = freg[I->dest] * freg[I->s1];                   pc++; break;   /* bcMulFloatTo */
    case 0xC813: ireg[I->dest] = ireg[I->s1] + I->imm;                          pc++; break;   /* bcAddIntConst */
    case 0xC814: ireg[I->dest] = ireg[I->s1] - I->imm;                          pc++; break;   /* bcSubIntConst */
    case 0xC815: ireg[I->dest] = ireg[I->s1] * I->imm;                          pc++; break;   /* bcMulIntConst */
    case 0xC816: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = freg[I->s1] + d; } pc++; break;   /* bcAddFloatConst */
    case 0xC817: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = freg[I->s1] - d; } pc++; break;   /* bcSubFloatConst */
    case 0xC818: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = freg[I->s1] * d; } pc++; break;   /* bcMulFloatConst */
    case 0xC838: ireg[I->dest] += ireg[I->s1];                                  pc++; break;   /* bcAddIntSelf */
    case 0xC839: ireg[I->dest] -= ireg[I->s1];                                  pc++; break;   /* bcSubIntSelf */

    case 0x002E: pc = (int)I->imm; continue;                                              /* bcJump */
    case 0x002F: pc = (ireg[I->s1] == 0) ? (int)I->imm : pc + 1; continue;                /* bcJumpIfZero */
    case 0x0030: pc = (ireg[I->s1] != 0) ? (int)I->imm : pc + 1; continue;                /* bcJumpIfNotZero */
    case 0xC800: pc = (ireg[I->s1] == ireg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchEqInt */
    case 0xC801: pc = (ireg[I->s1] != ireg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchNeInt */
    case 0xC802: pc = (ireg[I->s1] <  ireg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchLtInt */
    case 0xC803: pc = (ireg[I->s1] >  ireg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchGtInt */
    case 0xC804: pc = (ireg[I->s1] <= ireg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchLeInt */
    case 0xC805: pc = (ireg[I->s1] >= ireg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchGeInt */
    case 0xC806: pc = (freg[I->s1] == freg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchEqFloat */
    case 0xC807: pc = (freg[I->s1] != freg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchNeFloat */
    case 0xC808: pc = (freg[I->s1] <  freg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchLtFloat */
    case 0xC809: pc = (freg[I->s1] >  freg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchGtFloat */
    case 0xC80A: pc = (freg[I->s1] <= freg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchLeFloat */
    case 0xC80B: pc = (freg[I->s1] >= freg[I->s2]) ? (int)I->imm : pc + 1; continue;      /* bcBranchGeFloat */
    case 0xC81A: pc = (ireg[I->s1] == 0) ? (int)I->imm : pc + 1; continue;                /* bcBranchEqZeroInt */
    case 0xC81B: pc = (ireg[I->s1] != 0) ? (int)I->imm : pc + 1; continue;                /* bcBranchNeZeroInt */
    case 0xC821: ireg[I->dest] += ireg[I->s1]; pc = (ireg[I->dest] <= ireg[I->s2]) ? (int)I->imm : pc + 1; continue; /* bcAddIntToBranchLe */
    case 0xC822: ireg[I->dest] += ireg[I->s1]; pc = (ireg[I->dest] <  ireg[I->s2]) ? (int)I->imm : pc + 1; continue; /* bcAddIntToBranchLt */
    case 0xC823: ireg[I->dest] -= ireg[I->s1]; pc = (ireg[I->dest] >= ireg[I->s2]) ? (int)I->imm : pc + 1; continue; /* bcSubIntToBranchGe */
    case 0xC824: ireg[I->dest] -= ireg[I->s1]; pc = (ireg[I->dest] >  ireg[I->s2]) ? (int)I->imm : pc + 1; continue; /* bcSubIntToBranchGt */

    /* ---- typed array element access. Src1 is the ARRAY ID, Src2 the register holding the index. ---- */
    case 0x0303:   /* bcArrayLoadInt */
      if (!modern_arrays) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        ireg[I->dest] = (li >= 0 && li < d[2]) ? ((const int64_t *)(intptr_t)d[0])[li] : 0; }
      pc++; break;
    case 0x0304:   /* bcArrayLoadFloat */
      if (!modern_arrays) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        freg[I->dest] = (li >= 0 && li < d[2]) ? ((const double *)(intptr_t)d[1])[li] : 0.0; }
      pc++; break;
    case 0x0306:   /* bcArrayStoreInt - the VALUE is in Dest, read not written */
      if (!modern_arrays) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if (li >= 0 && li < d[2]) ((int64_t *)(intptr_t)d[0])[li] = ireg[I->dest]; }
      pc++; break;
    case 0x0307:   /* bcArrayStoreFloat */
      if (!modern_arrays) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if (li >= 0 && li < d[2]) ((double *)(intptr_t)d[1])[li] = freg[I->dest]; }
      pc++; break;

    default:
      return pc;   /* not ours: the Pascal loop runs this one, then calls back in */
    }
  }
}
