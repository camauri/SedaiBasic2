/* hotdisp.c - the interpreter's hot opcodes, compiled by a C compiler rather than by FPC.
 *
 * WHY THIS EXISTS. Measured on 19 Aug 2026: the same dispatch loop - same arms, same eleven values
 * live across every iteration - runs in 253 ms under gcc -O2 and 443 ms under FPC, on the same
 * machine, with no optimisation level of FPC closing any of it (-O1 454, -O2 455, -O3 448, -O4 453).
 * The gap is FPC's register allocator: gcc keeps the hot pointers in registers (5 frame accesses in
 * 1001 instructions) where FPC spills them (27). Inline assembly was tried and is WORSE (+8%),
 * because an asm block is opaque to the compiler and forces spills around itself.
 *
 * WHAT IT IS ALLOWED TO DO. Only opcodes whose whole effect is on the register banks, the arrays
 * and the program counter. Anything else - a division that can raise, a dialect-dependent
 * conversion, a bounds check that must throw, a string, a call, an I/O - RETURNS the PC and lets
 * the Pascal loop run that one instruction. So an unsupported opcode costs one C call, paid once
 * per COLD instruction, never per hot one. What it costs when it lands INSIDE a hot loop was
 * measured on 19 Aug: three uncovered opcodes in spectral-norm's 22-instruction inner loop split it
 * into four covered runs and made the whole thing 15% SLOWER than no C loop at all. Covering them
 * took it to 14.6 s from 34.4.
 *
 * HOW IT DISPATCHES. Not on the opcode: the Pascal side precomputes, per PC, the INDEX of the arm
 * that runs it (0 = not ours), so the loop is one load and one indirect jump with no decoding. The
 * indices come from the table this file publishes through sedai_hot_ops, which is why there is no
 * second copy of the opcode list anywhere to drift out of step.
 *
 * ⛔ FREESTANDING ON PURPOSE: no libc, no globals, every operand arrives as a pointer. That is what
 * makes it link into an FPC program with a plain {$L hotdisp.o} and no C runtime, identically on
 * Linux and on win64 (where FPC's cdecl IS the Microsoft x64 ABI that MinGW-w64 emits).
 *
 * ⛔ REQUIRES gcc or clang, for the labels-as-values extension used by the dispatch table.
 *
 * ⚠️ EVERY ARM IS TRANSCRIBED FROM RunTemplate.inc, and the two must not drift. An arm here that
 * disagrees with the Pascal one is a miscompile that shows only on the programs that reach it.
 */
#include <stdint.h>

typedef struct { uint16_t op, dest, s1, s2; int64_t imm; } SbInstr;

#define HF_MODERN_ARRAYS 1
#define HF_MODERN_CONV   2

/* THE list. One entry per arm, and it drives three things that used to be maintained separately:
   the opcode table handed to the Pascal side, the dispatch table, and the label each arm carries.
   A name in the list with no matching arm is a COMPILE error, which is the point. */
#define HOT_OP_LIST \
  X(0x0068, RecordLoadInt         ) \
  X(0x0069, RecordLoadFloat       ) \
  X(0x006B, RecordStoreInt        ) \
  X(0x006C, RecordStoreFloat      ) \
  X(0x0000, LoadConstInt          ) \
  X(0x0003, CopyInt               ) \
  X(0x0008, AddInt                ) \
  X(0x0009, SubInt                ) \
  X(0x000A, MulInt                ) \
  X(0x000B, DivInt                ) \
  X(0x000C, ModInt                ) \
  X(0x000D, NegInt                ) \
  X(0x002A, BitwiseAnd            ) \
  X(0x002B, BitwiseOr             ) \
  X(0x002C, BitwiseXor            ) \
  X(0x002D, BitwiseNot            ) \
  X(0x0001, LoadConstFloat        ) \
  X(0x0004, CopyFloat             ) \
  X(0x000E, AddFloat              ) \
  X(0x000F, SubFloat              ) \
  X(0x0010, MulFloat              ) \
  X(0x0011, DivFloat              ) \
  X(0x0013, NegFloat              ) \
  X(0x0015, FloatToInt            ) \
  X(0x0206, MathSqr               ) \
  X(0x0309, ArrayLBound           ) \
  X(0x0014, IntToFloat            ) \
  X(0x001A, CmpEqInt              ) \
  X(0x001B, CmpNeInt              ) \
  X(0x001C, CmpLtInt              ) \
  X(0x001D, CmpGtInt              ) \
  X(0x001E, CmpLeInt              ) \
  X(0x001F, CmpGeInt              ) \
  X(0x0020, CmpEqFloat            ) \
  X(0x0021, CmpNeFloat            ) \
  X(0x0022, CmpLtFloat            ) \
  X(0x0023, CmpGtFloat            ) \
  X(0x0024, CmpLeFloat            ) \
  X(0x0025, CmpGeFloat            ) \
  X(0xC80C, AddIntTo              ) \
  X(0xC80D, SubIntTo              ) \
  X(0xC80E, MulIntTo              ) \
  X(0xC80F, AddFloatTo            ) \
  X(0xC810, SubFloatTo            ) \
  X(0xC811, MulFloatTo            ) \
  X(0xC813, AddIntConst           ) \
  X(0xC814, SubIntConst           ) \
  X(0xC815, MulIntConst           ) \
  X(0xC816, AddFloatConst         ) \
  X(0xC817, SubFloatConst         ) \
  X(0xC818, MulFloatConst         ) \
  X(0xC838, AddIntSelf            ) \
  X(0xC839, SubIntSelf            ) \
  X(0x002E, Jump                  ) \
  X(0x002F, JumpIfZero            ) \
  X(0x0030, JumpIfNotZero         ) \
  X(0xC800, BranchEqInt           ) \
  X(0xC801, BranchNeInt           ) \
  X(0xC802, BranchLtInt           ) \
  X(0xC803, BranchGtInt           ) \
  X(0xC804, BranchLeInt           ) \
  X(0xC805, BranchGeInt           ) \
  X(0xC806, BranchEqFloat         ) \
  X(0xC807, BranchNeFloat         ) \
  X(0xC808, BranchLtFloat         ) \
  X(0xC809, BranchGtFloat         ) \
  X(0xC80A, BranchLeFloat         ) \
  X(0xC80B, BranchGeFloat         ) \
  X(0xC81A, BranchEqZeroInt       ) \
  X(0xC81B, BranchNeZeroInt       ) \
  X(0xC821, AddIntToBranchLe      ) \
  X(0xC822, AddIntToBranchLt      ) \
  X(0xC823, SubIntToBranchGe      ) \
  X(0xC824, SubIntToBranchGt      ) \
  X(0x0303, ArrayLoadInt          ) \
  X(0x0304, ArrayLoadFloat        ) \
  X(0x0306, ArrayStoreInt         ) \
  X(0x0307, ArrayStoreFloat       ) \
  X(0xC825, MulAddFloat           ) \
  X(0xC826, MulSubFloat           ) \
  X(0xC827, MulAddToFloat         ) \
  X(0xC828, MulSubToFloat         ) \
  X(0xC82C, SquareSumFloat        ) \
  X(0xC82D, AddSquareFloat        ) \
  X(0xC82E, MulMulFloat           ) \
  X(0x0061, XferStoreInt          ) \
  X(0x0062, XferStoreFloat        ) \
  X(0x0064, XferLoadInt           ) \
  X(0x0065, XferLoadFloat         ) \
  X(0xC829, ArrayLoadAddFloat     ) \
  X(0xC82A, ArrayLoadSubFloat     ) \
  X(0xC83A, ArrayLoadIntTo        ) \
  X(0xC81E, ArrayStoreIntConst    ) \
  X(0xC830, ArrayLoadIntBranchNZ  ) \
  X(0xC831, ArrayLoadIntBranchZ   )


#define X(hex, name) hex,
static const uint16_t hot_ops[] = { HOT_OP_LIST };
#undef X

#define HOT_OP_N ((int)(sizeof hot_ops / sizeof hot_ops[0]))

/* The opcodes this file implements, in DISPATCH-TABLE order: entry i of this list is run by arm i.
   The Pascal side turns it into a per-PC index. Published rather than duplicated. */
int sedai_hot_ops(const uint16_t **out)
{
  *out = hot_ops;
  return HOT_OP_N;
}

/* arrdesc: the JIT/AOT array-descriptor table, FOUR int64 per array - IntData pointer, FloatData
 * pointer, TotalSize, lower bound of dim 0 - built by RebuildJitArrDesc. Reused rather than
 * reinvented: a second copy of that knowledge is how this VM has been bitten before.
 *
 * flags: HF_MODERN_ARRAYS - MODERN follows FreeBASIC, where an out-of-bounds read yields the default
 * and an out-of-bounds store is dropped, both expressible here. CLASSIC must RAISE Commodore's
 * ?BAD SUBSCRIPT, and so must --bounds-check, and this function cannot raise: without the bit the
 * array arms hand the PC back and the Pascal loop does it.
 * HF_MODERN_CONV - MODERN rounds an implicit float-to-int to nearest, ties to even (FreeBASIC);
 * CLASSIC truncates (Commodore v7). One bit, two instructions.
 *
 * hidx: per PC, 1 + the index of the arm that runs it, or 0 for an instruction that is not ours. */
int sedai_hot_run(const SbInstr *prog, int64_t *ireg, double *freg,
                  int pc, int count, int64_t tv,
                  const int64_t *arrdesc, int flags,
                  int64_t *xi, double *xf, const uint16_t *hidx,
                  const int64_t *recdesc)
{
  /* RECORD FIELDS. recdesc is built on the Pascal side, which is the side that knows the layout of
     TRecordStorage - this file holds no offset of its own, so the two cannot drift the way a
     hand-copied struct would. Its six slots are:
        [0] base of the executing context's per-thread Records array (0 = none)
        [1] stride between two records = SizeOf(TRecordStorage)
        [2] byte offset of the Bytes field inside TRecordStorage
        [3] base of the shared-record pointer table, or 0 when the per-access lock is in force -
            and 0 makes every shared handle leave the C loop, which is the prudent answer
        [4] SHARED_REC_FLAG   [5] SHARED_REC_MASK
     A null recdesc disables all four arms. */
#define RECPTR(h_, out_) do {                                                     \
    int64_t hh_ = (h_);                                                           \
    if (!recdesc) return pc;                                                      \
    if (hh_ & recdesc[4]) {                                                       \
      if (!recdesc[3]) return pc;               /* locked mode: not ours */       \
      (out_) = ((char *const *)(intptr_t)recdesc[3])[hh_ & recdesc[5]];           \
    } else {                                                                      \
      if (!recdesc[0] || hh_ < 0) return pc;                                      \
      (out_) = (char *)(intptr_t)recdesc[0] + hh_ * recdesc[1];                   \
    }                                                                             \
    if (!(out_)) return pc;                                                       \
  } while (0)

/* The field byte image, then the width code in the low nibble of the immediate - transcribed from
   RecFieldInt / RecSetFieldInt in SedaiBytecodeVM.pas. A record whose Bytes array is still nil
   hands the PC back rather than dereferencing it. */
#define RECBYTES(rec_, enc_, out_) do {                                           \
    uint8_t *b_ = *(uint8_t **)((rec_) + recdesc[2]);                             \
    if (!b_) return pc;                                                           \
    (out_) = b_ + ((enc_) >> 4);                                                  \
  } while (0)

#define X(hex, name) &&L_##name,
  static void *const disp[] = { HOT_OP_LIST };
#undef X

  const SbInstr *I;

/* One load, one test, one indirect jump. The bound test also catches a negative pc. */
#define NEXT  do { if ((unsigned)pc >= (unsigned)count) return pc;                \
                   { unsigned h_ = hidx[pc]; if (!h_) return pc;                  \
                     I = prog + pc; goto *disp[h_ - 1]; } } while (0)

  NEXT;   /* entry is the same step as every other */

  /* RECORD FIELDS, transcribed from RunTemplate.inc's four bcRecordLoad and bcRecordStore arms and from
     RecFieldInt / RecFieldFloat / RecSetFieldInt / RecSetFieldFloat in SedaiBytecodeVM.pas.
     The width lives in the low nibble of the immediate and the byte offset in the rest of it; code 7
     is a SINGLE, which is four bytes and not a widened Double. bcRecordLoadString and its store are
     deliberately absent - the string bank is off limits to this loop.

     ⭐ WHY THESE FOUR EARN THEIR PLACE. Measured 21 Aug 2026 on binary-trees-modern (records) and
     binary-trees-modern-arena (the same algorithm over a flat array), N=16, interpreter only:
         arena, C loop on 0.566 s / off 0.821 s .... the C loop is worth 45%
         records, C loop on 1.252 s / off 1.231 s .. worth NOTHING
     A record field appearing in a hot loop split it into covered runs too short to pay for
     themselves, which is the exact failure the header of this file already records for
     spectral-norm. On a probe isolating one field read, the field cost +132.6% of the loop with the
     C loop on and +32.2% with it off - the same +32% an array read costs. So the field was never
     expensive; it was expensive only because it left. */
  L_RecordLoadInt: {
    char *rec_; uint8_t *p_; int64_t enc_ = I->imm;
    RECPTR(ireg[I->s1], rec_);
    RECBYTES(rec_, enc_, p_);
    switch (enc_ & 0xF) {
      case 1:  ireg[I->dest] = *(int8_t   *)p_; break;
      case 2:  ireg[I->dest] = *(uint8_t  *)p_; break;
      case 3:  ireg[I->dest] = *(int16_t  *)p_; break;
      case 4:  ireg[I->dest] = *(uint16_t *)p_; break;
      case 5:  ireg[I->dest] = *(int32_t  *)p_; break;
      case 6:  ireg[I->dest] = *(uint32_t *)p_; break;
      default: ireg[I->dest] = *(int64_t  *)p_; break;
    }
    pc++; } NEXT;
  L_RecordLoadFloat: {
    char *rec_; uint8_t *p_; int64_t enc_ = I->imm;
    RECPTR(ireg[I->s1], rec_);
    RECBYTES(rec_, enc_, p_);
    freg[I->dest] = ((enc_ & 0xF) == 7) ? (double)*(float *)p_ : *(double *)p_;
    pc++; } NEXT;
  L_RecordStoreInt: {
    char *rec_; uint8_t *p_; int64_t enc_ = I->imm, v_ = ireg[I->s2];
    RECPTR(ireg[I->s1], rec_);
    RECBYTES(rec_, enc_, p_);
    switch (enc_ & 0xF) {
      case 1: case 2: *(uint8_t  *)p_ = (uint8_t )v_; break;
      case 3: case 4: *(uint16_t *)p_ = (uint16_t)v_; break;
      case 5: case 6: *(uint32_t *)p_ = (uint32_t)v_; break;
      default:        *(int64_t  *)p_ = v_;           break;
    }
    pc++; } NEXT;
  L_RecordStoreFloat: {
    char *rec_; uint8_t *p_; int64_t enc_ = I->imm; double v_ = freg[I->s2];
    RECPTR(ireg[I->s1], rec_);
    RECBYTES(rec_, enc_, p_);
    if ((enc_ & 0xF) == 7) *(float *)p_ = (float)v_; else *(double *)p_ = v_;
    pc++; } NEXT;

  L_LoadConstInt: ireg[I->dest] = I->imm;                                        pc++; NEXT;
  L_CopyInt: ireg[I->dest] = ireg[I->s1];                                   pc++; NEXT;
  L_AddInt: ireg[I->dest] = ireg[I->s1] + ireg[I->s2];                     pc++; NEXT;
  L_SubInt: ireg[I->dest] = ireg[I->s1] - ireg[I->s2];                     pc++; NEXT;
  L_MulInt: ireg[I->dest] = ireg[I->s1] * ireg[I->s2];                     pc++; NEXT;
    /* ⚠️ The two cases C cannot execute are exactly the two the Pascal arm RAISES on: a zero divisor
     * (EDivByZero) and INT64_MIN \ -1, whose quotient 2^63 no Int64 holds (EIntOverflow). Both hand
     * the PC back. Everywhere else C's / and FPC's div agree - both truncate toward zero. */
  L_DivInt:
      if (ireg[I->s2] == 0 || (ireg[I->s1] == INT64_MIN && ireg[I->s2] == -1)) return pc;
      ireg[I->dest] = ireg[I->s1] / ireg[I->s2];                                pc++; NEXT;
    /* Mod parts company with Div on INT64_MIN Mod -1: the remainder is 0, it FITS, and the Pascal
     * arm answers it rather than inheriting a hardware fault raised for a quotient nobody asked for
     * (user decision, 13 Aug 2026). So only the zero divisor goes back. */
  L_ModInt:
      if (ireg[I->s2] == 0) return pc;
      ireg[I->dest] = (ireg[I->s1] == INT64_MIN && ireg[I->s2] == -1)
                        ? 0 : ireg[I->s1] % ireg[I->s2];                        pc++; NEXT;
  L_NegInt: ireg[I->dest] = -ireg[I->s1];                                  pc++; NEXT;
  L_BitwiseAnd: ireg[I->dest] = ireg[I->s1] & ireg[I->s2];                     pc++; NEXT;
  L_BitwiseOr: ireg[I->dest] = ireg[I->s1] | ireg[I->s2];                     pc++; NEXT;
  L_BitwiseXor: ireg[I->dest] = ireg[I->s1] ^ ireg[I->s2];                     pc++; NEXT;
  L_BitwiseNot: ireg[I->dest] = ~ireg[I->s1];                                  pc++; NEXT;
  L_LoadConstFloat: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = d; } pc++; NEXT;
  L_CopyFloat: freg[I->dest] = freg[I->s1];                                   pc++; NEXT;
  L_AddFloat: freg[I->dest] = freg[I->s1] + freg[I->s2];                     pc++; NEXT;
  L_SubFloat: freg[I->dest] = freg[I->s1] - freg[I->s2];                     pc++; NEXT;
  L_MulFloat: freg[I->dest] = freg[I->s1] * freg[I->s2];                     pc++; NEXT;
    /* A zero divisor is dialect-dependent - MODERN yields IEEE Inf/NaN, CLASSIC raises - so it goes
     * back to DivZeroFloat. Every other divisor is the same IEEE division on both sides. */
  L_DivFloat:
      if (freg[I->s2] == 0.0) return pc;
      freg[I->dest] = freg[I->s1] / freg[I->s2];                                pc++; NEXT;
  L_NegFloat: freg[I->dest] = -freg[I->s1];                                  pc++; NEXT;

  /* The implicit float-to-int conversion. MODERN rounds to nearest, ties to even - which is what
     x86's cvtsd2si does natively, because IEEE 754 makes that the DEFAULT rounding direction and
     MXCSR starts there. CLASSIC truncates, which is the cast. One bit picks the instruction.
     ⚠️ __builtin_llrint only becomes cvtsd2si under -fno-math-errno; without it gcc emits a call to
     libm, which a freestanding object cannot even link. See build.sh.
     An UNSIGNED destination (Immediate 1) is a genuinely different conversion with its own NaN and
     out-of-range rules - see FloatToUIntConv - and hands the PC back. */
  L_FloatToInt:
      if (I->imm == 1) return pc;
      ireg[I->dest] = (flags & HF_MODERN_CONV) ? __builtin_llrint(freg[I->s1])
                                               : (int64_t)freg[I->s1];
      pc++; NEXT;

  /* Sqr of a NEGATIVE is dialect-dependent - MODERN answers a NaN whose sign bit is CLEAR (FPC's own
     NaN constant has it SET, so this arm must not invent one) and CLASSIC raises ?ILLEGAL QUANTITY.
     Both go back. Everything else is one sqrtsd. */
  L_MathSqr:
      if (!(freg[I->s1] >= 0.0)) return pc;   /* also sends NaN back, which the Pascal arm handles */
      freg[I->dest] = __builtin_sqrt(freg[I->s1]);
      pc++; NEXT;

  /* LBOUND(arr, dim). Only dim 0 is in the descriptor, so other dimensions and the rank query (a
     NEGATIVE index, which answers 1) hand the PC back - the same line the JIT draws from the same
     table. UBOUND is deliberately NOT here: it needs Dimensions[0], which is TotalSize only for a
     one-dimensional array, and this side cannot tell the rank. */
  L_ArrayLBound:
      if (ireg[I->s2] != 0) return pc;
      ireg[I->dest] = arrdesc[4*(int)I->s1 + 3];
      pc++; NEXT;
    /* Immediate is BITS, not a choice of values: bit 0 = the source is UNSIGNED, bit 1 = the result
     * rounds to binary32. Only the plain signed-to-double case is here, and it is exact. The other
     * three hand the PC back: that is where C's rounding and FPC's could disagree, and a conversion
     * that is ALMOST right is worse than one that costs a call. */
  L_IntToFloat:
      if (I->imm != 0) return pc;
      freg[I->dest] = (double)ireg[I->s1];                                      pc++; NEXT;
  L_CmpEqInt: ireg[I->dest] = (ireg[I->s1] == ireg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpNeInt: ireg[I->dest] = (ireg[I->s1] != ireg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpLtInt: ireg[I->dest] = (ireg[I->s1] <  ireg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpGtInt: ireg[I->dest] = (ireg[I->s1] >  ireg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpLeInt: ireg[I->dest] = (ireg[I->s1] <= ireg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpGeInt: ireg[I->dest] = (ireg[I->s1] >= ireg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpEqFloat: ireg[I->dest] = (freg[I->s1] == freg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpNeFloat: ireg[I->dest] = (freg[I->s1] != freg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpLtFloat: ireg[I->dest] = (freg[I->s1] <  freg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpGtFloat: ireg[I->dest] = (freg[I->s1] >  freg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpLeFloat: ireg[I->dest] = (freg[I->s1] <= freg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_CmpGeFloat: ireg[I->dest] = (freg[I->s1] >= freg[I->s2]) ? tv : 0;         pc++; NEXT;
  L_AddIntTo: ireg[I->dest] = ireg[I->dest] + ireg[I->s1];                   pc++; NEXT;
  L_SubIntTo: ireg[I->dest] = ireg[I->dest] - ireg[I->s1];                   pc++; NEXT;
  L_MulIntTo: ireg[I->dest] = ireg[I->dest] * ireg[I->s1];                   pc++; NEXT;
  L_AddFloatTo: freg[I->dest] = freg[I->dest] + freg[I->s1];                   pc++; NEXT;
  L_SubFloatTo: freg[I->dest] = freg[I->dest] - freg[I->s1];                   pc++; NEXT;
  L_MulFloatTo: freg[I->dest] = freg[I->dest] * freg[I->s1];                   pc++; NEXT;
  L_AddIntConst: ireg[I->dest] = ireg[I->s1] + I->imm;                          pc++; NEXT;
  L_SubIntConst: ireg[I->dest] = ireg[I->s1] - I->imm;                          pc++; NEXT;
  L_MulIntConst: ireg[I->dest] = ireg[I->s1] * I->imm;                          pc++; NEXT;
  L_AddFloatConst: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = freg[I->s1] + d; } pc++; NEXT;
  L_SubFloatConst: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = freg[I->s1] - d; } pc++; NEXT;
  L_MulFloatConst: { double d; __builtin_memcpy(&d, &I->imm, 8); freg[I->dest] = freg[I->s1] * d; } pc++; NEXT;
  L_AddIntSelf: ireg[I->dest] += ireg[I->s1];                                  pc++; NEXT;
  L_SubIntSelf: ireg[I->dest] -= ireg[I->s1];                                  pc++; NEXT;
  L_Jump: pc = (int)I->imm; NEXT;
  L_JumpIfZero: pc = (ireg[I->s1] == 0) ? (int)I->imm : pc + 1; NEXT;
  L_JumpIfNotZero: pc = (ireg[I->s1] != 0) ? (int)I->imm : pc + 1; NEXT;
  L_BranchEqInt: pc = (ireg[I->s1] == ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchNeInt: pc = (ireg[I->s1] != ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchLtInt: pc = (ireg[I->s1] <  ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchGtInt: pc = (ireg[I->s1] >  ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchLeInt: pc = (ireg[I->s1] <= ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchGeInt: pc = (ireg[I->s1] >= ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchEqFloat: pc = (freg[I->s1] == freg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchNeFloat: pc = (freg[I->s1] != freg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchLtFloat: pc = (freg[I->s1] <  freg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchGtFloat: pc = (freg[I->s1] >  freg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchLeFloat: pc = (freg[I->s1] <= freg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchGeFloat: pc = (freg[I->s1] >= freg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_BranchEqZeroInt: pc = (ireg[I->s1] == 0) ? (int)I->imm : pc + 1; NEXT;
  L_BranchNeZeroInt: pc = (ireg[I->s1] != 0) ? (int)I->imm : pc + 1; NEXT;
  L_AddIntToBranchLe: ireg[I->dest] += ireg[I->s1]; pc = (ireg[I->dest] <= ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_AddIntToBranchLt: ireg[I->dest] += ireg[I->s1]; pc = (ireg[I->dest] <  ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_SubIntToBranchGe: ireg[I->dest] -= ireg[I->s1]; pc = (ireg[I->dest] >= ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;
  L_SubIntToBranchGt: ireg[I->dest] -= ireg[I->s1]; pc = (ireg[I->dest] >  ireg[I->s2]) ? (int)I->imm : pc + 1; NEXT;

    /* ---- typed array element access. Src1 is the ARRAY ID, Src2 the register holding the index. ---- */
  L_ArrayLoadInt:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        ireg[I->dest] = (li >= 0 && li < d[2]) ? ((const int64_t *)(intptr_t)d[0])[li] : 0; }
      pc++; NEXT;
  L_ArrayLoadFloat:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        freg[I->dest] = (li >= 0 && li < d[2]) ? ((const double *)(intptr_t)d[1])[li] : 0.0; }
      pc++; NEXT;
  L_ArrayStoreInt:   /* bcArrayStoreInt - the VALUE is in Dest, read not written */
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if (li >= 0 && li < d[2]) ((int64_t *)(intptr_t)d[0])[li] = ireg[I->dest]; }
      pc++; NEXT;
  L_ArrayStoreFloat:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if (li >= 0 && li < d[2]) ((double *)(intptr_t)d[1])[li] = freg[I->dest]; }
      pc++; NEXT;

    /* ---- fused float arithmetic and the transfer banks ---- */
  L_MulAddFloat: freg[I->dest] = freg[I->imm] + freg[I->s1] * freg[I->s2];          pc++; NEXT;
  L_MulSubFloat: freg[I->dest] = freg[I->imm] - freg[I->s1] * freg[I->s2];          pc++; NEXT;
  L_MulAddToFloat: freg[I->dest] = freg[I->dest] + freg[I->s1] * freg[I->s2];         pc++; NEXT;
  L_MulSubToFloat: freg[I->dest] = freg[I->dest] - freg[I->s1] * freg[I->s2];         pc++; NEXT;
  L_SquareSumFloat: freg[I->dest] = freg[I->s1]*freg[I->s1] + freg[I->s2]*freg[I->s2]; pc++; NEXT;
  L_AddSquareFloat: freg[I->dest] = freg[I->s1] + freg[I->s2]*freg[I->s2];             pc++; NEXT;
  L_MulMulFloat: freg[I->dest] = freg[I->s1] * freg[I->s2] * freg[I->imm];          pc++; NEXT;
  L_XferStoreInt: xi[I->imm] = ireg[I->s1];                                          pc++; NEXT;
  L_XferStoreFloat: xf[I->imm] = freg[I->s1];                                          pc++; NEXT;
  L_XferLoadInt: ireg[I->dest] = xi[I->imm];                                        pc++; NEXT;
  L_XferLoadFloat: freg[I->dest] = xf[I->imm];                                        pc++; NEXT;

    /* ---- fused array forms. MODERN only, for the same reason as the plain accessors. ---- */
  L_ArrayLoadAddFloat:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        freg[I->dest] = freg[I->imm] + ((li >= 0 && li < d[2]) ? ((const double *)(intptr_t)d[1])[li] : 0.0); }
      pc++; NEXT;
  L_ArrayLoadSubFloat:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        freg[I->dest] = freg[I->imm] - ((li >= 0 && li < d[2]) ? ((const double *)(intptr_t)d[1])[li] : 0.0); }
      pc++; NEXT;
  L_ArrayLoadIntTo:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        ireg[I->dest] = (li >= 0 && li < d[2]) ? ((const int64_t *)(intptr_t)d[0])[li] : 0; }
      pc++; NEXT;
  L_ArrayStoreIntConst:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if (li >= 0 && li < d[2]) ((int64_t *)(intptr_t)d[0])[li] = I->imm; }
      pc++; NEXT;
  L_ArrayLoadIntBranchNZ:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        int nz = (li >= 0 && li < d[2]) && ((const int64_t *)(intptr_t)d[0])[li] != 0;
        pc = nz ? (int)I->imm : pc + 1; } NEXT;
  L_ArrayLoadIntBranchZ:
      if (!(flags & HF_MODERN_ARRAYS)) return pc;
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        int z = !(li >= 0 && li < d[2]) || ((const int64_t *)(intptr_t)d[0])[li] == 0;
        pc = z ? (int)I->imm : pc + 1; } NEXT;

#undef NEXT
#undef HOT_OP_LIST
}
