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

/* ⛔ THE ORDER OF THIS LIST COSTS REAL TIME, AND THERE IS NO RULE FOR IT - ONLY MEASUREMENT.
   The arms are indirect-jump targets, so where each one LANDS matters. Adding the four record
   entries cost fannkuch-redux 12% (1.345 -> 1.507 s at N=10) although it touches no record at all:
   bisecting the binaries put the loss at exactly that commit, and switching the C loop off collapsed
   the gap to +2.3%, so it is layout and nothing else.

   ⛔ AND THE FIRST ATTEMPT TO FIX IT MEASURED A LIE. Moving the four entries to the END read as
   spectral-norm +17.4%, n-body +11.9%, fannkuch +5.8% - so "append instead of insert" looked
   decisively wrong. It was not: the edit that moved them had also DROPPED
   X(0xC831, ArrayLoadIntBranchZ) from the list, taking an opcode out of the C loop entirely, and
   that is what those numbers measured. The list went from 95 entries to 94 and nothing complained -
   a name with no arm is a compile error, an ARM WITH NO NAME is silent. Caught by diffing the
   generated .text against HEAD, which is the check worth keeping for any edit to this list.
   Re-run with all 95 present, appending is slightly BETTER on three of four (fannkuch -1.6%,
   binary-trees -1.0%, spectral-norm -0.9%, n-body +0.2%), and that is the arrangement below.

   -falign-labels=32 was swept the same way against none/16/64 and wins on all three programs, so
   that one stays. ⇒ Do not reason about this list's order: change it only with an A/B in hand, on
   more than one program - and diff the .text to be sure the A/B is comparing what you think.

   THE list. One entry per arm, and it drives three things that used to be maintained separately:
   the opcode table handed to the Pascal side, the dispatch table, and the label each arm carries.
   A name in the list with no matching arm is a COMPILE error, which is the point. */
/* -ffreestanding means no math.h; these three are resolved at the FINAL link, where FPC already
   pulls in libm (ldd shows libm.so.6). Prototyped by hand so the object stays freestanding. */
/* ⛔ THE LINKER, NOT THE MATHS, DECIDES WHICH OF THESE IS USED.
   On Unix the bare libc names link straight through - the Pascal RTL has already pulled libm - and
   that is the fast path: measured on a 3 M-iteration loop saturated with Sin/Cos/Tan, calling the
   libc directly runs 0.097 s where forwarding through a Pascal wrapper runs 0.114 s, +17.5%.
   On win64 there is no libm to pull and FPC ships no msvcrt import library, so a bare "sin" left the
   link with "Undefined symbol: sin" - and it did so for anyone cross-building for win64 while NOBODY
   SAW IT, because build.ps1 does not implement the C hot loop at all and no Windows build ever links
   this object. There the three are forwarded from SedaiBytecodeVM, which reaches the CRT through its
   own "external 'msvcrt'" declarations - the very same c_sin/c_cos/c_tan the interpreter and the AOT
   use, so every engine on one platform still answers identically. The extra call is paid only where
   the alternative is not linking at all. */
#ifdef _WIN32
double sb_hot_sin(double);
double sb_hot_cos(double);
double sb_hot_tan(double);
#define HOT_SIN sb_hot_sin
#define HOT_COS sb_hot_cos
#define HOT_TAN sb_hot_tan
#else
double sin(double);
double cos(double);
double tan(double);
#define HOT_SIN sin
#define HOT_COS cos
#define HOT_TAN tan
#endif

#define HOT_OP_LIST \
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
  X(0x0200, MathSin               ) \
  X(0x0201, MathCos               ) \
  X(0x0202, MathTan               ) \
  X(0x0209, MathInt               ) \
  X(0x0A1A, GfxPset               ) \
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
  X(0xC831, ArrayLoadIntBranchZ   ) \
  X(0x0068, RecordLoadInt         ) \
  X(0x0069, RecordLoadFloat       ) \
  X(0x006B, RecordStoreInt        ) \
  X(0x006C, RecordStoreFloat      )



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
                  const int64_t *recdesc, int backedge_budget,
                  const int64_t *gfxdesc)
{
  int be_ = backedge_budget;
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

/* BACK-EDGE BUDGET. Every arm that can move the pc BACKWARDS goes through this instead of writing pc
   itself, so the budget is spent per ITERATION of a BASIC loop and not per instruction executed.
   That distinction is the whole design: a test per instruction inside this loop was measured at
   3.5-11.8%, which would tax every headless program to help the windowed ones. A forward branch and
   a fall-through pay one compare and nothing else.
   When the budget runs out the loop hands the PC back exactly as any uncovered opcode does, and the
   Pascal side pumps events and comes straight back in. With no event callback the caller passes
   INT32_MAX, so a run would have to take two thousand million back edges to see one extra re-entry -
   and one extra re-entry is all it would cost.
   ⭐ A budget exit returns the PC NEGATED as -(pc+1), so the caller can tell it apart from the
   ordinary "I do not implement this opcode" exit WITHOUT a second output parameter and without a
   memory write per back edge. A PC is never negative, so the encoding is unambiguous, and the only
   cost on the hot path is the negation on a branch that is already leaving. */
#define JUMPTO(expr_)  do { int np_ = (expr_);                                    \
                            if (np_ <= pc && --be_ <= 0) return -(np_ + 1);       \
                            pc = np_; } while (0); NEXT

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

  /* SIN/COS/TAN call the platform C library, exactly as the Pascal side now does. They are here
     because a math opcode LEAVING the loop is not neutral: it splits the loop into runs too short
     to pay for their own entry - measured 22 Aug 2026, a Sin/Cos/Int recurrence exited 60 M times
     in 20 M iterations (three per iteration) and the C loop was worth only +20% there against
     +381% on the same loop shape with everything covered.
     ⚠️ These MUST be the same functions the interpreter calls (SedaiBytecodeVM.pas declares them
     external 'm'), or the answer would depend on whether the hot loop ran - HOTC_OFF=1 would
     change the result. FPC's own Sin is NOT the same function: it is the x87's fsin, which loses
     nine significant digits by 1e15. See the note by the declaration in SedaiBytecodeVM.pas. */
  L_MathSin: freg[I->dest] = HOT_SIN(freg[I->s1]);                                pc++; NEXT;
  L_MathCos: freg[I->dest] = HOT_COS(freg[I->s1]);                                pc++; NEXT;
  L_MathTan: freg[I->dest] = HOT_TAN(freg[I->s1]);                                pc++; NEXT;

  /* INT() is FLOOR, not truncation - Int(-1.5) is -2 - which is FloorDouble on the Pascal side and
     __builtin_floor here. Exact in both, so no dialect bit and no way for the two to disagree. */
  L_MathInt: freg[I->dest] = __builtin_floor(freg[I->s1]);                     pc++; NEXT;

  /* PSET (x,y),colour. gfxdesc is built by the Pascal side, which is the side that knows when this
     store is the WHOLE of what PSET does, and it hands over a NULL base whenever it is not: palette
     mode (which searches for the nearest index), an active clip, a WINDOW transform, a VIEW offset,
     or a draw target that is not the plain work page. One test at the top, and the arm is refused
     wholesale - the same shape recdesc uses for a locked shared record.
        [0] framebuffer base of the draw surface, 0 = not ours
        [1] width in pixels   [2] height in pixels
        [3] address of FDrawPenX   [4] address of FDrawPenY
     ⚠️ The pen is set even when the point falls OUTSIDE the surface, because the interpreter sets it
     after SetPixel unconditionally and POINTCOORD reads it. Clipping the store but not the pen is
     what the Pascal arm does, so it is what this one does. */
  L_GfxPset:
      if (!gfxdesc || !gfxdesc[0]) return pc;
      { int64_t x_ = ireg[I->s1], y_ = ireg[I->s2];
        if (x_ >= 0 && x_ < gfxdesc[1] && y_ >= 0 && y_ < gfxdesc[2])
          ((uint32_t *)(intptr_t)gfxdesc[0])[y_ * gfxdesc[1] + x_] = (uint32_t)ireg[I->imm];
        *(int32_t *)(intptr_t)gfxdesc[3] = (int32_t)x_;
        *(int32_t *)(intptr_t)gfxdesc[4] = (int32_t)y_; }
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
  L_Jump: JUMPTO((int)I->imm);
  L_JumpIfZero: JUMPTO((ireg[I->s1] == 0) ? (int)I->imm : pc + 1);
  L_JumpIfNotZero: JUMPTO((ireg[I->s1] != 0) ? (int)I->imm : pc + 1);
  L_BranchEqInt: JUMPTO((ireg[I->s1] == ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchNeInt: JUMPTO((ireg[I->s1] != ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchLtInt: JUMPTO((ireg[I->s1] <  ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchGtInt: JUMPTO((ireg[I->s1] >  ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchLeInt: JUMPTO((ireg[I->s1] <= ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchGeInt: JUMPTO((ireg[I->s1] >= ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchEqFloat: JUMPTO((freg[I->s1] == freg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchNeFloat: JUMPTO((freg[I->s1] != freg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchLtFloat: JUMPTO((freg[I->s1] <  freg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchGtFloat: JUMPTO((freg[I->s1] >  freg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchLeFloat: JUMPTO((freg[I->s1] <= freg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchGeFloat: JUMPTO((freg[I->s1] >= freg[I->s2]) ? (int)I->imm : pc + 1);
  L_BranchEqZeroInt: JUMPTO((ireg[I->s1] == 0) ? (int)I->imm : pc + 1);
  L_BranchNeZeroInt: JUMPTO((ireg[I->s1] != 0) ? (int)I->imm : pc + 1);
  L_AddIntToBranchLe: ireg[I->dest] += ireg[I->s1]; JUMPTO((ireg[I->dest] <= ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_AddIntToBranchLt: ireg[I->dest] += ireg[I->s1]; JUMPTO((ireg[I->dest] <  ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_SubIntToBranchGe: ireg[I->dest] -= ireg[I->s1]; JUMPTO((ireg[I->dest] >= ireg[I->s2]) ? (int)I->imm : pc + 1);
  L_SubIntToBranchGt: ireg[I->dest] -= ireg[I->s1]; JUMPTO((ireg[I->dest] >  ireg[I->s2]) ? (int)I->imm : pc + 1);

    /* ---- typed array element access. Src1 is the ARRAY ID, Src2 the register holding the index. ---- */
    /* ⭐ THE FLAG GUARDS THE OUT-OF-BOUNDS CASE, NOT THE ACCESS. These four used to hand the PC back
       on the very first instruction whenever HF_MODERN_ARRAYS was clear - and that flag is
       "MODERN dialect AND bounds checking off", so EVERY CLASSIC program lost the C loop at its
       first array element. In bounds, the two dialects do exactly the same thing through exactly
       the same descriptor; they differ only when the index is out of range, where MODERN yields
       zero (or drops the store) and CLASSIC has to RAISE. So the in-bounds path is taken in both,
       and only the out-of-range case leaves - which is an error path, not a hot one.

       Measured 21 Aug 2026 by HOTC_DIAG=1 over the benchmark corpus: ArrayLoadFloat alone handed
       the PC back 6.40 M times, 11.2% of all exits, and ArrayLoadInt another 0.91 M - all of it in
       the CLASSIC programs, spectral-norm.bas and n-body.bas among them. */
    /* The in-bounds test is ONE unsigned compare: a negative index wraps to a huge unsigned value
       and fails the same test, so there is no separate li >= 0 branch on the hot path. */
  L_ArrayLoadInt:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) ireg[I->dest] = ((const int64_t *)(intptr_t)d[0])[li];
        else if (flags & HF_MODERN_ARRAYS) ireg[I->dest] = 0;
        else return pc; }
      pc++; NEXT;
  L_ArrayLoadFloat:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) freg[I->dest] = ((const double *)(intptr_t)d[1])[li];
        else if (flags & HF_MODERN_ARRAYS) freg[I->dest] = 0.0;
        else return pc; }
      pc++; NEXT;
  L_ArrayStoreInt:   /* bcArrayStoreInt - the VALUE is in Dest, read not written */
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) ((int64_t *)(intptr_t)d[0])[li] = ireg[I->dest];
        else if (!(flags & HF_MODERN_ARRAYS)) return pc; }
      pc++; NEXT;
  L_ArrayStoreFloat:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) ((double *)(intptr_t)d[1])[li] = freg[I->dest];
        else if (!(flags & HF_MODERN_ARRAYS)) return pc; }
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

    /* ---- fused array forms. ----
       ⛔⛔ THIS BLOCK IS THE TWIN THAT WAS LEFT BEHIND ON 21 AUG 2026, and its header used to say
       "MODERN only, for the same reason as the plain accessors" - citing a reason that had been
       CORRECTED four hundred lines above and never re-read here. The plain accessors' note now
       states it exactly: THE FLAG GUARDS THE OUT-OF-BOUNDS CASE, NOT THE ACCESS. HF_MODERN_ARRAYS
       means "MODERN dialect AND bounds checking off", so a leading `if (!flag) return pc` hands the
       PC back on the FIRST element of every CLASSIC program - which is what it did here, for these
       six, for eleven days after the four next door were fixed.

       📊 Measured 2 Sep 2026, job/tests/bench/nbody_v7.bas (CLASSIC), HOTC_DIAG=1: superinstruction
       kind 15 fuses ArrayLoadFloat + AddFloat into bcArrayLoadSubFloat, and that took C-loop entries
       from 2 000 004 to 62 000 124 - 60 000 120 exits on that one opcode, every element - and the
       program from 1151 ms to 1707. ⇒ The FUSION read as the pessimisation; the fusion was fine and
       the arm it fused into was closed. A pass that mints an opcode this loop refuses is the easiest
       way there is to break a covered run, so a fusion and its arm are one change, never two.

       In bounds the two dialects do exactly the same thing through exactly the same descriptor; they
       differ only out of range, where MODERN yields the element default (a load reads 0, a store is
       dropped, NZ does not branch, Z branches - RunTemplate.inc sub-ops 41/42/58/30/48/49) and
       CLASSIC has to RAISE, which this loop cannot do: there it hands the PC back, an error path.
       The in-bounds test is ONE unsigned compare - a negative index wraps to a huge unsigned value
       and fails the same test - as in the plain accessors above. */
  L_ArrayLoadAddFloat:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) freg[I->dest] = freg[I->imm] + ((const double *)(intptr_t)d[1])[li];
        else if (flags & HF_MODERN_ARRAYS) freg[I->dest] = freg[I->imm];
        else return pc; }
      pc++; NEXT;
  L_ArrayLoadSubFloat:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) freg[I->dest] = freg[I->imm] - ((const double *)(intptr_t)d[1])[li];
        else if (flags & HF_MODERN_ARRAYS) freg[I->dest] = freg[I->imm];
        else return pc; }
      pc++; NEXT;
  L_ArrayLoadIntTo:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) ireg[I->dest] = ((const int64_t *)(intptr_t)d[0])[li];
        else if (flags & HF_MODERN_ARRAYS) ireg[I->dest] = 0;
        else return pc; }
      pc++; NEXT;
  L_ArrayStoreIntConst:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2];
        if ((uint64_t)li < (uint64_t)d[2]) ((int64_t *)(intptr_t)d[0])[li] = I->imm;
        else if (!(flags & HF_MODERN_ARRAYS)) return pc; }
      pc++; NEXT;
  L_ArrayLoadIntBranchNZ:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2]; int nz;
        if ((uint64_t)li < (uint64_t)d[2]) nz = ((const int64_t *)(intptr_t)d[0])[li] != 0;
        else if (flags & HF_MODERN_ARRAYS) nz = 0;   /* the element default is 0: no branch */
        else return pc;
        JUMPTO(nz ? (int)I->imm : pc + 1); }
  L_ArrayLoadIntBranchZ:
      { const int64_t *d = arrdesc + 4*(int)I->s1; int64_t li = ireg[I->s2]; int z;
        if ((uint64_t)li < (uint64_t)d[2]) z = ((const int64_t *)(intptr_t)d[0])[li] == 0;
        else if (flags & HF_MODERN_ARRAYS) z = 1;    /* the element default is 0: branch */
        else return pc;
        JUMPTO(z ? (int)I->imm : pc + 1); }

#undef NEXT
#undef HOT_OP_LIST
}
