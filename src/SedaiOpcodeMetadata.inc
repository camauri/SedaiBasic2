{*
 * SedaiOpcodeMetadata.inc - Opcode register usage metadata
 *
 * This file defines which registers each opcode reads/writes.
 * It is the SINGLE SOURCE OF TRUTH for opcode register semantics.
 *
 * IMPORTANT: When adding a new opcode, add its metadata here!
 * The register compaction pass uses this data to correctly remap registers.
 *
 * Format: Each opcode has flags indicating:
 *   - DestType: What type Dest register is (none/int/float/string)
 *   - DestMode: Whether Dest is written (output) or read (input)
 *   - Src1Type: What type Src1 register is
 *   - Src2Type: What type Src2 register is
 *   - ImmediateType: Whether Immediate contains register index(es)
 *
 * Copyright (C) 2025 Maurizio Cammalleri
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

{ ============================================================================
  OPCODE REGISTER METADATA TABLE

  This table is used by SedaiRegisterCompaction to determine which registers
  each opcode reads and writes.

  When adding a NEW OPCODE:
  1. Add the opcode constant to SedaiBytecodeTypes.pas
  2. Add its metadata entry here
  3. Implement the opcode in SedaiBytecodeVM.pas

  The register compaction pass will automatically handle the new opcode.
  ============================================================================ }

{ Register type flags for opcode metadata }
const
  OMD_NONE   = 0;  // No register used
  OMD_INT    = 1;  // Integer register
  OMD_FLOAT  = 2;  // Float register
  OMD_STRING = 3;  // String register

  // Dest mode flags
  OMD_WRITE  = 0;  // Dest is written (output)
  OMD_READ   = 4;  // Dest is read (input, e.g. ArrayStore value)

type
  TOpcodeMetadata = record
    OpCode: Word;
    DestType: Byte;   // OMD_NONE/INT/FLOAT/STRING + OMD_READ flag
    Src1Type: Byte;   // OMD_NONE/INT/FLOAT/STRING
    Src2Type: Byte;   // OMD_NONE/INT/FLOAT/STRING
    ImmType: Byte;    // Special: packed register types in Immediate
  end;

{ ============================================================================
  CHECKLIST FOR NEW OPCODES

  When you add a new opcode, verify:
  [ ] Added to SedaiBytecodeTypes.pas with correct group
  [ ] Added metadata entry in GetOpcodeMetadata below
  [ ] Implemented handler in SedaiBytecodeVM.pas (ExecuteXxxOp or RunTemplate)
  [ ] If opcode uses Immediate for register index, add to ImmediateIsFloatReg/special handling
  [ ] Test with register compaction enabled
  ============================================================================ }

