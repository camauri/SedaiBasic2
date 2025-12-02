# SedaiBasic2 - Compilation Pipeline Architecture

## Overview

SedaiBasic2 is a BASIC language compiler/interpreter implementing a complete compilation pipeline with:
- Finite State Machine (FSM) based Lexer
- Packrat Parser with memoization
- Static Single Assignment (SSA) intermediate representation
- Multi-phase optimization pipeline
- Bytecode compiler
- Register-based Virtual Machine with superinstructions

---

## 1. LEXER - Lexical Analysis

### Source Files
- `sedailexerfsm.pas` - Main FSM lexer implementation
- `SedaiLexerTypes.pas` - Token type definitions
- `sedailexertoken.pas` - Token class

### Architecture

The lexer uses a unified **FastPath+FSM tokenization engine** with a zero-allocation token pool.

#### Key Features
- **Unified FastPath+FSM tokenization engine** supporting both BASIC and Structured Text syntax
- **Configurable spacing modes** for different language requirements
- **Language-agnostic keyword recognition system**
- **Optimized** for both legacy educational languages and modern industrial automation

#### Token Types (TTokenType)

| Category | Examples |
|----------|----------|
| Keywords | `ttKeyword`, `ttConditionalIf`, `ttLoopBlockStart` |
| Literals | `ttNumber`, `ttStringLiteral`, `ttLineNumber`, `ttIdentifier` |
| Operators | `ttOpAdd`, `ttOpMul`, `ttOpEq`, `ttOpLt` |
| Delimiters | `ttDelimParOpen`, `ttDelimBrackOpen`, `ttDelimBraceOpen` |
| Control Flow | `ttJumpGoto`, `ttConditionalElse`, `ttLoopControl` |
| I/O | `ttInputCommand`, `ttOutputCommand`, `ttPrintString` |
| Comments | `ttCommentRemark`, `ttCommentStart`, `ttCommentEnd` |

#### Main Data Structures

```pascal
TLexerToken = class
  TokenType: TTokenType;
  Value: string;              // Lazy extracted from source buffer
  Line, Column: Integer;
  Position, Length: Integer;
  KeywordInfo: TKeywordInfo;  // Optional reference
  ValueExtractor: TFunc;      // Callback for lazy extraction
end;

TTokenRec = packed record    // Lightweight record without allocation
  StartPos, Length: Word;    // 16-bit
  Line, Column: Word;        // 16-bit
  TokenType: Byte;
end;
```

#### FSM States
- `lsInitial` → `lsInIdentifier`, `lsInNumber`, `lsInString`, `lsInComment`
- Character classification: Letters, Digits, Operators, Delimiters, Whitespace

#### Lexer Optimizations
- Zero-allocation token pool (records, not objects)
- Lazy string extraction from source buffer
- Character class cache
- Pre-allocation based on source size estimation
- Zero-copy string handling where possible

---

## 2. PARSER - Syntactic Analysis

### Source Files
- `sedaipackratparser.pas` - Main parser
- `SedaiPackratCore.pas` - Packrat parser core
- `SedaiParserTypes.pas` - Parser types
- `sedaiexpressionparser.pas` - Expression parser
- `sedaiast.pas` - AST definition

### Architecture

The parser implements a **Packrat Parser** with adaptive memoization.

#### Key Features
- **Packrat memoization** for complex recursive constructs
- **Pratt parsing** for expression handling with correct operator precedence
- **Recursive descent** for statement-level parsing
- **Modular OOP architecture** with specialized parser classes for each language
- **Shared AST infrastructure** with language-specific node types
- **Complete Abstract Syntax Trees (AST)** generation for further analysis

#### Memoization Configuration
- Mode: `mmAdaptive`
- Caching threshold: 3 recursion levels
- Supports recursive descent with backtracking

#### AST Node Types (TASTNodeType)

| Category | Types |
|----------|-------|
| Literals | `antLiteral`, `antIdentifier`, `antLineNumber` |
| Expressions | `antBinaryOp`, `antUnaryOp`, `antFunctionCall`, `antParentheses` |
| Statements | `antAssignment`, `antPrint`, `antInput`, `antGoto`, `antGosub` |
| Control Flow | `antIf`, `antForLoop`, `antWhileLoop`, `antDoLoop` |
| Declarations | `antDim`, `antDef`, `antData`, `antConst` |
| Arrays | `antArrayAccess`, `antArrayDecl` |
| Special | `antBlock`, `antProgram`, `antExpressionList` |

#### Operator Precedence (TPrecedence)

| Level | Operators | Precedence |
|-------|-----------|------------|
| 1 | `=` (assignment) | Lowest |
| 2 | `OR` | |
| 3 | `AND` | |
| 4 | `=`, `<>` (equality) | |
| 5 | `<`, `>`, `<=`, `>=` | |
| 6 | `+`, `-` | |
| 7 | `*`, `/` | |
| 8 | `-`, `NOT` (unary) | |
| 9 | `^` (power) | |
| 10 | Function calls, array access | Highest |

#### AST Node Structure

```pascal
TASTNode = class
  NodeType: TASTNodeType;
  Token: TLexerToken;           // Token reference
  Children: TFPObjectList;       // Tree structure
  Value: Variant;               // For literals
  SourceLine, SourceColumn: Integer;
  Attributes: TMetadata;
end;
```

#### Parsing Methods
- `ParseProgram()` → Statement list
- `ParseStatement()` → Handles all BASIC statements
- `ParseExpression()` → Pratt parser with precedence climbing
- `ParseIfStatement()`, `ParseForStatement()`, `ParsePrintStatement()`, etc.

#### Key Optimizations
- Selective memoization (applied only where beneficial)
- Efficient AST node management
- Zero-copy string handling where possible

---

## 3. SSA - Intermediate Representation

### Source Files
- `SedaiSSA.pas` - Main SSA implementation
- `SedaiSSATypes.pas` - SSA types
- `SedaiSSAConstruction.pas` - SSA construction
- `SedaiPhiElimination.pas` - PHI elimination

### Architecture

The **Static Single Assignment (SSA)** form guarantees that each variable is assigned exactly once.

#### Register Types (TSSARegisterType)
- `srtInt` - 64-bit integers
- `srtFloat` - 64-bit doubles
- `srtString` - String values

#### Value Representation (TSSAValue)

```pascal
TSSAValue = record
  Kind: TSSAValueKind;
    // svkNone, svkRegister, svkConstInt, svkConstFloat
    // svkConstString, svkVariable, svkLabel, svkArrayRef
  RegType: TSSARegisterType;
  RegIndex: Integer;    // 0-65535
  Version: Integer;     // SSA versioning for register renaming
end;
```

#### SSA Instruction Set (TSSAOpCode ~80 opcodes)

| Category | Instructions |
|----------|--------------|
| Load/Copy | `ssaLoadConstInt`, `ssaCopyInt`, `ssaCopyFloat` |
| Arithmetic | `ssaAddInt`, `ssaSubInt`, `ssaMulInt`, `ssaDivInt`, `ssaModInt` |
| Conversion | `ssaIntToFloat`, `ssaFloatToInt`, `ssaIntToString` |
| Comparison | `ssaCmpEqInt`, `ssaCmpLtInt`, `ssaCmpEqString` (18 operations) |
| Logic | `ssaLogicalAnd`, `ssaLogicalOr`, `ssaLogicalNot` |
| Strings | `ssaStrConcat`, `ssaStrLen`, `ssaStrLeft`, `ssaStrRight`, `ssaStrMid` |
| Math | `ssaMathSin`, `ssaMathCos`, `ssaMathAbs`, `ssaMathSqr`, `ssaMathRnd` |
| Control Flow | `ssaJump`, `ssaJumpIfZero`, `ssaJumpIfNotZero`, `ssaCall`, `ssaReturn` |
| Arrays | `ssaArrayLoad`, `ssaArrayStore`, `ssaArrayDim` |
| I/O | `ssaPrint`, `ssaPrintString`, `ssaInput`, `ssaInputInt` |
| Special | `ssaLabel`, `ssaEnd`, `ssaPhi` |

#### Control Flow Graph (CFG)

```pascal
TSSAProgram = class
  Blocks: TSSABasicBlockList;    // CFG nodes
  Variables: TVariableTracker;    // Register allocation tracking
  Labels: TLabelMap;             // Jump targets
  Arrays: TArrayDeclarations;    // Array declarations with dimensions
end;

TSSABasicBlock = class
  Instructions: TSSAInstructionList;
  LabelName: string;              // Unique identifier
  Predecessors: TBlockList;       // Incoming CFG edges
  Successors: TBlockList;         // Outgoing CFG edges
end;

TSSAInstruction = class
  OpCode: TSSAOpCode;
  Dest, Src1, Src2, Src3: TSSAValue;  // Operands
  PhiSources: TPhiSourceList;         // For PHI functions
  SourceLine: Integer;                // Original BASIC line
end;
```

#### Register Allocation
- Initial: 256 registers per type (MIN_REGISTER_SLOTS)
- Maximum: 65536 registers per type (MAX_REGISTER_SLOTS)
- Dynamic growth as needed

---

## 4. OPTIMIZATION PIPELINE

Optimizations are organized in progressive tiers on SSA code.

### Tier 1: Base Optimizations

#### 1. Constant Propagation (`SedaiConstProp.pas`)
- Identifies variables assigned a single constant value
- Replaces loads with constants
- Enables constant folding

#### 2. Copy Propagation (`SedaiCopyProp.pas`)
- Eliminates redundant copy operations
- Propagates values through assignments

#### 3. Dead Block Elimination (`SedaiDBE.pas`)
- Removes unreachable basic blocks
- Simplifies control flow

### Tier 2: Global Optimizations

#### 4. Dominator Tree Construction (`SedaiDominators.pas`)
- Cooper-Harvey-Kennedy algorithm O(E*V)
- Preorder/postorder numbering for O(1) dominance queries
- Scalable: tested on CFGs with 10,000+ blocks

#### 5. Global Value Numbering (`SedaiGVN.pas`)
- Scoped hash table stack for value numbering
- Identifies equivalent expressions
- Eliminates redundant computations
- Dominance-aware: reuses only values that dominate

#### 6. Common Subexpression Elimination (`SedaiCSE.pas`)
- Similar to GVN, focused on redundant expressions
- Tracks expression results across blocks

### Tier 3: Advanced Optimizations

#### 7. Algebraic Simplification (`SedaiAlgebraic.pas`)
- `x + 0 → x`, `x * 1 → x`, etc.
- Strength reduction patterns

#### 8. Strength Reduction (`SedaiStrengthReduction.pas`)
- `x * 2 → x << 1` (multiplication to shift)
- `x / 2 → x >> 1` (division to shift)
- Expensive operations → cheap operations

#### 9. Loop-Invariant Code Motion (`SedaiLICM.pas`)
- Natural loop detection (back-edge based)
- Identifies invariant computations
- Pre-header block creation and hoisting
- Conservative: hoists only when safe

#### 10. Dead Code Elimination (`SedaiDCE.pas`)
- SSA-aware with versioned liveness tracking
- Worklist algorithm: O(N) time
- Preserves: control flow, memory operations, I/O, function calls

#### 11. Aggressive Constant Propagation (`SedaiConstPropAggressive.pas`)
- Multi-level constant propagation
- Configurable aggressiveness

#### 12. Copy Coalescing (`SedaiCopyCoalescing.pas`)
- Merges copy operations
- Reduces register pressure

### Bytecode-Level Optimizations

#### 13. Peephole Optimization (`SedaiPeephole.pas`)
Performed after bytecode compilation:
- Self-copy elimination (`CopyInt R0, R0 → NOP`)
- Jump chain optimization
- Dead jump elimination
- Conditional jump simplification

#### 14. Superinstruction Fusion (`SedaiSuperinstructions.pas`)
Fuses common 2-3 instruction patterns:

| Pattern | Superinstruction | Opcode |
|---------|------------------|--------|
| Compare + JumpIfZero | `BranchEqInt` | 100-105 |
| Arithmetic + Copy | `AddIntTo` | 120-133 |
| Constant arithmetic | `AddIntConst` | 140-153 |
| Compare-zero-branch | various | 160-171 |

---

## 5. BYTECODE COMPILER

### Source Files
- `SedaiBytecodeCompiler.pas` - Main compiler
- `SedaiBytecodeTypes.pas` - Bytecode types

### Bytecode Instruction Format

```pascal
TBytecodeInstruction = packed record  // 32 bytes
  OpCode: Byte;           // TBytecodeOp
  Dest: Word;             // Register index 0-65535
  Src1: Word;             // Register index 0-65535
  Src2: Word;             // Register index 0-65535
  Immediate: Int64;       // Constants, jump offsets
  SourceLine: Integer;    // Original BASIC line (debug)
end;
```

### Bytecode Opcodes (TBytecodeOp)

| Category | Opcodes |
|----------|---------|
| Load | `bcLoadConstInt`, `bcLoadConstFloat`, `bcLoadConstString` |
| Copy | `bcCopyInt`, `bcCopyFloat`, `bcCopyString` |
| Int Arithmetic | `bcAddInt`, `bcSubInt`, `bcMulInt`, `bcDivInt`, `bcModInt` |
| Float Arithmetic | `bcAddFloat`, `bcSubFloat`, `bcMulFloat`, `bcDivFloat`, `bcPowFloat` |
| Comparison | 18 opcodes for Int, Float, String |
| Control Flow | `bcJump`, `bcJumpIfZero`, `bcJumpIfNotZero`, `bcCall`, `bcReturn` |
| Arrays | `bcArrayDim`, `bcArrayLoad`, `bcArrayStore` |
| I/O | `bcPrint`, `bcPrintLn`, `bcInput`, `bcInputInt` |
| Math | `bcMathSin`, `bcMathCos`, `bcMathAbs`, `bcMathSqr`, `bcMathRnd` |

### Compilation Process
1. SSA versioning mapping → bytecode registers
2. Direct SSA opcode → bytecode mapping
3. Label resolution (forward reference fixup)
4. Jump fixup table for multi-pass resolution

### Register Mapping
- SSA versioning: `INT[0]_1`, `INT[0]_2`, etc. map to unique bytecode registers
- Separate register arrays: Int, Float, String (each 0-65535)
- VM has 3 separate register banks

---

## 6. VIRTUAL MACHINE

### Source Files
- `SedaiBytecodeVM.pas` - Main VM
- `SedaiBytecodeDisassembler.pas` - Disassembler

### Architecture

The VM is **purely register-based** (no stack for operations).

#### Register Model

```pascal
// 3 Register Arrays (each up to 65536 slots)
FIntRegs: array of Int64;
FFloatRegs: array of Double;
FStringRegs: array of string;

// Temporary Registers (shadow arrays for arithmetic)
FTempIntRegs, FTempFloatRegs, FTempStringRegs: array;

// Call Stack
FCallStack: array[0..255] of Integer;
FCallStackPtr: Integer;
```

#### Main State
- `FPC` (Program Counter): current instruction index
- `FRunning`: execution flag
- `FProgram`: loaded bytecode program
- `FOutputDevice`, `FInputDevice`: I/O interfaces

#### Execution Loop

```pascal
procedure Run();
begin
  while FRunning do begin
    Instr := FProgram.Instructions[FPC];
    ExecuteInstruction(Instr);
    // Superinstruction handling (opcode >= 100)
  end;
end;

procedure ExecuteInstruction(Instr: TBytecodeInstruction);
begin
  // Decode: OpCode determines operation
  // Operands: Dest (result), Src1, Src2 (inputs)
  // Immediate: constants or jump targets
  // Execute: perform operation, update destination
end;
```

#### Instruction Categories

**1. Load** - Populate registers
```pascal
bcLoadConstInt   → FIntRegs[Dest] := Immediate;
bcLoadConstFloat → FFloatRegs[Dest] := DoubleBits(Immediate);
bcLoadConstString → FStringRegs[Dest] := StringConstants[Immediate];
```

**2. Arithmetic** - Compute results
```pascal
bcAddInt → FIntRegs[Dest] := FIntRegs[Src1] + FIntRegs[Src2];
bcDivFloat → FFloatRegs[Dest] := FFloatRegs[Src1] / FFloatRegs[Src2];
```

**3. Comparison** - Set condition codes
```pascal
bcCmpLtInt → FIntRegs[Dest] := Ord(FIntRegs[Src1] < FIntRegs[Src2]);
```

**4. Control Flow** - Modify PC
```pascal
bcJump       → FPC := Immediate;
bcJumpIfZero → if FIntRegs[Src1] = 0 then FPC := Immediate;
bcCall       → FCallStack[FCallStackPtr] := FPC; Inc(FCallStackPtr); FPC := Immediate;
bcReturn     → Dec(FCallStackPtr); FPC := FCallStack[FCallStackPtr];
```

**5. Array Operations**
```pascal
bcArrayDim   → Allocate array
bcArrayLoad  → Load element from array
bcArrayStore → Store element in array
```

**6. Superinstructions** (opcode 100+)
```pascal
bcBranchEqInt (100) → if Src1 = Src2 then goto Immediate;
bcAddIntTo (120)    → Dest := Dest + Src1;
```

#### Array Storage

```pascal
TArrayStorage = record
  ElementType: Byte;           // 0=Int, 1=Float, 2=String
  DimCount: Integer;           // Number of dimensions
  Dimensions: array of Integer;
  TotalSize: Integer;          // Product of all dimensions
  IntData: array of Int64;
  FloatData: array of Double;
  StringData: array of string;
end;
```

#### Error Handling
- NaN detection (optional, controlled by DEBUG_FLOAT_CHECKS)
- Infinity detection
- Register bounds checking
- Jump target validation

#### Performance Features
- Instruction counting (ENABLE_INSTRUCTION_COUNTING)
- Optional float validity checks
- Register dump on error (optional)
- Superinstruction execution reduces dispatch overhead

---

## 7. REGISTER ALLOCATION

### Source Files
- `SedaiRegAllocator.pas`

### Strategy

Allocation uses **linear scan** with liveness analysis.

#### Register Limits
- R0 (Int accumulator) - RESERVED for VM
- R1-R15 available for variables
- F0 (Float accumulator, mapped as R16) - RESERVED
- F1-F15 available for variables
- S0-S15 available for strings

#### Live Interval

```pascal
TLiveInterval = record
  VarName: string;
  VarType: TRegisterType;     // Int, Float, String
  StartPos, EndPos: Integer;   // Bytecode positions
  RefCount: Integer;           // Usage frequency
  RegIndex: Integer;           // Allocated register (-1 if spilled)
  Priority: Integer;           // 1=loop counter, 2=hot, 3=normal
  IsLoopCounter: Boolean;      // I/J/K variables detected
end;
```

#### Allocation Priority
1. Loop counters (I, J, K, I%, J%, K%)
2. Hot variables (RefCount >= 3)
3. Normal variables

#### Spilling Strategy
- When registers are exhausted, variables moved to FStack[]
- Spill count tracking
- Conservative: prioritizes frequently used variables

---

## 8. PIPELINE DIAGRAM

```
    BASIC Source Code (text)
                ↓
    ┌───────────────────┐
    │      LEXER        │ (SedaiLexerFSM)
    │      (FSM)        │ → TTokenList
    └───────────────────┘
                ↓
    ┌───────────────────┐
    │      PARSER       │ (SedaiPackratParser)
    │    (Packrat)      │ → AST
    └───────────────────┘
                ↓
    ┌───────────────────┐
    │  SSA GENERATOR    │ (SedaiSSA)
    │                   │ → SSA Program (CFG + Instructions)
    └───────────────────┘
                ↓
    ┌─────────────────────────────────────────┐
    │   OPTIMIZATION PIPELINE (SSA Passes)    │
    │                                         │
    │  Tier 1:                                │
    │   1. Constant Propagation               │
    │   2. Copy Propagation                   │
    │   3. Dead Block Elimination             │
    │                                         │
    │  Tier 2:                                │
    │   4. Dominator Tree Construction        │
    │   5. Global Value Numbering             │
    │   6. Common Subexpression Elimination   │
    │                                         │
    │  Tier 3:                                │
    │   7. Algebraic Simplification           │
    │   8. Strength Reduction                 │
    │   9. Loop-Invariant Code Motion         │
    │   10. Dead Code Elimination             │
    │   11. Copy Coalescing                   │
    │                                         │
    │  Output: Optimized SSA Program          │
    └─────────────────────────────────────────┘
                ↓
    ┌───────────────────┐
    │    BYTECODE       │ (SedaiBytecodeCompiler)
    │    COMPILER       │ → TBytecodeProgram
    └───────────────────┘
                ↓
    ┌───────────────────────────┐
    │  BYTECODE OPTIMIZATION    │
    │                           │
    │  1. Peephole Optimization │
    │  2. Superinstruction      │
    │     Fusion                │
    └───────────────────────────┘
                ↓
    ┌───────────────────┐
    │        VM         │ (SedaiBytecodeVM)
    │    EXECUTION      │ → Output
    └───────────────────┘
                ↓
            Output (PRINT)
```

---

## 9. DATA STRUCTURES SUMMARY

| Component | Structure | Purpose |
|-----------|-----------|---------|
| Lexer | `TLexerFSM` + `TTokenRec` pool | Zero-allocation tokenization |
| Parser | `TPackratParser` + `TASTNode` tree | Memoized Packrat parsing |
| AST | `TASTNode` + 30+ node types | Hierarchical syntax tree |
| SSA | `TSSAProgram` + CFG blocks | Control flow + versioned instructions |
| Bytecode | `TBytecodeInstruction` + `TBytecodeProgram` | 32-byte compiled instructions |
| VM | `TBytecodeVM` + 3 register arrays | Register-based execution |
| Optimization | `TSSAProgram.Run*` methods | Multi-pass IR optimization |

---

## 10. COMPUTATIONAL COMPLEXITY

| Phase | Complexity |
|-------|------------|
| Lexer | O(n) single pass, zero allocation overhead |
| Parser | O(n) with memoization for complex expressions |
| SSA Generation | O(n) AST traversal |
| GVN/CSE | O(n) with dominator tree overhead O(E*V) |
| DCE | O(n) worklist algorithm |
| LICM | O(n) loop detection + hoisting |
| Bytecode Compilation | O(n) direct mapping |
| VM Execution | O(i) where i = bytecode instructions |
| Superinstruction Dispatch | Reduced vs baseline (fused patterns) |

---

## 11. DEBUG FLAGS

Defined in `DebugFlags.inc`:

```pascal
{$DEFINE ENABLE_INSTRUCTION_COUNTING}  // Count executed instructions
{$DEFINE DEBUG_FLOAT_CHECKS}           // NaN/Infinity checks
{$DEFINE DEBUG_REGISTER_DUMP}          // Register dump on error
```

---

## 12. INTEGRATION POINTS

1. **Lexer ↔ Parser**: Token stream
2. **Parser ↔ SSA**: AST → SSA IR conversion
3. **SSA ↔ Optimizer**: In-place CFG/instruction modification
4. **SSA ↔ Bytecode Compiler**: SSA instruction → bytecode opcode mapping
5. **Bytecode ↔ VM**: Loaded program → execution
6. **VM ↔ Output**: I/O device interface
