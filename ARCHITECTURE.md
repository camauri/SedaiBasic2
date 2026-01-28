# SedaiBasic2 - Compilation Pipeline Architecture

## Overview

SedaiBasic2 is a BASIC language compiler/interpreter implementing a complete compilation pipeline with:
- Finite State Machine (FSM) based Lexer with FastPath optimizations
- Packrat Parser with Pratt expression parsing
- Semi-Pruned Static Single Assignment (SSA) intermediate representation
- 22-pass optimization pipeline (SSA + Bytecode levels)
- Bytecode compiler with grouped opcode encoding
- Register-based Virtual Machine with superinstructions

### High-Level Pipeline Diagram

```
BASIC Source Code
       │
       ▼
┌─────────────────────┐
│      LEXER          │  FSM + FastPath optimizations
│  (TLexerFSM)        │  Zero-allocation token pool
└─────────┬───────────┘
          │ Tokens
          ▼
┌─────────────────────┐
│      PARSER         │  Packrat parser (statements)
│ (TPackratParser)    │  + Pratt parser (expressions)
└─────────┬───────────┘
          │ AST (TASTNode)
          ▼
┌─────────────────────┐
│  SSA GENERATOR      │  Converts AST → Semi-Pruned SSA IR
│  (TSSAGenerator)    │  with PHI functions and versioning
└─────────┬───────────┘
          │ SSA IR (TSSAProgram)
          ▼
┌─────────────────────┐
│  SSA OPTIMIZATION   │  16 optimization passes
│  PIPELINE           │  (configurable via OptimizationFlags.inc)
└─────────┬───────────┘
          │ Optimized SSA IR
          ▼
┌─────────────────────┐
│ BYTECODE COMPILER   │  SSA → Bytecode (2-byte grouped opcodes)
│(TBytecodeCompiler)  │  Label resolution, register mapping
└─────────┬───────────┘
          │ TBytecodeProgram
          ▼
┌─────────────────────┐
│ BYTECODE OPTIMIZATION│ 6 post-compilation passes
│  (Peephole, Super-  │  Superinstructions, NOP compaction,
│   instructions)     │  Register compaction
└─────────┬───────────┘
          │ Optimized Bytecode
          ▼
┌─────────────────────┐
│   BYTECODE VM       │  Register-based VM (3 typed banks)
│  (TBytecodeVM)      │  Superinstruction dispatch
└─────────────────────┘
```

---

## 1. LEXER - Lexical Analysis

### Source Files
- `sedailexerfsm.pas` - Main FSM lexer implementation
- `SedaiLexerTypes.pas` - Token type definitions
- `sedailexertoken.pas` - Token class
- `sedaikeywordregistry.pas` - Keyword recognition via Trie
- `sedaikeywordtrie.pas` - Trie data structure for keywords

### Architecture

The lexer uses a unified **FastPath+FSM tokenization engine** with a zero-allocation token pool.

#### Key Features
- **Unified FastPath+FSM tokenization engine** supporting both BASIC and Structured Text syntax
- **FastPath optimization**: Common token patterns are recognized directly without FSM transitions
- **Configurable spacing modes** for different language requirements
- **Language-agnostic keyword recognition system** using Trie structure
- **Optimized** for both legacy educational languages and modern industrial automation

#### FastPath Optimization
The lexer implements **FastPath** shortcuts that bypass the FSM for common token patterns:

```pascal
function TLexerFSM.ScanAllTokensFast: TTokenList;
// Uses NextToken which internally:
// 1. Checks FastPath first for common patterns (identifiers, numbers, operators)
// 2. Falls back to FSM only for complex cases (strings, comments, special)
// Result: ~80-90% of tokens processed via FastPath (faster than full FSM)
```

FastPath handles:
- Single-character operators (`+`, `-`, `*`, `/`, `=`, `<`, `>`)
- Parentheses, brackets, delimiters
- Simple identifiers (A-Z followed by alphanumerics)
- Integer numbers
- Line numbers at start of line

The FSM is used only for:
- Floating-point numbers with decimals/exponents
- String literals (quote handling)
- Comments (REM, multi-line)
- Complex operators (`<=`, `>=`, `<>`)

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
- **Zero-allocation token pool** (`TTokenRec` records, not objects)
- **Lazy string extraction** from source buffer (only when needed)
- **Character class cache** (`FCharClassCache: array[Char] of TCharClass`)
- **Pre-allocation** based on source size estimation (`FTokensPerCharRatio: 0.15`)
- **Zero-copy string handling** where possible
- **FastPath bypass** for common token patterns (~80-90% coverage)
- **Token buffer reuse** (`FTokenBuffer: array[0..255] of Char`)

#### Pool Configuration
```pascal
FTokensPerCharRatio := 0.15;  // 15% of source chars become tokens
FSafetyFactor := 2.0;         // 2x safety margin for pre-allocation
FMinPoolSize := 100;          // Minimum 100 tokens
FMaxPoolSize := CalculateMaxTokensFromMemory(...);  // Dynamic max
```

---

## 2. PARSER - Syntactic Analysis

### Source Files
- `sedaipackratparser.pas` - Main parser
- `SedaiPackratCore.pas` - Packrat parser core
- `SedaiParserTypes.pas` - Parser types
- `sedaiexpressionparser.pas` - Expression parser
- `sedaiast.pas` - AST definition

### Architecture

The parser uses a **hybrid architecture**:
- **TPackratParser** (recursive descent with memoization) for statements
- **TExpressionParser** (Pratt parser) for expressions

#### Key Features
- **Packrat memoization** for complex recursive constructs (statements)
- **Pratt parsing** for expression handling with correct operator precedence
- **Recursive descent** for statement-level parsing
- **Modular OOP architecture** with specialized parser classes
- **Shared AST infrastructure** with language-specific node types
- **Complete Abstract Syntax Trees (AST)** generation for further analysis

#### Memoization Configuration
- Mode: `mmAdaptive`
- Caching threshold: 3 recursion levels
- Supports recursive descent with backtracking

### Expression Parser (Pratt Algorithm)

The **TExpressionParser** in `sedaiexpressionparser.pas` implements a **Pratt parser** (Top-Down Operator Precedence):

```pascal
TExpressionParser = class(TPackratCore)
  // Pratt rules initialized in InitializePrattRules
  function ParseExpression(Precedence: TPrecedence = precNone): TASTNode;
  function GetCurrentPrecedence: TPrecedence; inline;

  // PREFIX parsing functions (called for token at start of expression)
  function ParseNumber(Token: TLexerToken): TASTNode;
  function ParseString(Token: TLexerToken): TASTNode;
  function ParseIdentifier(Token: TLexerToken): TASTNode;
  function ParseUnaryMinus(Token: TLexerToken): TASTNode;
  function ParseParentheses(Token: TLexerToken): TASTNode;
  function ParseFunctionCall(Token: TLexerToken): TASTNode;

  // INFIX parsing functions (called for token in middle of expression)
  function ParseBinaryOperator(Left: TASTNode; Token: TLexerToken): TASTNode;
  function ParsePower(Left: TASTNode; Token: TLexerToken): TASTNode;  // Right-associative
  function ParseArrayAccess(Left: TASTNode; Token: TLexerToken): TASTNode;
end;
```

#### Pratt Parser Algorithm
```pascal
function TExpressionParser.ParseExpression(Precedence: TPrecedence): TASTNode;
begin
  // 1. Get prefix parselet for current token (number, identifier, unary op, etc.)
  Token := Consume;
  Left := CallPrefixParselet(Token);

  // 2. While next token has higher precedence, apply infix parselet
  while Precedence < GetCurrentPrecedence do begin
    Token := Consume;
    Left := CallInfixParselet(Left, Token);  // Binary op, array access, etc.
  end;

  Result := Left;
end;
```

#### Benefits of Pratt Parsing
- **Correct precedence handling** without explicit grammar rules
- **Right-associative operators** (like `^` power) handled naturally
- **Extensible**: new operators added by registering parselets
- **Efficient**: single-pass, no backtracking for expressions

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
- `SedaiSSA.pas` - Main SSA generator (AST → SSA)
- `SedaiSSATypes.pas` - SSA types and data structures
- `SedaiSSAConstruction.pas` - Semi-Pruned SSA construction
- `SedaiPhiElimination.pas` - PHI elimination (SSA → non-SSA)
- `SedaiDominators.pas` - Dominator tree construction

### Architecture

SedaiBasic uses **Semi-Pruned SSA**, a practical compromise between:
- **Minimal SSA** (fewer PHI functions, more complex algorithms)
- **Pruned SSA** (only PHI where variables are live)
- **Full SSA** (PHI at every join point)

#### Semi-Pruned SSA Characteristics
- PHI functions inserted at **dominance frontiers** of definition points
- **Pruning**: PHI functions removed where variable is not live
- **Global variable semantics mode**: For BASIC compatibility (Version=0 means unversioned)
- **SSA versioning**: Each assignment creates new version (R0_1, R0_2, etc.)

```pascal
TSSAProgram = class
  FGlobalVariableSemantics: Boolean;  // True = BASIC mode (no versioning)
  // Methods for SSA construction and optimization
  procedure RunSSAConstruction;       // Convert to proper SSA with PHI
  procedure RunPhiElimination;        // Convert PHI to copies (final pass)
end;
```

### Why Semi-Pruned SSA?

| SSA Type | PHI Count | Algorithm Complexity | Memory | Used By |
|----------|-----------|---------------------|--------|---------|
| Minimal | Lowest | O(n²) | Low | Research |
| **Semi-Pruned** | Medium | **O(n log n)** | **Medium** | **SedaiBasic** |
| Pruned | Low | O(n × live) | Medium | GCC, LLVM |
| Full | Highest | O(n) | High | Simple compilers |

**Semi-Pruned was chosen because:**
1. **Good balance** between optimization power and complexity
2. **Works well with BASIC** (relatively simple control flow)
3. **Efficient** for programs with 1000-10000 instructions
4. **Enables all standard optimizations** (GVN, CSE, DCE, LICM)

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

The optimization pipeline consists of **22 configurable passes** controlled by `OptimizationFlags.inc`.

### Master Control

```pascal
// OptimizationFlags.inc
{.$DEFINE DISABLE_ALL_OPTIMIZATIONS}  // Uncomment to disable ALL

// Individual flags (in execution order):
{$DEFINE DISABLE_CONSTANT_FOLDING}    // 1. Constant folding in SSA generation
{$DEFINE DISABLE_DBE}                 // 2. Dead block elimination
{$DEFINE DISABLE_DOMINATOR_TREE}      // 3. Dominator tree construction
{$DEFINE DISABLE_SSA_CONSTRUCTION}    // 4. Semi-Pruned SSA (PHI + versioning)
{$DEFINE DISABLE_GVN}                 // 5. Global Value Numbering
{$DEFINE DISABLE_CSE}                 // 6. Common Subexpression Elimination
{$DEFINE DISABLE_ALGEBRAIC}           // 7. Algebraic simplification
{$DEFINE DISABLE_STRENGTH_RED}        // 8. Strength reduction
{$DEFINE DISABLE_GOSUB_INLINE}        // 9. GOSUB inlining
{$DEFINE DISABLE_CONST_PROP}          // 10. Aggressive constant propagation
{$DEFINE DISABLE_COPY_PROP}           // 11. Copy propagation
{$DEFINE DISABLE_LICM}                // 12. Loop-invariant code motion
{$DEFINE DISABLE_LOOP_UNROLL}         // 13. Loop unrolling
{$DEFINE DISABLE_DCE}                 // 14. Dead code elimination
{$DEFINE DISABLE_PHI_ELIM}            // 15. PHI elimination
{$DEFINE DISABLE_COPY_COAL}           // 16. Copy coalescing
{$DEFINE DISABLE_REG_ALLOC}           // 17. Register allocation
// Post-compilation (bytecode level):
{$DEFINE DISABLE_PEEPHOLE}            // 18. Peephole optimizations
{$DEFINE DISABLE_SUPERINSTRUCTIONS}   // 19. Superinstruction fusion
{$DEFINE DISABLE_ARRAYSTORECONST}     // 20. ArrayStoreConst fusion
{$DEFINE DISABLE_NOP_COMPACTION}      // 21. NOP removal
{$DEFINE DISABLE_PEEPHOLE_PASS2}      // 22. Second peephole pass
{$DEFINE DISABLE_REG_COMPACTION}      // 23. Register compaction
```

---

### SSA-Level Optimizations (16 passes)

#### Pass 1: Constant Folding (during SSA generation)
- Evaluates constant expressions at compile time
- `3 + 5 → 8`, `"A" + "B" → "AB"`
- Applied during AST → SSA conversion

#### Pass 2: Dead Block Elimination (`SedaiDBE.pas`)
- Removes unreachable basic blocks
- Simplifies control flow graph
- `TSSAProgram.RunDBE: Integer`

#### Pass 3: Dominator Tree Construction (`SedaiDominators.pas`)
- **Cooper-Harvey-Kennedy algorithm** O(E × α(V))
- Preorder/postorder numbering for O(1) dominance queries
- Required for GVN, LICM, SSA construction
- Scalable: tested on CFGs with 10,000+ blocks

#### Pass 4: Semi-Pruned SSA Construction (`SedaiSSAConstruction.pas`)
- Inserts PHI functions at dominance frontiers
- Versions all variable assignments (R0_1, R0_2, ...)
- `TSSAProgram.RunSSAConstruction`

#### Pass 5: Global Value Numbering (`SedaiGVN.pas`)
- **Scoped hash table stack** for value numbering
- Identifies equivalent expressions across blocks
- Dominance-aware: reuses only values that dominate
- `TSSAProgram.RunGVN: Integer`
- **Note**: Mutually exclusive with CSE (use one or the other)

#### Pass 6: Common Subexpression Elimination (`SedaiCSE.pas`)
- Similar to GVN, focused on local redundancy
- Tracks expression results within blocks
- `TSSAProgram.RunCSE: Integer`
- **Note**: Mutually exclusive with GVN

#### Pass 7: Algebraic Simplification (`SedaiAlgebraic.pas`)
- Identity elimination: `x + 0 → x`, `x * 1 → x`, `x - 0 → x`
- Zero propagation: `x * 0 → 0`
- Negation folding: `x - x → 0`
- `TSSAProgram.RunAlgebraic: Integer`

#### Pass 8: Strength Reduction (`SedaiStrengthReduction.pas`)
- `x * 2 → x + x` or `x << 1`
- `x / 2 → x >> 1` (for integers)
- `x * (power of 2) → x << n`
- `TSSAProgram.RunStrengthReduction: Integer`

#### Pass 9: GOSUB Inlining (`SedaiGosubInlining.pas`)
- Inlines small GOSUB subroutines
- Reduces call overhead for frequently-called subs
- `TSSAProgram.RunGosubInlining: Integer`

#### Pass 10: Aggressive Constant Propagation (`SedaiConstPropAggressive.pas`)
- Multi-level constant propagation
- Configurable aggressiveness (Level 1-3)
- Propagates through PHI functions when possible
- `TSSAProgram.RunAggressiveConstProp(Level: Integer): Integer`

#### Pass 11: Copy Propagation (`SedaiCopyProp.pas`)
- Eliminates redundant copy operations
- `X := Y; Z := X → Z := Y`
- `TSSAProgram.RunCopyProp: Integer`

#### Pass 12: Loop-Invariant Code Motion (`SedaiLICM.pas`)
- **Natural loop detection** (back-edge based)
- Identifies invariant computations
- Creates pre-header blocks and hoists code
- Conservative: hoists only when safe (no side effects)
- `TSSAProgram.RunLICM: Integer`

#### Pass 13: Loop Unrolling (`SedaiLoopUnroll.pas`)
- Unrolls small loops with constant bounds
- Reduces loop overhead
- `TSSAProgram.RunLoopUnrolling: Integer`

#### Pass 14: Dead Code Elimination (`SedaiDCE.pas`)
- **SSA-aware** with versioned liveness tracking
- **Worklist algorithm**: O(N) time complexity
- Preserves: control flow, memory operations, I/O, function calls
- `TSSAProgram.RunDCE: Integer`

#### Pass 15: PHI Elimination (`SedaiPhiElimination.pas`)
- Converts PHI functions to copy instructions
- **MUST run after DCE**, before bytecode compilation
- `TSSAProgram.RunPhiElimination`

#### Pass 16: Copy Coalescing (`SedaiCopyCoalescing.pas`)
- Merges redundant copy operations
- Reduces register pressure
- `TSSAProgram.RunCopyCoalescing: Integer`

---

### Bytecode-Level Optimizations (6 passes)

#### Pass 17: Register Allocation (`SedaiRegAllocator.pas`)
- **Linear scan** algorithm with liveness analysis
- Separate allocation for Int, Float, String registers
- Spilling strategy for register exhaustion

#### Pass 18: Peephole Optimization Pass 1 (`SedaiPeephole.pas`)
Performed after bytecode compilation:
- **Self-copy elimination**: `CopyInt R0, R0 → NOP`
- **Jump chain optimization**: `Jump L1; L1: Jump L2 → Jump L2`
- **Dead jump elimination**: `Jump L1; L1: next → NOP`
- **Conditional jump simplification**: `JumpIfZero to next → NOP`

#### Pass 19: Superinstruction Fusion (`SedaiSuperinstructions.pas`)
Fuses common 2-3 instruction patterns into single superinstructions:

| Pattern | Superinstruction | Savings |
|---------|------------------|---------|
| `CmpLeInt + JumpIfZero` | `BranchGtInt` | 1 instr + 1 reg |
| `AddInt + CopyInt` | `AddIntTo` | 1 instr + 1 reg |
| `LoadConstInt + AddInt` | `AddIntConst` | 1 instr + 1 reg |
| `LoadConstInt + CmpEqInt + JumpIfZero` | `BranchNeZeroInt` | 2 instr + 2 reg |
| `AddInt + CmpLeInt + JumpIfZero` | `AddIntToBranchLe` | Loop optimization |
| `MulFloat + AddFloat` | `MulAddFloat` (FMA) | 1 instr |
| `ArrayLoadInt + ArrayStoreInt` (swap) | `ArraySwapInt` | 3+ instr |

**Advanced patterns** (for compute benchmarks):
- `ArrayLoadAddFloat`, `SquareSumFloat`, `MulMulFloat`, `AddSqrtFloat`
- `ArrayReverseRange`, `ArrayShiftLeft`, `ArrayCopyElement`

#### Pass 20: ArrayStoreConst Fusion
- Specific fusion for `LoadConst + ArrayStore → ArrayStoreConst`
- Common in array initialization

#### Pass 21: NOP Compaction (`SedaiNopCompaction.pas`)
- Removes all NOP instructions generated by previous passes
- **Adjusts all jump targets** using index mapping
- Reduces bytecode size

#### Pass 22: Peephole Optimization Pass 2
- Second peephole pass after NOP compaction
- Catches additional opportunities exposed by compaction

#### Pass 23: Register Compaction (`SedaiRegisterCompaction.pas`)
- **Remaps sparse register numbers** to dense contiguous range
- Example: Registers R420, R421, R500 → R0, R1, R2
- **Improves cache locality** by reducing working set size
- Critical for compute-intensive loops

---

## 5. BYTECODE COMPILER

### Source Files
- `SedaiBytecodeCompiler.pas` - Main compiler (SSA → Bytecode)
- `SedaiBytecodeTypes.pas` - Bytecode types and opcodes
- `SedaiBytecodeSerializer.pas` - Bytecode save/load (BSAVE/BLOAD)
- `SedaiBytecodeDisassembler.pas` - Disassembler for debugging

### Bytecode Instruction Format

```pascal
TBytecodeInstruction = packed record  // 16 bytes (optimized from 20)
  OpCode: Word;           // 2 bytes: Group.Opcode encoding (256 groups × 256 ops)
  Dest: Word;             // Destination register index (0-65535)
  Src1: Word;             // Source 1 register index (0-65535)
  Src2: Word;             // Source 2 register index (0-65535)
  Immediate: Int64;       // Immediate value (constants, jump offsets)
end;

// Source line tracking is separate for cache efficiency:
TSourceMapEntry = packed record
  StartPC, EndPC: Integer;    // Instruction range
  SourceLine: Integer;        // BASIC source line number
end;
```

### Opcode Encoding (2-byte Group.Opcode)

The opcode uses a **2-byte grouped encoding** for extensibility:

```pascal
const
  bcGroupCore    = $0000;  // Group 0: Core VM operations
  bcGroupString  = $0100;  // Group 1: String operations
  bcGroupMath    = $0200;  // Group 2: Math functions
  bcGroupArray   = $0300;  // Group 3: Array operations
  bcGroupIO      = $0400;  // Group 4: I/O operations
  bcGroupSpecial = $0500;  // Group 5: Special variables (TI, TI$, etc.)
  bcGroupFileIO  = $0600;  // Group 6: File I/O
  bcGroupSprite  = $0700;  // Group 7: Sprite operations
  bcGroupGraphics= $0A00;  // Group 10: Graphics
  bcGroupSound   = $0B00;  // Group 11: Sound
  bcGroupSuper   = $C800;  // Groups 200-255: Superinstructions
```

### Bytecode Opcodes by Group

#### Group 0: Core VM (~90 opcodes)
| Subgroup | Opcodes |
|----------|---------|
| Constants | `bcLoadConstInt`, `bcLoadConstFloat`, `bcLoadConstString` |
| Copy | `bcCopyInt`, `bcCopyFloat`, `bcCopyString` |
| Variables | `bcLoadVar`, `bcStoreVar` |
| Int Arithmetic | `bcAddInt`, `bcSubInt`, `bcMulInt`, `bcDivInt`, `bcModInt`, `bcNegInt` |
| Float Arithmetic | `bcAddFloat`, `bcSubFloat`, `bcMulFloat`, `bcDivFloat`, `bcPowFloat`, `bcNegFloat` |
| Conversion | `bcIntToFloat`, `bcFloatToInt`, `bcIntToString`, `bcFloatToString` |
| Int Comparison | `bcCmpEqInt`, `bcCmpNeInt`, `bcCmpLtInt`, `bcCmpGtInt`, `bcCmpLeInt`, `bcCmpGeInt` |
| Float Comparison | `bcCmpEqFloat`, `bcCmpNeFloat`, `bcCmpLtFloat`, `bcCmpGtFloat`, ... |
| String Comparison | `bcCmpEqString`, `bcCmpNeString`, `bcCmpLtString`, `bcCmpGtString` |
| Bitwise | `bcBitwiseAnd`, `bcBitwiseOr`, `bcBitwiseXor`, `bcBitwiseNot` |
| Control Flow | `bcJump`, `bcJumpIfZero`, `bcJumpIfNotZero`, `bcCall`, `bcReturn` |
| System | `bcEnd`, `bcStop`, `bcFast`, `bcSlow`, `bcSleep`, `bcNop`, `bcClear` |
| Debug | `bcTron`, `bcTroff` |
| DATA/READ | `bcDataAdd`, `bcDataReadInt`, `bcDataReadFloat`, `bcDataReadString`, `bcDataRestore` |
| Input | `bcGet`, `bcGetkey` |
| Error | `bcTrap`, `bcResume`, `bcResumeNext` |

#### Group 1: String Operations (~12 opcodes)
`bcStrConcat`, `bcStrLen`, `bcStrLeft`, `bcStrRight`, `bcStrMid`, `bcStrAsc`, `bcStrChr`, `bcStrStr`, `bcStrVal`, `bcStrHex`, `bcStrInstr`, `bcStrErr`

#### Group 2: Math Functions (~15 opcodes)
`bcMathSin`, `bcMathCos`, `bcMathTan`, `bcMathAtn`, `bcMathLog`, `bcMathExp`, `bcMathSqr`, `bcMathAbs`, `bcMathSgn`, `bcMathInt`, `bcMathRnd`, `bcMathLog10`, `bcMathLog2`, `bcMathLogN`, `bcStrDec`

#### Group 3: Array Operations (~9 opcodes)
`bcArrayDim`, `bcArrayLoadInt`, `bcArrayLoadFloat`, `bcArrayLoadString`, `bcArrayStoreInt`, `bcArrayStoreFloat`, `bcArrayStoreString`

#### Group 4: I/O Operations (~15 opcodes)
`bcPrint`, `bcPrintLn`, `bcPrintString`, `bcPrintInt`, `bcPrintComma`, `bcPrintSemicolon`, `bcPrintTab`, `bcPrintSpc`, `bcPrintNewLine`, `bcInput`, `bcInputInt`, `bcInputFloat`, `bcInputString`

#### Group 5: Special Variables (~10 opcodes)
`bcLoadTI`, `bcLoadTIS`, `bcStoreTIS`, `bcLoadDTS`, `bcFre`, `bcLoadEL`, `bcLoadER`, `bcLoadERRS`, `bcPeek`, `bcPoke`

#### Groups 6-11: Domain-Specific
- **File I/O**: `bcDopen`, `bcDclose`, `bcGetFile`, `bcInputFile`, `bcPrintFile`, `bcCmd`
- **Sprites**: `bcSprite`, `bcMovspr*`, `bcSprcolor`, `bcSprsav`, `bcCollision`, `bcBump`, `bcRsp*`
- **Graphics**: `bcGraphic*`, `bcSetColor`, `bcGetColor`, `bcPLoad`, `bcPSave`, `bcScnClr`
- **Sound**: `bcSoundVol`, `bcSoundSound`, `bcSoundEnvelope`, `bcSoundTempo`, `bcSoundPlay`, `bcSoundFilter`

#### Groups 200-255: Superinstructions (~50 opcodes)
| Pattern | Opcode |
|---------|--------|
| Compare-and-Branch (Int) | `bcBranchEqInt` ... `bcBranchGeInt` |
| Compare-and-Branch (Float) | `bcBranchEqFloat` ... `bcBranchGeFloat` |
| Arithmetic-to-Dest (Int) | `bcAddIntTo`, `bcSubIntTo`, `bcMulIntTo` |
| Arithmetic-to-Dest (Float) | `bcAddFloatTo`, `bcSubFloatTo`, `bcMulFloatTo`, `bcDivFloatTo` |
| Constant Arithmetic (Int) | `bcAddIntConst`, `bcSubIntConst`, `bcMulIntConst` |
| Constant Arithmetic (Float) | `bcAddFloatConst`, `bcSubFloatConst`, `bcMulFloatConst`, `bcDivFloatConst` |
| Compare-Zero-Branch | `bcBranchEqZeroInt`, `bcBranchNeZeroInt`, `bcBranchEqZeroFloat`, ... |
| Loop Increment+Branch | `bcAddIntToBranchLe`, `bcAddIntToBranchLt`, `bcSubIntToBranchGe`, ... |
| FMA (Float) | `bcMulAddFloat`, `bcMulSubFloat`, `bcMulAddToFloat`, `bcMulSubToFloat` |
| Array+Arithmetic | `bcArrayLoadAddFloat`, `bcArrayLoadSubFloat`, `bcArrayLoadDivAddFloat` |
| Square/Sum | `bcSquareSumFloat`, `bcAddSquareFloat` |
| Array Operations | `bcArraySwapInt`, `bcArrayLoadIntTo`, `bcArrayCopyElement`, `bcArrayMoveElement` |
| Array Bulk | `bcArrayReverseRange`, `bcArrayShiftLeft` |

### Compilation Process

```pascal
TBytecodeCompiler = class
  FLabelMap: TStringList;           // Maps label names → bytecode offsets
  FJumpFixups: array of TJumpFixup; // Forward references to resolve
  FRegisterMap: array[TSSARegisterType] of TStringList;  // SSA reg → BC reg
  FNextBytecodeReg: array[TSSARegisterType] of Integer;

  function Compile(SSAProgram: TSSAProgram): TBytecodeProgram;
  // 1. Build label map (first pass)
  // 2. Compile instructions (second pass)
  // 3. Resolve forward jumps (fixup pass)
end;
```

**Compilation steps:**
1. **SSA → Bytecode mapping**: Direct opcode translation (`ssaAddInt → bcAddInt`)
2. **Register mapping**: SSA versioned registers (R0_1, R0_2) → flat bytecode registers
3. **Label resolution**: Build label → PC mapping, then fixup forward references
4. **Source map**: Separate source line tracking for debugging

### Register Mapping

```pascal
function TBytecodeCompiler.MapSSARegisterToBytecode(
  RegType: TSSARegisterType; RegIndex, Version: Integer): Integer;
// Key = "RegIndex:Version"
// Returns: unique bytecode register index per type
```

- SSA versioning: `INT[0]_1`, `INT[0]_2` → different bytecode registers
- Separate register banks: Int (0-65535), Float (0-65535), String (0-65535)
- Variable registers preserved at start (for debugging)

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

## 8. DETAILED PIPELINE DIAGRAM

```
    BASIC Source Code (text)
                ↓
    ┌─────────────────────────────────────────┐
    │              LEXER                       │ (SedaiLexerFSM)
    │  ┌─────────────────┬──────────────────┐ │
    │  │    FastPath     │       FSM        │ │
    │  │  (80-90% tokens)│  (complex tokens)│ │
    │  └─────────────────┴──────────────────┘ │
    │  Zero-allocation TTokenRec pool         │
    └─────────────────────────────────────────┘
                ↓ TTokenList
    ┌─────────────────────────────────────────┐
    │              PARSER                      │
    │  ┌──────────────────┬─────────────────┐ │
    │  │  Packrat Parser  │   Pratt Parser  │ │
    │  │   (statements)   │  (expressions)  │ │
    │  └──────────────────┴─────────────────┘ │
    │  Memoization for recursive constructs   │
    └─────────────────────────────────────────┘
                ↓ AST (TASTNode tree)
    ┌─────────────────────────────────────────┐
    │          SSA GENERATOR                   │ (SedaiSSA)
    │  ┌────────────────────────────────────┐ │
    │  │   Semi-Pruned SSA Construction     │ │
    │  │   - PHI functions at dom frontiers │ │
    │  │   - Register versioning (R0_1...)  │ │
    │  │   - Constant folding (inline)      │ │
    │  └────────────────────────────────────┘ │
    └─────────────────────────────────────────┘
                ↓ SSA IR (TSSAProgram)
    ┌─────────────────────────────────────────┐
    │   SSA OPTIMIZATION PIPELINE (16 passes) │
    │                                         │
    │  ┌─ Tier 1: Base ─────────────────────┐ │
    │  │  1. Constant Folding               │ │
    │  │  2. Dead Block Elimination (DBE)   │ │
    │  │  3. Dominator Tree Construction    │ │
    │  │  4. SSA Construction (PHI+version) │ │
    │  └────────────────────────────────────┘ │
    │  ┌─ Tier 2: Global ───────────────────┐ │
    │  │  5. Global Value Numbering (GVN)   │ │
    │  │  6. Common Subexpression Elim (CSE)│ │
    │  │  7. Algebraic Simplification       │ │
    │  │  8. Strength Reduction             │ │
    │  └────────────────────────────────────┘ │
    │  ┌─ Tier 3: Advanced ─────────────────┐ │
    │  │  9. GOSUB Inlining                 │ │
    │  │  10. Aggressive Constant Prop      │ │
    │  │  11. Copy Propagation              │ │
    │  │  12. Loop-Invariant Code Motion    │ │
    │  │  13. Loop Unrolling                │ │
    │  │  14. Dead Code Elimination (DCE)   │ │
    │  └────────────────────────────────────┘ │
    │  ┌─ Tier 4: Finalization ─────────────┐ │
    │  │  15. PHI Elimination (SSA→copies)  │ │
    │  │  16. Copy Coalescing               │ │
    │  └────────────────────────────────────┘ │
    └─────────────────────────────────────────┘
                ↓ Optimized SSA IR
    ┌─────────────────────────────────────────┐
    │        BYTECODE COMPILER                 │ (SedaiBytecodeCompiler)
    │  - SSA opcode → Bytecode mapping        │
    │  - Register mapping (versioned→flat)    │
    │  - Label resolution & jump fixup        │
    │  - 2-byte grouped opcode encoding       │
    └─────────────────────────────────────────┘
                ↓ TBytecodeProgram
    ┌─────────────────────────────────────────┐
    │  BYTECODE OPTIMIZATION (6 passes)       │
    │                                         │
    │  17. Register Allocation (linear scan)  │
    │  18. Peephole Pass 1 (copy elim, jumps) │
    │  19. Superinstruction Fusion            │
    │      - Compare+Branch → BranchXX        │
    │      - Arith+Copy → ArithTo             │
    │      - LoadConst+Arith → ArithConst     │
    │      - FMA patterns (MulAddFloat)       │
    │  20. ArrayStoreConst Fusion             │
    │  21. NOP Compaction + Jump Adjust       │
    │  22. Peephole Pass 2 (cleanup)          │
    │  23. Register Compaction (sparse→dense) │
    └─────────────────────────────────────────┘
                ↓ Optimized Bytecode
    ┌─────────────────────────────────────────┐
    │          BYTECODE VM                     │ (SedaiBytecodeVM)
    │  ┌────────────────────────────────────┐ │
    │  │  3 Typed Register Banks:           │ │
    │  │  - FIntRegs: array of Int64        │ │
    │  │  - FFloatRegs: array of Double     │ │
    │  │  - FStringRegs: array of string    │ │
    │  └────────────────────────────────────┘ │
    │  ┌────────────────────────────────────┐ │
    │  │  Grouped Opcode Dispatch:          │ │
    │  │  - Group 0: Core ops               │ │
    │  │  - Groups 1-11: Domain ops         │ │
    │  │  - Groups 200+: Superinstructions  │ │
    │  └────────────────────────────────────┘ │
    └─────────────────────────────────────────┘
                ↓
            Output (PRINT, Graphics, Sound)
```

---

## 9. DATA STRUCTURES SUMMARY

| Component | Structure | Purpose |
|-----------|-----------|---------|
| Lexer | `TLexerFSM` + `TTokenRec` pool | Zero-allocation tokenization |
| Parser | `TPackratParser` + `TExpressionParser` | Packrat (statements) + Pratt (expressions) |
| AST | `TASTNode` + 50+ node types | Hierarchical syntax tree |
| SSA | `TSSAProgram` + `TSSABasicBlock` + PHI | Semi-Pruned SSA with versioning |
| Bytecode | `TBytecodeInstruction` (16 bytes) | 2-byte grouped opcodes |
| VM | `TBytecodeVM` + 3 register banks | Register-based execution |
| Optimization | 23 configurable passes | SSA + Bytecode level |

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

### DebugFlags.inc

```pascal
{$DEFINE ENABLE_INSTRUCTION_COUNTING}  // Count executed instructions
{$DEFINE DEBUG_FLOAT_CHECKS}           // NaN/Infinity checks
{$DEFINE DEBUG_REGISTER_DUMP}          // Register dump on error
{$DEFINE DEBUG_SSA}                    // SSA generation tracing
{$DEFINE DEBUG_BYTECODE}               // Bytecode compilation tracing
{$DEFINE DEBUG_GVN}                    // GVN optimization tracing
{$DEFINE DEBUG_DCE}                    // DCE optimization tracing
{$DEFINE DEBUG_LICM}                   // LICM optimization tracing
{$DEFINE DEBUG_SUPERINSTR}             // Superinstruction fusion tracing
```

### OptimizationFlags.inc

```pascal
{.$DEFINE DISABLE_ALL_OPTIMIZATIONS}   // Master switch
// Individual optimization disable flags (see Section 4)
```

---

## 12. INTEGRATION POINTS

1. **Lexer ↔ Parser**: `TTokenList` stream (lazy extraction)
2. **Parser ↔ SSA**: `TASTNode` tree → `TSSAProgram` (CFG + instructions)
3. **SSA ↔ Optimizer**: In-place `TSSAProgram` modification via `Run*` methods
4. **SSA ↔ Bytecode Compiler**: `TSSAInstruction` → `TBytecodeInstruction` mapping
5. **Bytecode ↔ VM**: `TBytecodeProgram.LoadProgram()` → execution
6. **VM ↔ Output**: `IOutputDevice` / `IInputDevice` interfaces

---

## 13. COMMODORE 128 COMPATIBILITY LAYER

SedaiBasic emulates key aspects of the Commodore 128 for compatibility:

### Memory Mapping (PEEK/POKE)
| Address Range | Description |
|---------------|-------------|
| 1024-2023 | Screen memory (40×25 characters) |
| 55296-56295 | Color memory |
| 53280 | Border color (VIC-II $D020) |
| 53281 | Background color (VIC-II $D021) |
| 54272-54296 | SID registers (partial) |

### Graphics Modes
| Mode | Resolution | Description |
|------|------------|-------------|
| 0 | 40×25 text | Standard text mode |
| 1 | 320×200 | Hi-res bitmap |
| 2 | 160×200 | Multicolor bitmap |
| 3 | 320×200 split | Text + bitmap |
| 4 | 640×200 | VIC-II extended |
| 5-10 | Various | Extended modes |
| 11 | Custom | SDL2 native (up to 4K) |

### Extended Features
- **256 sprites** (vs 8 original)
- **256-color palette** (vs 16 original)
- **Fractional sprite scaling** (0.1x - 10.0x)
- **RGBA colors** with alpha transparency
- **JSON palette files** (PLOAD/PSAVE)

---

## 14. SOURCE FILE INDEX

### Core Pipeline
| File | Description |
|------|-------------|
| `sedailexerfsm.pas` | FSM Lexer with FastPath |
| `SedaiLexerTypes.pas` | Token types |
| `sedaikeywordtrie.pas` | Keyword recognition |
| `SedaiPackratParser.pas` | Packrat parser (statements) |
| `sedaiexpressionparser.pas` | Pratt parser (expressions) |
| `sedaiast.pas` | AST node definitions |
| `SedaiSSA.pas` | SSA generator |
| `SedaiSSATypes.pas` | SSA types |
| `SedaiBytecodeCompiler.pas` | Bytecode compiler |
| `SedaiBytecodeTypes.pas` | Bytecode opcodes |
| `SedaiBytecodeVM.pas` | Virtual machine |

### Optimization Passes
| File | Pass |
|------|------|
| `SedaiConstProp.pas` | Constant propagation |
| `SedaiConstPropAggressive.pas` | Aggressive const prop |
| `SedaiCopyProp.pas` | Copy propagation |
| `SedaiDBE.pas` | Dead block elimination |
| `SedaiDCE.pas` | Dead code elimination |
| `SedaiDominators.pas` | Dominator tree |
| `SedaiGVN.pas` | Global value numbering |
| `SedaiCSE.pas` | Common subexpression elim |
| `SedaiAlgebraic.pas` | Algebraic simplification |
| `SedaiStrengthReduction.pas` | Strength reduction |
| `SedaiLICM.pas` | Loop-invariant code motion |
| `SedaiLoopUnroll.pas` | Loop unrolling |
| `SedaiGosubInlining.pas` | GOSUB inlining |
| `SedaiCopyCoalescing.pas` | Copy coalescing |
| `SedaiSSAConstruction.pas` | SSA construction |
| `SedaiPhiElimination.pas` | PHI elimination |
| `SedaiPeephole.pas` | Peephole optimization |
| `SedaiSuperinstructions.pas` | Superinstruction fusion |
| `SedaiNopCompaction.pas` | NOP removal |
| `SedaiRegisterCompaction.pas` | Register compaction |
| `SedaiRegAllocator.pas` | Register allocation |

### Subsystems
| File | Description |
|------|-------------|
| `SedaiGraphicsModes.pas` | Graphics mode handling |
| `SedaiGraphicsPrimitives.pas` | Drawing primitives |
| `sedaisprite.pas` | Sprite system |
| `SedaiSpriteTypes.pas` | Sprite types |
| `SedaiC128MemoryMapper.pas` | C128 memory emulation |
| `sedaisdl2graphicsoutput.pas` | SDL2 graphics backend |
| `sedaisdl2input.pas` | SDL2 input handling |

---

## 15. PERFORMANCE CHARACTERISTICS

### Benchmark: SIEVE (N=10000)
| Configuration | Time |
|---------------|------|
| All optimizations disabled | ~150ms |
| All optimizations enabled | ~25-30ms |
| Improvement | **~5-6x faster** |

### Key Performance Factors
1. **Superinstructions**: 15-30% dispatch reduction
2. **Register compaction**: Better cache locality
3. **Loop optimizations**: LICM + unrolling for hot loops
4. **Constant propagation**: Eliminates runtime computation
5. **Dead code elimination**: Smaller bytecode

### Memory Usage
- Bytecode instruction: **16 bytes** (optimized from 20)
- Token record: **~10 bytes** (zero-allocation pool)
- SSA instruction: ~80 bytes (temporary during compilation)
- VM register arrays: Dynamic, based on program needs
