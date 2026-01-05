{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}
program SedaiBasic2Tester;

{$mode objfpc}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, TypInfo, DateUtils, Math, Variants, SDL2,
  // Lexer units
  SedaiDateTimeUtils, SedaiMemoryUtils,
  SedaiLexerFSM, SedaiLexerTypes, SedaiLexerToken, SedaiTokenList,
  // Parser units
  SedaiParserTypes, SedaiAST, SedaiParserContext, SedaiParserResults, SedaiParserErrors,
  SedaiPackratCore, SedaiExpressionParser, SedaiPackratParser,
  // Executor units
  SedaiExecutor, SedaiExecutionResult, SedaiExecutorContext, SedaiCommandTypes,
  // NEW: Console and Program Memory units
  SedaiConsole, SedaiProgramMemory, SedaiCommandRouter,
  SedaiOutputInterface, SedaiSDL2Output, SedaiGraphicsModes;

var
  SourcePath: string;

// === LEXER TESTING ===

procedure TestLexer(Source: TStringList);
var
  Lexer: TLexerFSM;
  Token: TLexerToken;
  TokenCount, i: Integer;
  TokensPerSecond: Double;
  ATimer: THiResTimer;
  ElapsedMicros: Double;
  ATokenList: TTokenList;
begin
  WriteLn('=== LEXER TEST ===');
  WriteLn;

  Lexer := TLexerFSM.Create;
  try
    // Configure lexer
    Lexer.SetHasLineNumbers(True);
    Lexer.SetRequireSpacesBetweenTokens(True);
    Lexer.SetCaseSensitive(False);
    Lexer.SetCustomTokenDensity(0.25, 2);

    // Set source and start timing
    Lexer.Source := Source.Text;

    for i := 1 to 1 do
    begin
      ATimer.Start;
      ATokenList := Lexer.ScanAllTokensFast;
      ElapsedMicros := ATimer.ElapsedMicroseconds;
      if ElapsedMicros > 0 then
        TokensPerSecond := Lexer.TokenCount / (ElapsedMicros / 1000000.0)
      else
        TokensPerSecond := 0;
      WriteLn(Format('Fast scanned %d tokens in %.2f ms (%.0f tokens/sec)',
        [Lexer.TokenCount, ElapsedMicros / 1000.0, TokensPerSecond]));
      Sleep(200);
    end;

    // Show tokens (safe iteration)
    if Assigned(ATokenList) and (ATokenList.Count > 0) then
    begin
      for i := 0 to ATokenList.Count - 1 do
      begin
        if i >= Lexer.TokenCount then Break; // Safety check

        Token := TLexerToken(ATokenList.Items[i]);
        if not Assigned(Token) then Break; // Safety check

        if Token.TokenType <> ttEndOfLine then
        begin
          WriteLn(Format('%-20s %-25s %-8d %-8d', [
            Copy(Token.Value, 1, 19),
            Copy(GetEnumName(TypeInfo(TTokenType), Ord(Token.TokenType)), 1, 24),
            Token.Line,
            Token.Column
          ]));
        end
        else
        begin
          WriteLn(Format('%-20s %-25s %-8d', [
            '<EOL>',
            GetEnumName(TypeInfo(TTokenType), Ord(Token.TokenType)),
            Token.Line
          ]));
        end;
      end;
    end;

  finally
    Lexer.Free;
  end;
end;

// === PARSER TESTING ===

function CountNodes(Node: TASTNode): Integer;
var
  i: Integer;
begin
  if not Assigned(Node) then
  begin
    Result := 0;
    Exit;
  end;

  Result := 1; // Count this node

  // Count all children recursively
  for i := 0 to Node.ChildCount - 1 do
    Result := Result + CountNodes(Node.Child[i]);
end;

procedure PrintASTTree(Node: TASTNode; Indent: Integer = 0; TokenList: TTokenList = nil);
var
 i: Integer;
 Prefix: string;
 NodeInfo: string;
begin
 if not Assigned(Node) then Exit;

 Prefix := StringOfChar(' ', Indent * 2);

 // Format node information with type and value
 if not VarIsEmpty(Node.Value) then
   NodeInfo := Format('%s: "%s"', [
     GetEnumName(TypeInfo(TASTNodeType), Ord(Node.NodeType)),
     VarToStr(Node.Value)
   ])
 else
   NodeInfo := GetEnumName(TypeInfo(TASTNodeType), Ord(Node.NodeType));

 WriteLn(Format('%s%s', [Prefix, NodeInfo]));

 // Print all children recursively
 for i := 0 to Node.ChildCount - 1 do
   PrintASTTree(Node.Child[i], Indent + 1, TokenList);
end;

procedure TestParser(Source: TStringList);
var
 Lexer: TLexerFSM;
 Parser: TPackratParser;
 TokenList: TTokenList;
 Result: TParsingResult;
 Timer: THiResTimer;
 i: Integer;
 ValidTokenCount: Integer;
 LineNumbers, Statements: Integer;
 AElapsed: double;
begin
 WriteLn('=== PACKRAT PARSER TEST ===');
 WriteLn;

 // === CREATE COMPONENTS ===
 Lexer := TLexerFSM.Create;
 Parser := CreatePackratParser;

 try
   // === CONFIGURE LEXER ===
   WriteLn('Configuring lexer...');
   Lexer.SetHasLineNumbers(True);
   Lexer.SetRequireSpacesBetweenTokens(True);
   Lexer.SetCaseSensitive(False);
   Lexer.SetCustomTokenDensity(0.25, 2);

   // === TOKENIZE SOURCE ===
   WriteLn('Tokenizing...');
   Timer := CreateHiResTimer;
   Lexer.Source := Source.Text;
   TokenList := Lexer.ScanAllTokensFast;

   // Count valid tokens (until ttEndOfFile)
   ValidTokenCount := 0;
   if Assigned(TokenList) then
   begin
     for i := 0 to TokenList.Count - 1 do
     begin
       if TLexerToken(TokenList.Items[i]).TokenType = ttEndOfFile then
         Break;
       Inc(ValidTokenCount);
     end;
   end;

   WriteLn(Format('>>> Tokenized %d tokens in %.2f ms',
     [ValidTokenCount, Timer.ElapsedMilliseconds]));
   WriteLn;

   // === PARSE PROGRAM ===
   WriteLn('Parsing...');
   Timer := CreateHiResTimer;

   Timer.Start;
   Result := Parser.Parse(TokenList);
   AElapsed := Timer.ElapsedMilliseconds;

   WriteLn(Format('>>> Parsed in %.2f ms', [AElapsed]));
   WriteLn;

   // === SHOW RESULTS ===
   if Result.Success then
   begin
     WriteLn('>>>>> PARSING SUCCESSFUL! <<<<<');
     WriteLn;

     if Assigned(Result.AST) and Result.AST.HasChildren then
     begin
       WriteLn('AST TREE:');
       WriteLn(StringOfChar('=', 60));
       PrintASTTree(Result.AST, 0, TokenList);
       WriteLn(StringOfChar('=', 60));
       WriteLn;

       // Basic stats
       LineNumbers := 0;
       Statements := 0;
       for i := 0 to Result.AST.ChildCount - 1 do
       begin
         if Result.AST.Child[i].NodeType = antLineNumber then
           Inc(LineNumbers)
         else
           Inc(Statements);
       end;

       WriteLn(Format('Total nodes: %d', [CountNodes(Result.AST)]));
       WriteLn(Format('Line numbers: %d', [LineNumbers]));
       WriteLn(Format('Statements: %d', [Statements]));
     end
     else
     begin
       WriteLn('!!! Empty AST generated !!!');
     end;
   end
   else
   begin
     WriteLn('!!!!! PARSING FAILED !!!!!');
     WriteLn;

     if Result.Errors.Count > 0 then
     begin
      WriteLn('Errors:');
      for i := 0 to Result.Errors.Count - 1 do
        WriteLn(Format('  %s', [Result.Errors[i].ToString]));
     end;
   end;

   Result.Free;

 finally
   Parser.Free;
   Lexer.Free;
 end;

 WriteLn;
end;

// === NEW SDL2 CONSOLE TEST ===

// Hack per modificare le costanti a runtime (solo per test!)
type
  PGraphicModeInfo = ^TGraphicModeInfo;
  PGraphicModesArray = ^TGraphicModesArray;
  TGraphicModesArray = array[TGraphicMode] of TGraphicModeInfo;

procedure TestConsole(Source: TStringList);
var
  Console: TSedaiConsole;
  TinyBasPath: string;
  TinyBasSource: TStringList;
  i: Integer;
  ModesPtr: PGraphicModesArray;
  OriginalWindowWidth, OriginalWindowHeight: Integer;
begin
  WriteLn('===== SEDAIBASIC V2.0 SDL2 CONSOLE TEST =====');
  WriteLn('Creating SDL2 TTF-based console...');

  // Hack: modifica temporanea della costante GRAPHICS_MODES per il test
  // Casting di const a var per modificare i valori a runtime (unsafe ma funziona per test)
  ModesPtr := @GRAPHICS_MODES;
  
  // Salva valori originali
  OriginalWindowWidth := ModesPtr^[gm40ColText].WindowWidth;
  OriginalWindowHeight := ModesPtr^[gm40ColText].WindowHeight;
  
  // Modifica temporanea: 2x scale + bordo 50px per lato
  // NativeWidth: 320 -> WindowWidth: 320*2 + 50*2 = 740
  // NativeHeight: 200 -> WindowHeight: 200*2 + 50*2 = 500
  WriteLn('Modifico dimensioni finestra: 740x500 (2x + bordo 50px)');
  ModesPtr^[gm40ColText].WindowWidth := 740;
  ModesPtr^[gm40ColText].WindowHeight := 500;

  Console := TSedaiConsole.Create;
  try
    WriteLn('Console created successfully');

    // Initialize with GRAPHIC 0 (320x200, 40x25 text mode) in windowed mode
    if Console.Initialize(gm40ColText, False) then
    begin
      WriteLn('Console initialized successfully');
      
      // Load SIEVE.BAS for performance benchmark
      TinyBasPath := 'bas' + PathDelim + 'SIEVE.BAS';
      //TinyBasPath := 'test_only_rem.bas';
      if FileExists(TinyBasPath) then
      begin
        WriteLn('Loading SIEVE.BAS...');
        TinyBasSource := TStringList.Create;
        try
          TinyBasSource.LoadFromFile(TinyBasPath);
          // Load each line into program memory
          for i := 0 to TinyBasSource.Count - 1 do
          begin
            if Trim(TinyBasSource[i]) <> '' then
              Console.ProcessCommand(TinyBasSource[i]);
          end;
          WriteLn('SIEVE.BAS loaded successfully (', TinyBasSource.Count, ' lines)');
          WriteLn('Starting benchmark...');
          // Auto-execute for benchmark (DISABLED - manual execution only)
          // Console.ProcessCommand('RUN');
        finally
          TinyBasSource.Free;
        end;
      end
      else
      begin
        WriteLn('WARNING: TINY.BAS not found at ', TinyBasPath);
      end;
      
      WriteLn('Starting console run loop...');

      // Run the console main loop
      Console.Run;
    end
    else
    begin
      WriteLn('ERROR: Failed to initialize console');
    end;

  finally
    Console.Free;
    // Ripristina valori originali
    WriteLn('Ripristino dimensioni originali finestra');
    ModesPtr^[gm40ColText].WindowWidth := OriginalWindowWidth;
    ModesPtr^[gm40ColText].WindowHeight := OriginalWindowHeight;
  end;

  WriteLn('Session ended.');
end;

// === ENUMERATE DISPLAY MODES ===

procedure EnumerateDisplayModes;
var
  NumDisplays, NumModes, i, j: Integer;
  Mode: TSDL_DisplayMode;
  Ratio: Double;
  RatioStr: string;
begin
  WriteLn;
  WriteLn('=== ENUMERATING SDL2 DISPLAY MODES ===');
  WriteLn;

  if SDL_Init(SDL_INIT_VIDEO) < 0 then
  begin
    WriteLn('ERROR: Failed to initialize SDL2: ', SDL_GetError);
    Exit;
  end;

  try
    NumDisplays := SDL_GetNumVideoDisplays;
    WriteLn('Number of displays: ', NumDisplays);
    WriteLn;

    for i := 0 to NumDisplays - 1 do
    begin
      WriteLn('Display ', i, ': ', SDL_GetDisplayName(i));
      WriteLn(StringOfChar('-', 70));

      NumModes := SDL_GetNumDisplayModes(i);
      WriteLn('  Available modes: ', NumModes);
      WriteLn;

      WriteLn('  #   Resolution      Ratio     Refresh  Format');
      WriteLn('  ', StringOfChar('-', 65));

      for j := 0 to NumModes - 1 do
      begin
        if SDL_GetDisplayMode(i, j, @Mode) = 0 then
        begin
          // Calculate aspect ratio
          Ratio := Mode.w / Mode.h;

          // Identify common aspect ratios
          if Abs(Ratio - 4/3) < 0.01 then
            RatioStr := '4:3'
          else if Abs(Ratio - 16/9) < 0.01 then
            RatioStr := '16:9'
          else if Abs(Ratio - 16/10) < 0.01 then
            RatioStr := '16:10'
          else if Abs(Ratio - 21/9) < 0.01 then
            RatioStr := '21:9'
          else if Abs(Ratio - 5/4) < 0.01 then
            RatioStr := '5:4'
          else
            RatioStr := Format('%.3f', [Ratio]);

          WriteLn(Format('  %-3d %4dx%-4d    %-8s  %3dHz   %s',
            [j, Mode.w, Mode.h, RatioStr, Mode.refresh_rate,
             IntToHex(Mode.format, 8)]));
        end;
      end;

      WriteLn;
    end;

  finally
    SDL_Quit;
  end;

  WriteLn('Press Enter to continue...');
  ReadLn;
end;

// === SIMPLE EXPRESSION TEST ===

procedure TestExpression(const ExprText: string);
var
  Lexer: TLexerFSM;
  Parser: TExpressionParser;
  Context: TParserContext;
  TokenList: TTokenList;
  AST: TASTNode;
  Timer: THiResTimer;
begin
  WriteLn(Format('Testing expression: "%s"', [ExprText]));

  Lexer := TLexerFSM.Create;
  Parser := CreateExpressionParser;

  try
    Lexer.SetCaseSensitive(False);
    Lexer.Source := ExprText;
    TokenList := Lexer.ScanAllTokensFast;

    Context := TParserContext.Create(TokenList);
    try
      Parser.SetContext(Context);

      Timer := CreateHiResTimer;
      AST := Parser.ParseExpression;

      Write(Format('  Result: %s (%.1f micros) ',
        [BoolToStr(Assigned(AST) and not Context.HasErrors, True), Timer.ElapsedMicroseconds]));

      if Assigned(AST) and not Context.HasErrors then
      begin
        WriteLn('OK');
        PrintASTTree(AST, 2); // Indent by 2
        AST.Free;
      end
      else
      begin
        WriteLn('FAIL');
        if Context.HasErrors then
          WriteLn(Format('    Error: %s', [Context.Errors[0].ToString]));
      end;

    finally
      Context.Free;
    end;

  finally
    Parser.Free;
    Lexer.Free;
  end;

  WriteLn;
end;

// === MENU SYSTEM ===

function ShowMenu: Char;
begin
  WriteLn;
  WriteLn('=== SEDAIBASIC2 TESTING MENU ===');
  WriteLn;
  WriteLn('Choose a test to run:');
  WriteLn('  L - Lexer Test');
  WriteLn('  P - Parser Test');
  WriteLn('  C - Interactive Console (Simple)');
  WriteLn('  A - All Tests (Lexer -> Parser -> Console)');
  WriteLn('  X - Expression Test');
  WriteLn('  D - Display Modes (Enumerate SDL2 resolutions)');
  WriteLn('  Q - Quit');
  WriteLn;
  Write('Your choice: ');
  ReadLn(Result);
  Result := UpCase(Result);
  WriteLn;
end;

procedure RunExpressionTests;
begin
  WriteLn('=== EXPRESSION TESTS ===');
  WriteLn;

  TestExpression('5 + 3');
  TestExpression('10 - 4 * 2');
  TestExpression('(5 + 3) * 2');
  TestExpression('A + B * C');
  TestExpression('SIN(3.14159)');
  TestExpression('LEN("HELLO")');
  TestExpression('A(5)');
  TestExpression('X = 5');

  WriteLn('Expression tests completed.');
end;

procedure LoadSourceCode(Source: TStringList);
var
  TestFile: string;
begin
  // Try to load TINY.BAS file
  //TestFile := ConcatPaths([ExtractFilePath(ParamStr(0)), 'bas', 'TINY.BAS']);
  //TestFile := ConcatPaths([ExtractFilePath(ParamStr(0)), 'bas', 'FIBONACCI.BAS']);
  //TestFile := ConcatPaths([ExtractFilePath(ParamStr(0)), 'bas', 'eratosthenes.bas']);
  TestFile := ConcatPaths([ExtractFilePath(ParamStr(0)), 'bas', 'TEST_AST.BAS']);

  if FileExists(TestFile) then
  begin
    WriteLn('Loading file: ', TestFile);
    Source.LoadFromFile(TestFile);
    SourcePath := TestFile;
  end
  else
  begin
    // Use SIMPLIFIED test program to avoid complex features that might crash
    WriteLn('File not found. Using simplified test program...');
    SourcePath := 'Built-in Test';
    Source.Text :=
      '10 REM SIMPLE TEST' + LineEnding +
      '20 PRINT "HELLO WORLD"' + LineEnding +
      '30 LET A = 5' + LineEnding +
      '40 PRINT "A ="; A' + LineEnding +
      '50 END';
  end;

  // Show source code
  //WriteLn('Source code:');
  //WriteLn(StringOfChar('-', 50));
  //WriteLn(Source.Text);
  //WriteLn(StringOfChar('-', 50));
  //WriteLn;
end;

// === MAIN PROGRAM ===

var
  Source: TStringList;
  Choice: Char;

begin
  try
    // Initialize random number generator
    Randomize;

    WriteLn('SedaiBasic2 Simple Console Tester');
    WriteLn('==================================');

    Source := TStringList.Create;
    try
      LoadSourceCode(Source);

      repeat
        Choice := ShowMenu;

        case Choice of
          'L': TestLexer(Source);
          'P': TestParser(Source);
          'C': TestConsole(Source);
          'A': begin
                 TestLexer(Source);
                 WriteLn;
                 TestParser(Source);
                 WriteLn;
                 TestConsole(Source);
               end;
          'X': RunExpressionTests;
          'D': EnumerateDisplayModes;
          'Q': WriteLn('Goodbye!');
          else
            WriteLn('Invalid choice. Please try again.');
        end;

        if Choice <> 'Q' then
        begin
          WriteLn;
          WriteLn('Press Enter to continue...');
          ReadLn;
        end;

      until Choice = 'Q';

    finally
      Source.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      WriteLn;
      WriteLn('Press Enter to exit...');
      ReadLn;
      ExitCode := 1;
    end;
  end;
end.
