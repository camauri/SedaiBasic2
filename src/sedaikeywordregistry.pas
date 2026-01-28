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
unit SedaiKeywordRegistry;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, fgl, contnrs, StrUtils, FPMasks, Math, RegExpr, DateUtils,
  SedaiLexerTypes, SedaiLexerToken, SedaiBasicKeywords,
  SedaiKeywordTypes, SedaiKeywordCategory, SedaiKeywordTrie, SedaiKeywordUtils;

type

  { TKeywordRegistry - Advanced keyword registry }
  TKeywordRegistry = class
  private
    FKeywords: TFPHashObjectList; // Main hash table O(1)
    FConstraints: TFPHashObjectList;
    FCategories: TFPHashObjectList; // Keyword categories
    FTrieRoot: TKeywordTrieNode; // Trie for prefix search
    FMaxKeywordLength: Integer;
    FMinKeywordLength: Integer;
    FCaseSensitive: Boolean;
    FAllowDuplicates: Boolean;
    FAutoCategories: Boolean;

    // Statistics
    FTotalRegistrations: Integer;
    FTotalLookups: Integer;
    FTotalPrefixSearches: Integer;

    procedure UpdateLengthLimits(const Keyword: string);
    procedure RegisterBasicKeywords;
    procedure BuildTrie;
    procedure AddToTrie(const Keyword: string; KeywordInfo: TKeywordInfo);
    function SearchTrie(const Text: string; AllowPartial: Boolean): TKeywordMatchArray;

    // Search optimizations
    function ShouldUseDirectLookup(const Text: string; const Options: TKeywordSearchOptions): Boolean;
    function PerformDirectLookup(const Text: string): TKeywordMatchResult;
    function ApplySearchFilters(const Matches: TKeywordMatchArray; const Options: TKeywordSearchOptions): TKeywordMatchArray;

  protected
    // Virtual methods for extensibility
    procedure DoKeywordRegistered(KeywordInfo: TKeywordInfo); virtual;
    procedure DoKeywordRemoved(const Keyword: string; KeywordInfo: TKeywordInfo); virtual;
    procedure DoCategoryAdded(Category: TKeywordCategory); virtual;

  public
    constructor Create(ACaseSensitive: Boolean = False; AAllowDuplicates: Boolean = False);
    destructor Destroy; override;

    // === REGISTRATION METHODS ===
    procedure RegisterKeyword(const Keyword: string; TokenType: TTokenType = ttKeyword;
                             const Description: string = ''; const Category: string = ''); overload;
    procedure RegisterKeyword(KeywordInfo: TKeywordInfo); overload;
    procedure RegisterKeywords(const Keywords: array of string; TokenType: TTokenType = ttKeyword;
                              const Category: string = '');
    procedure RegisterKeywordsFromFile(const FileName: string; TokenType: TTokenType = ttKeyword);
    procedure RegisterKeywordsFromStrings(Strings: TStrings; TokenType: TTokenType = ttKeyword);

    function UnregisterKeyword(const Keyword: string): Boolean;
    function UnregisterKeywordsByCategory(const Category: string): Integer;
    procedure Clear;

    // === BASIC LOOKUP METHODS ===
    function IsKeyword(const Text: string): Boolean;
    function GetKeywordInfo(const Text: string): TKeywordInfo;
    function GetKeywordType(const Text: string): TTokenType;
    function TryGetKeywordInfo(const Text: string; out KeywordInfo: TKeywordInfo): Boolean;

    // === ADVANCED SEARCH METHODS ===
    function FindKeyword(const Text: string; Options: TKeywordSearchOptions): TKeywordMatchResult;
    function FindKeywords(const Text: string; Options: TKeywordSearchOptions): TKeywordMatchArray;
    function FindKeywordsByCategory(const Category: string): TKeywordInfoArray;
    function FindKeywordsByType(TokenType: TTokenType): TKeywordInfoArray;
    function FindKeywordsByPattern(const Pattern: string; UseRegex: Boolean = False): TKeywordInfoArray;

    // === PREFIX MATCHING (for BASIC classic mode) ===
    function HasKeywordWithPrefix(const Prefix: string): Boolean;
    function FindKeywordsWithPrefix(const Prefix: string; MaxResults: Integer = 0): TKeywordInfoArray;
    function FindLongestKeywordMatch(const Text: string; out KeywordInfo: TKeywordInfo;
                                   out RemainingText: string): Boolean;
    function FindBestKeywordMatch(const Text: string; MinLength: Integer = 1): TKeywordMatchResult;

    // === CATEGORY MANAGEMENT ===
    function AddCategory(const Name, Description: string; IsSystem: Boolean = False): TKeywordCategory;
    function GetCategory(const Name: string): TKeywordCategory;
    function HasCategory(const Name: string): Boolean;
    function RemoveCategory(const Name: string): Boolean;
    function GetCategoryNames: TStringList;
    function GetKeywordsByCategory(const Category: string): TStringList;

    // === CONSTRAINT MANAGEMENT ===
    procedure RegisterConstraint(const Keyword, RequiredNext: string;
                               Optional: Boolean = False;
                               const ErrorMsg: string = '');
    function HasConstraint(const Keyword: string): Boolean; inline;
    function GetConstraint(const Keyword: string): TKeywordConstraint;
    function ValidateConstraint(const Keyword, NextToken: string): Boolean;
    procedure ClearConstraints;

    // === ANALYSIS & STATISTICS ===
    function GetKeywordCount: Integer;
    function GetCategoryCount: Integer;
    function GetAverageKeywordLength: Double;
    function GetKeywordLengthDistribution: TStringList; // Length -> Count
    function GetTypeDistribution: TStringList; // TokenType -> Count
    function GetCategoryDistribution: TStringList; // Category -> Count
    function GetStatisticsReport: string;

    // === OPTIMIZATION ===
    procedure OptimizeForSpeed; // Rebuild structures for speed

    // === SERIALIZATION ===
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    // === UTILITY METHODS ===
    function Clone: TKeywordRegistry;
    procedure Assign(Source: TKeywordRegistry);
    procedure Merge(Other: TKeywordRegistry; AllowConflicts: Boolean = False);
    function IsEqual(Other: TKeywordRegistry): Boolean;
    function GetChecksum: string;

    // === DEBUG & DIAGNOSTICS ===
    function GetDebugInfo: string;
    procedure DumpToConsole;
    procedure DumpTrieToConsole;

    // === PROPERTIES ===
    property KeywordCount: Integer read GetKeywordCount;
    property CategoryCount: Integer read GetCategoryCount;
    property MinKeywordLength: Integer read FMinKeywordLength;
    property MaxKeywordLength: Integer read FMaxKeywordLength;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property AllowDuplicates: Boolean read FAllowDuplicates write FAllowDuplicates;
    property AutoCategories: Boolean read FAutoCategories write FAutoCategories;
    property TotalRegistrations: Integer read FTotalRegistrations;
    property TotalLookups: Integer read FTotalLookups;
    property TotalPrefixSearches: Integer read FTotalPrefixSearches;
  end;

// === FACTORY FUNCTIONS ===
function CreateKeywordRegistry(CaseSensitive: Boolean = False): TKeywordRegistry;
function CreateBasicKeywordRegistry: TKeywordRegistry;
function CreateModernKeywordRegistry: TKeywordRegistry;

implementation

{ TKeywordRegistry }

constructor TKeywordRegistry.Create(ACaseSensitive: Boolean; AAllowDuplicates: Boolean);
begin
  inherited Create;

  FKeywords := TFPHashObjectList.Create(True); // Own TKeywordInfo objects
  FConstraints := TFPHashObjectList.Create(True); // Own constraint objects
  FCategories := TFPHashObjectList.Create(True); // Own TKeywordCategory objects
  FTrieRoot := TKeywordTrieNode.Create;

  FCaseSensitive := ACaseSensitive;
  FAllowDuplicates := AAllowDuplicates;
  FAutoCategories := True;

  FMinKeywordLength := MaxInt;
  FMaxKeywordLength := 0;

  // Initialize statistics
  FTotalRegistrations := 0;
  FTotalLookups := 0;
  FTotalPrefixSearches := 0;

  //WriteLn('DEBUG TKeywordRegistry: Created');

  // Register all basic keywords
  RegisterBasicKeywords;
end;

destructor TKeywordRegistry.Destroy;
begin
  //WriteLn('DEBUG TKeywordRegistry: Destroying');
  FTrieRoot.Free;
  FCategories.Free;
  FConstraints.Free;
  FKeywords.Free;
  inherited Destroy;
end;

procedure TKeywordRegistry.UpdateLengthLimits(const Keyword: string);
var
  KeywordLength: Integer;
begin
  KeywordLength := System.Length(Keyword);

  if KeywordLength < FMinKeywordLength then
  begin
    FMinKeywordLength := KeywordLength;
    //WriteLn('DEBUG RegisterKeyword: New min length ', FMinKeywordLength, ' from "', Keyword, '"');
  end;

  if KeywordLength > FMaxKeywordLength then
  begin
    FMaxKeywordLength := KeywordLength;
    //WriteLn('DEBUG RegisterKeyword: New max length ', FMaxKeywordLength, ' from "', Keyword, '"');
  end;
end;

procedure TKeywordRegistry.DoKeywordRegistered(KeywordInfo: TKeywordInfo);
begin
  // Virtual method - override in descendants
  Inc(FTotalRegistrations);
end;

procedure TKeywordRegistry.DoKeywordRemoved(const Keyword: string; KeywordInfo: TKeywordInfo);
begin
  // Virtual method - override in descendants
end;

procedure TKeywordRegistry.DoCategoryAdded(Category: TKeywordCategory);
begin
  // Virtual method - override in descendants
end;

function TKeywordRegistry.ShouldUseDirectLookup(const Text: string; const Options: TKeywordSearchOptions): Boolean;
begin
  // Use direct O(1) lookup if:
  // 1. No partial match required
  // 2. No category filter
  // 3. Minimum confidence is 0 or very low
  // 4. Text contains no wildcards
  Result := (not Options.AllowPartialMatch) and
            (Options.CategoryFilter = '') and
            (Options.MinConfidence <= 0.1) and
            (Pos('*', Text) = 0) and
            (Pos('?', Text) = 0);
end;

function TKeywordRegistry.PerformDirectLookup(const Text: string): TKeywordMatchResult;
var
  UpperText: string;
  KeywordInfo: TKeywordInfo;
begin
  // Initialize result
  Result.Found := False;
  Result.KeywordInfo := nil;
  Result.MatchedLength := 0;
  Result.RemainingText := Text;
  Result.MatchType := mtNone;
  Result.Confidence := 0.0;

  // Prepare text for search
  if FCaseSensitive then
    UpperText := Text
  else
    UpperText := UpperCase(Text);

  // O(1) lookup in hash table
  KeywordInfo := TKeywordInfo(FKeywords.Find(UpperText));

  if Assigned(KeywordInfo) then
  begin
    Result.Found := True;
    Result.KeywordInfo := KeywordInfo;
    Result.MatchedLength := Length(Text);
    Result.RemainingText := '';
    Result.MatchType := mtExact;
    Result.Confidence := 1.0;
  end;
end;

function TKeywordRegistry.ApplySearchFilters(const Matches: TKeywordMatchArray;
                                           const Options: TKeywordSearchOptions): TKeywordMatchArray;
var
  i, ResultCount: Integer;
  Match: TKeywordMatchResult;
begin
  SetLength(Result, 0);
  ResultCount := 0;

  for i := 0 to Length(Matches) - 1 do
  begin
    Match := Matches[i];

    // Apply filters
    if Match.Confidence < Options.MinConfidence then
      Continue;

    if not Options.IncludeDeprecated and Assigned(Match.KeywordInfo) and
       Match.KeywordInfo.IsDeprecated then
      Continue;

    if (Options.CategoryFilter <> '') and Assigned(Match.KeywordInfo) and
       (Match.KeywordInfo.Category <> Options.CategoryFilter) then
      Continue;

    // Add to results
    if ResultCount >= Length(Result) then
      SetLength(Result, Max(4, Length(Result) * 2));

    Result[ResultCount] := Match;
    Inc(ResultCount);

    // Check results limit
    if (Options.MaxResults > 0) and (ResultCount >= Options.MaxResults) then
      Break;
  end;

  SetLength(Result, ResultCount);
end;

// === REGISTRATION METHODS ===

procedure TKeywordRegistry.RegisterKeyword(const Keyword: string; TokenType: TTokenType;
                                         const Description: string; const Category: string);
var
  KeywordInfo: TKeywordInfo;
  UpperKeyword: string;
  CategoryObj: TKeywordCategory;
begin
  UpperKeyword := UpperCase(Keyword);

  // Check for duplicates
  if not FAllowDuplicates and IsKeyword(UpperKeyword) then
    raise EKeywordDuplicateException.CreateFmt('Keyword "%s" already registered', [Keyword]);

  // Create keyword info
  if Description <> '' then
    KeywordInfo := TKeywordInfo.Create(UpperKeyword, TokenType, Description)
  else
    KeywordInfo := TKeywordInfo.Create(UpperKeyword, TokenType);

  // Set category
  if Category <> '' then
  begin
    KeywordInfo.Category := Category;

    // Auto-create category if needed
    if FAutoCategories and not HasCategory(Category) then
    begin
      CategoryObj := AddCategory(Category, Format('Auto-created category for %s keywords', [Category]));
      CategoryObj.AddKeyword(UpperKeyword);
    end
    else
    begin
      CategoryObj := GetCategory(Category);
      if Assigned(CategoryObj) then
        CategoryObj.AddKeyword(UpperKeyword);
    end;
  end
  else if FAutoCategories then
  begin
    // Auto-categorize based on token type
    KeywordInfo.Category := GetTokenCategory(TokenType);
  end;

  // Add to hash table
  FKeywords.Add(UpperKeyword, KeywordInfo);

  // Add to trie for prefix searches
  AddToTrie(UpperKeyword, KeywordInfo);

  // Update statistics
  UpdateLengthLimits(Keyword);
  DoKeywordRegistered(KeywordInfo);

  //WriteLn('DEBUG RegisterKeyword: Added "', UpperKeyword, '" -> ', Ord(TokenType), ' (', Description, ')');
end;

procedure TKeywordRegistry.RegisterKeyword(KeywordInfo: TKeywordInfo);
begin
  if not Assigned(KeywordInfo) then
    raise EKeywordRegistryException.Create('KeywordInfo cannot be nil');

  RegisterKeyword(KeywordInfo.Keyword, KeywordInfo.TokenType,
                 KeywordInfo.Description, KeywordInfo.Category);
end;

procedure TKeywordRegistry.RegisterKeywords(const Keywords: array of string; TokenType: TTokenType;
                                          const Category: string);
var
  i: Integer;
begin
  for i := Low(Keywords) to High(Keywords) do
    RegisterKeyword(Keywords[i], TokenType, '', Category);
end;

procedure TKeywordRegistry.RegisterKeywordsFromFile(const FileName: string; TokenType: TTokenType);
var
  FileContent: TStringList;
begin
  if not FileExists(FileName) then
    raise EKeywordRegistryException.CreateFmt('Keyword file not found: %s', [FileName]);

  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    RegisterKeywordsFromStrings(FileContent, TokenType);
  finally
    FileContent.Free;
  end;
end;

procedure TKeywordRegistry.RegisterKeywordsFromStrings(Strings: TStrings; TokenType: TTokenType);
var
  i: Integer;
  Line, Keyword, Description: string;
  SeparatorPos: Integer;
begin
  for i := 0 to Strings.Count - 1 do
  begin
    Line := Trim(Strings[i]);

    // Skip empty lines and comments
    if (Line = '') or (Line[1] = '#') or (Line[1] = ';') then
      Continue;

    // Parse line: keyword[=description]
    SeparatorPos := Pos('=', Line);
    if SeparatorPos > 0 then
    begin
      Keyword := Trim(Copy(Line, 1, SeparatorPos - 1));
      Description := Trim(Copy(Line, SeparatorPos + 1, Length(Line)));
    end
    else
    begin
      Keyword := Line;
      Description := '';
    end;

    if Keyword <> '' then
      RegisterKeyword(Keyword, TokenType, Description);
  end;
end;

function TKeywordRegistry.UnregisterKeyword(const Keyword: string): Boolean;
var
  UpperKeyword: string;
  KeywordInfo: TKeywordInfo;
  Category: TKeywordCategory;
begin
  UpperKeyword := UpperCase(Keyword);
  KeywordInfo := GetKeywordInfo(UpperKeyword);

  Result := Assigned(KeywordInfo);
  if Result then
  begin
    // Remove from category
    if KeywordInfo.Category <> '' then
    begin
      Category := GetCategory(KeywordInfo.Category);
      if Assigned(Category) then
        Category.RemoveKeyword(UpperKeyword);
    end;

    // Remove from main hash table (this will free the KeywordInfo)
    FKeywords.Delete(FKeywords.FindIndexOf(UpperKeyword));

    // TODO: Remove from trie (complex operation)
    // For now, we'll rebuild the trie if needed

    DoKeywordRemoved(Keyword, KeywordInfo);

    //WriteLn('DEBUG UnregisterKeyword: Removed "', UpperKeyword, '"');
  end;
end;

function TKeywordRegistry.UnregisterKeywordsByCategory(const Category: string): Integer;
var
  CategoryObj: TKeywordCategory;
  i: Integer;
  Keyword: string;
begin
  Result := 0;
  CategoryObj := GetCategory(Category);

  if Assigned(CategoryObj) then
  begin
    // Remove all keywords in this category (reverse iteration)
    for i := CategoryObj.Keywords.Count - 1 downto 0 do
    begin
      Keyword := CategoryObj.Keywords[i];
      if UnregisterKeyword(Keyword) then
        Inc(Result);
    end;
  end;
end;

procedure TKeywordRegistry.Clear;
begin
  FKeywords.Clear; // This frees all KeywordInfo objects
  FCategories.Clear; // This frees all Category objects
  FTrieRoot.Free;
  FTrieRoot := TKeywordTrieNode.Create;

  FMinKeywordLength := MaxInt;
  FMaxKeywordLength := 0;

  // Reset statistics
  FTotalRegistrations := 0;

  //WriteLn('DEBUG TKeywordRegistry: Cleared all keywords');
end;

// === BASIC LOOKUP METHODS (OTTIMIZZATI) ===

function TKeywordRegistry.IsKeyword(const Text: string): Boolean;
var
  UpperText: string;
begin
  Inc(FTotalLookups);

  if FCaseSensitive then
    UpperText := Text
  else
    UpperText := UpperCase(Text);

  // Direct O(1) lookup - fastest possible
  Result := FKeywords.Find(UpperText) <> nil;
end;

function TKeywordRegistry.GetKeywordInfo(const Text: string): TKeywordInfo;
var
  UpperText: string;
begin
  Inc(FTotalLookups);

  if FCaseSensitive then
    UpperText := Text
  else
    UpperText := UpperCase(Text);

  // Direct O(1) lookup
  Result := TKeywordInfo(FKeywords.Find(UpperText));
end;

function TKeywordRegistry.GetKeywordType(const Text: string): TTokenType;
var
  KeywordInfo: TKeywordInfo;
begin
  KeywordInfo := GetKeywordInfo(Text);
  if Assigned(KeywordInfo) then
    Result := KeywordInfo.TokenType
  else
    Result := ttIdentifier; // Default for non-keywords
end;

function TKeywordRegistry.TryGetKeywordInfo(const Text: string; out KeywordInfo: TKeywordInfo): Boolean;
begin
  KeywordInfo := GetKeywordInfo(Text);
  Result := Assigned(KeywordInfo);
end;

// === TRIE MANAGEMENT ===

procedure TKeywordRegistry.BuildTrie;
var
  i: Integer;
  KeywordInfo: TKeywordInfo;
begin
  // Clear existing trie
  FTrieRoot.Free;
  FTrieRoot := TKeywordTrieNode.Create;

  // Rebuild from all keywords
  for i := 0 to FKeywords.Count - 1 do
  begin
    KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
    AddToTrie(KeywordInfo.Keyword, KeywordInfo);
  end;

  //WriteLn('DEBUG BuildTrie: Rebuilt trie with ', FKeywords.Count, ' keywords');
end;

procedure TKeywordRegistry.AddToTrie(const Keyword: string; KeywordInfo: TKeywordInfo);
var
  i: Integer;
  CurrentNode: TKeywordTrieNode;
  Ch: Char;
begin
  CurrentNode := FTrieRoot;

  // Defensive logging to trace potential corruption during trie insertion
  try
    //WriteLn(Format('Registering keyword to trie: %s (len=%d)', [Keyword, Length(Keyword)]));
    for i := 1 to System.Length(Keyword) do
    begin
      Ch := Keyword[i];
      // Normalize case if needed
      if not FCaseSensitive then
        Ch := UpCase(Ch);
      //WriteLn(Format('  Insert char %d/%d: %s ord=%d', [i, Length(Keyword), Ch, Ord(Ch)]));
      CurrentNode := CurrentNode.AddChild(Ch);
      if not Assigned(CurrentNode) then
        raise EKeywordRegistryException.CreateFmt('Failed to create trie node for char ord=%d in keyword %s', [Ord(Ch), Keyword]);
    end;

    CurrentNode.IsEndOfWord := True;
    CurrentNode.KeywordInfo := KeywordInfo;
  except
    on E: Exception do
    begin
      WriteLn(Format('ERROR: Exception while adding keyword to trie: %s -> %s', [Keyword, E.ClassName + ': ' + E.Message]));
      // Attempt to provide additional diagnostics
      if Assigned(CurrentNode) then
        WriteLn(Format('  Current node depth: %d children=%d', [CurrentNode.Depth, CurrentNode.GetChildrenCount]));
      raise;
    end;
  end;
end;

function TKeywordRegistry.SearchTrie(const Text: string; AllowPartial: Boolean): TKeywordMatchArray;
var
  i: Integer;
  CurrentNode: TKeywordTrieNode;
  Ch: Char;
  MatchCount: Integer;
  TextLen: Integer;

  procedure AddMatch(Node: TKeywordTrieNode; MatchLen: Integer);
  var
    Confidence: Single;
    KeywordLength: Integer;
    IsCompleteKeyword: Boolean;
    IsCompleteText: Boolean;
  begin
    KeywordLength := System.Length(Node.KeywordInfo.Keyword);
    IsCompleteKeyword := (MatchLen = KeywordLength);
    IsCompleteText := (MatchLen = TextLen);

    if MatchCount >= Length(Result) then
      SetLength(Result, Max(4, Length(Result) * 2));

    // Calculate confidence based on match completeness
    if (MatchLen = System.Length(Node.KeywordInfo.Keyword)) AND (MatchLen = TextLen) then
      Confidence := 1.0 // Exact match
    else
      Confidence := MatchLen / System.Length(Node.KeywordInfo.Keyword); // Partial match

    Result[MatchCount].Found := True;
    Result[MatchCount].KeywordInfo := Node.KeywordInfo;
    Result[MatchCount].MatchedLength := MatchLen;
    Result[MatchCount].RemainingText := Copy(Text, MatchLen + 1, TextLen - MatchLen);
    Result[MatchCount].Confidence := Confidence;

    if IsCompleteKeyword and IsCompleteText then
      Result[MatchCount].MatchType := mtExact   // "NOT" matches "NOT"
    else if IsCompleteKeyword then
      Result[MatchCount].MatchType := mtPrefix  // "NOT" found in "NOTX"
    else
      Result[MatchCount].MatchType := mtPartial; // Partial keyword match (rare)

    Inc(MatchCount);
  end;

begin
  SetLength(Result, 0);
  MatchCount := 0;
  TextLen := System.Length(Text);

  if TextLen = 0 then
    Exit;

  CurrentNode := FTrieRoot;

  // Traverse the trie following the input text
  for i := 1 to TextLen do
  begin
    Ch := Text[i];
    if not FCaseSensitive then
      Ch := UpCase(Ch);

    CurrentNode := CurrentNode.GetChild(Ch);
    if not Assigned(CurrentNode) then
      Break; // No more matches possible

    // Check if this is a complete keyword
    if CurrentNode.IsEndOfWord then
      AddMatch(CurrentNode, i);

    // If partial matches not allowed and we found a complete match, stop
    if not AllowPartial and CurrentNode.IsEndOfWord then
      Break;
  end;

  SetLength(Result, MatchCount);
end;

// === ADVANCED SEARCH METHODS (OTTIMIZZATI) ===

function TKeywordRegistry.FindKeyword(const Text: string; Options: TKeywordSearchOptions): TKeywordMatchResult;
var
  Matches: TKeywordMatchArray;
begin
  // Initialize result
  Result.Found := False;
  Result.KeywordInfo := nil;
  Result.MatchedLength := 0;
  Result.RemainingText := Text;
  Result.MatchType := mtNone;
  Result.Confidence := 0.0;

  Matches := FindKeywords(Text, Options);

  if Length(Matches) > 0 then
  begin
    // Return the best match (highest confidence)
    Result := Matches[0];
  end;
end;

function TKeywordRegistry.FindKeywords(const Text: string; Options: TKeywordSearchOptions): TKeywordMatchArray;
var
  DirectResult: TKeywordMatchResult;
  TrieMatches: TKeywordMatchArray;
begin
  Inc(FTotalLookups);


  // Try direct O(1) lookup if possible
  if ShouldUseDirectLookup(Text, Options) then
  begin
    DirectResult := PerformDirectLookup(Text);
    if DirectResult.Found then
    begin
      SetLength(Result, 1);
      Result[0] := DirectResult;
    end
    else
    begin
      SetLength(Result, 0);
    end;
    Exit;
  end;

  // Use trie search for complex cases
  TrieMatches := SearchTrie(Text, Options.AllowPartialMatch);

  // Apply remaining filters
  Result := ApplySearchFilters(TrieMatches, Options);
end;

function TKeywordRegistry.FindKeywordsByCategory(const Category: string): TKeywordInfoArray;
var
  CategoryObj: TKeywordCategory;
  i, ResultCount: Integer;
  KeywordInfo: TKeywordInfo;
begin
  SetLength(Result, 0);
  ResultCount := 0;

  CategoryObj := GetCategory(Category);
  if not Assigned(CategoryObj) then
    Exit;

  SetLength(Result, CategoryObj.Count);

  for i := 0 to CategoryObj.Keywords.Count - 1 do
  begin
    KeywordInfo := GetKeywordInfo(CategoryObj.Keywords[i]);
    if Assigned(KeywordInfo) then
    begin
      Result[ResultCount] := KeywordInfo;
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

function TKeywordRegistry.FindKeywordsByType(TokenType: TTokenType): TKeywordInfoArray;
var
  i, ResultCount: Integer;
  KeywordInfo: TKeywordInfo;
begin
  SetLength(Result, 0);
  ResultCount := 0;

  for i := 0 to FKeywords.Count - 1 do
  begin
    KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
    if KeywordInfo.TokenType = TokenType then
    begin
      if ResultCount >= Length(Result) then
        SetLength(Result, Max(16, Length(Result) * 2));

      Result[ResultCount] := KeywordInfo;
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

function TKeywordRegistry.FindKeywordsByPattern(const Pattern: string; UseRegex: Boolean): TKeywordInfoArray;
var
  i, ResultCount: Integer;
  KeywordInfo: TKeywordInfo;
  RegEx: TRegExpr;
  PatternUpper: string;
begin
  SetLength(Result, 0);
  ResultCount := 0;

  if UseRegex then
  begin
    RegEx := TRegExpr.Create;
    try
      RegEx.Expression := Pattern;
      RegEx.ModifierI := not FCaseSensitive; // Case insensitive if not case sensitive

      for i := 0 to FKeywords.Count - 1 do
      begin
        KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
        if RegEx.Exec(KeywordInfo.Keyword) then
        begin
          if ResultCount >= Length(Result) then
            SetLength(Result, Max(16, Length(Result) * 2));

          Result[ResultCount] := KeywordInfo;
          Inc(ResultCount);
        end;
      end;
    finally
      RegEx.Free;
    end;
  end
  else
  begin
    // Simple wildcard matching
    PatternUpper := UpperCase(Pattern);

    for i := 0 to FKeywords.Count - 1 do
    begin
      KeywordInfo := TKeywordInfo(FKeywords.Items[i]);

      if (Pattern = '*') or
         (Pos('*', Pattern) = 0) and (KeywordInfo.Keyword = PatternUpper) or
         (Pos('*', Pattern) > 0) and MatchesMask(KeywordInfo.Keyword, PatternUpper) then
      begin
        if ResultCount >= Length(Result) then
          SetLength(Result, Max(16, Length(Result) * 2));

        Result[ResultCount] := KeywordInfo;
        Inc(ResultCount);
      end;
    end;
  end;

  SetLength(Result, ResultCount);
end;

// === PREFIX MATCHING ===

function TKeywordRegistry.HasKeywordWithPrefix(const Prefix: string): Boolean;
var
  i: Integer;
  CurrentNode: TKeywordTrieNode;
  Ch: Char;
  UpperPrefix: string;
begin
  Inc(FTotalPrefixSearches);

  Result := False;

  if System.Length(Prefix) = 0 then
    Exit;

  if FCaseSensitive then
    UpperPrefix := Prefix
  else
    UpperPrefix := UpperCase(Prefix);

  CurrentNode := FTrieRoot;

  // Navigate trie following prefix
  for i := 1 to System.Length(UpperPrefix) do
  begin
    Ch := UpperPrefix[i];
    CurrentNode := CurrentNode.GetChild(Ch);

    if not Assigned(CurrentNode) then
      Exit; // No keywords with this prefix
  end;

  // If we got here, the prefix exists in the trie
  // Check if there are children (keywords that start with this prefix)
  Result := CurrentNode.HasChildren or CurrentNode.IsEndOfWord;

  //WriteLn('DEBUG HasKeywordWithPrefix: "', Prefix, '" -> ', Result);
end;

function TKeywordRegistry.FindKeywordsWithPrefix(const Prefix: string; MaxResults: Integer): TKeywordInfoArray;
var
  Matches: TKeywordMatchArray;
  i, ResultCount: Integer;
begin
  SetLength(Result, 0);

  if System.Length(Prefix) = 0 then
    Exit;

  Matches := SearchTrie(Prefix, True); // Allow partial matches
  ResultCount := 0;

  if MaxResults <= 0 then
    MaxResults := Length(Matches)
  else
    MaxResults := Min(MaxResults, Length(Matches));

  SetLength(Result, MaxResults);

  for i := 0 to Length(Matches) - 1 do
  begin
    if ResultCount >= MaxResults then
      Break;

    if Assigned(Matches[i].KeywordInfo) then
    begin
      Result[ResultCount] := Matches[i].KeywordInfo;
      Inc(ResultCount);
    end;
  end;

  SetLength(Result, ResultCount);
end;

function TKeywordRegistry.FindLongestKeywordMatch(const Text: string; out KeywordInfo: TKeywordInfo;
                                                out RemainingText: string): Boolean;
var
  Matches: TKeywordMatchArray;
  i, LongestIndex: Integer;
  LongestLength: Integer;
begin
  Result := False;
  KeywordInfo := nil;
  RemainingText := Text;

  if System.Length(Text) = 0 then
    Exit;

  Matches := SearchTrie(Text, True);

  if Length(Matches) = 0 then
    Exit;

  // Find the longest match
  LongestLength := 0;
  LongestIndex := -1;

  for i := 0 to Length(Matches) - 1 do
  begin
    if Matches[i].MatchedLength > LongestLength then
    begin
      LongestLength := Matches[i].MatchedLength;
      LongestIndex := i;
    end;
  end;

  if LongestIndex >= 0 then
  begin
    Result := True;
    KeywordInfo := Matches[LongestIndex].KeywordInfo;
    RemainingText := Matches[LongestIndex].RemainingText;

    //WriteLn('DEBUG FindLongestKeywordMatch: Found "', KeywordInfo.Keyword,
    //       '" in "', Text, '", remaining: "', RemainingText, '"');
  end;
end;

function TKeywordRegistry.FindBestKeywordMatch(const Text: string; MinLength: Integer): TKeywordMatchResult;
var
  Matches: TKeywordMatchArray;
  i, BestIndex: Integer;
  BestConfidence: Single;
begin
  // Initialize result
  Result.Found := False;
  Result.KeywordInfo := nil;
  Result.MatchedLength := 0;
  Result.RemainingText := Text;
  Result.MatchType := mtNone;
  Result.Confidence := 0.0;

  if System.Length(Text) < MinLength then
    Exit;

  Matches := SearchTrie(Text, True);

  if Length(Matches) = 0 then
    Exit;

  // Find the match with highest confidence
  BestConfidence := 0.0;
  BestIndex := -1;

  for i := 0 to Length(Matches) - 1 do
  begin
    if (Matches[i].MatchedLength >= MinLength) and
       (Matches[i].Confidence > BestConfidence) then
    begin
      BestConfidence := Matches[i].Confidence;
      BestIndex := i;
    end;
  end;

  if BestIndex >= 0 then
    Result := Matches[BestIndex];
end;

// === CATEGORY MANAGEMENT ===

function TKeywordRegistry.AddCategory(const Name, Description: string; IsSystem: Boolean): TKeywordCategory;
begin
  if HasCategory(Name) then
    raise EKeywordDuplicateException.CreateFmt('Category "%s" already exists', [Name]);

  Result := TKeywordCategory.Create(Name, Description, IsSystem);
  FCategories.Add(UpperCase(Name), Result);

  DoCategoryAdded(Result);

  //WriteLn('DEBUG AddCategory: Added "', Name, '"');
end;

function TKeywordRegistry.GetCategory(const Name: string): TKeywordCategory;
begin
  Result := TKeywordCategory(FCategories.Find(UpperCase(Name)));
end;

function TKeywordRegistry.HasCategory(const Name: string): Boolean;
begin
  Result := Assigned(GetCategory(Name));
end;

function TKeywordRegistry.RemoveCategory(const Name: string): Boolean;
var
  Category: TKeywordCategory;
  Index: Integer;
begin
  Category := GetCategory(Name);
  Result := Assigned(Category);

  if Result then
  begin
    // Don't allow removal of system categories
    if Category.IsSystem then
      raise EKeywordRegistryException.CreateFmt('Cannot remove system category: %s', [Name]);

    Index := FCategories.FindIndexOf(UpperCase(Name));
    if Index >= 0 then
      FCategories.Delete(Index); // This will free the category object

    //WriteLn('DEBUG RemoveCategory: Removed "', Name, '"');
  end;
end;

function TKeywordRegistry.GetCategoryNames: TStringList;
var
  i: Integer;
  Category: TKeywordCategory;
begin
  Result := TStringList.Create;

  for i := 0 to FCategories.Count - 1 do
  begin
    Category := TKeywordCategory(FCategories.Items[i]);
    Result.Add(Category.Name);
  end;

  Result.Sort;
end;

function TKeywordRegistry.GetKeywordsByCategory(const Category: string): TStringList;
var
  CategoryObj: TKeywordCategory;
begin
  Result := TStringList.Create;

  CategoryObj := GetCategory(Category);
  if Assigned(CategoryObj) then
    Result.Assign(CategoryObj.Keywords);
end;

// === CONSTRAINT MANAGEMENT ===

procedure TKeywordRegistry.RegisterConstraint(const Keyword, RequiredNext: string;
                                            Optional: Boolean; const ErrorMsg: string);
var
  Constraint: TKeywordConstraint;
  UpperKeyword: string;
  FinalErrorMsg: string;
begin
  UpperKeyword := UpperCase(Keyword);

  // Verify that keyword exists
  if not IsKeyword(UpperKeyword) then
  begin
    WriteLn('WARNING: Constraint registered for non-existent keyword: ', UpperKeyword);
    Exit;
  end;

  // Prepare error message
  if ErrorMsg <> '' then
    FinalErrorMsg := ErrorMsg
  else
    FinalErrorMsg := Format('%s must be followed by %s', [UpperKeyword, RequiredNext]);

  // Create constraint as object
  Constraint := TKeywordConstraint.Create(UpperKeyword, UpperCase(RequiredNext),
                                         Optional, FinalErrorMsg);

  // Add to hash table O(1)
  FConstraints.Add(UpperKeyword, Constraint);

  //WriteLn('DEBUG RegisterConstraint: ', UpperKeyword, ' requires ', RequiredNext);
end;

function TKeywordRegistry.HasConstraint(const Keyword: string): Boolean;
begin
  Result := FConstraints.Find(UpperCase(Keyword)) <> nil; // O(1)!
end;

function TKeywordRegistry.GetConstraint(const Keyword: string): TKeywordConstraint;
begin
  Result := TKeywordConstraint(FConstraints.Find(UpperCase(Keyword)));
end;

function TKeywordRegistry.ValidateConstraint(const Keyword, NextToken: string): Boolean;
var
  Constraint: TKeywordConstraint;
  UpperNextToken: string;
begin
  Result := True; // Default: no constraint violated

  Constraint := GetConstraint(Keyword); // Get the class object
  if not Assigned(Constraint) then
    Exit; // No constraint to validate

  UpperNextToken := UpperCase(NextToken);

  // Handle special patterns
  if Constraint.RequiredNext = 'FN*' then
  begin
    // DEF requires that the next token starts with FN
    Result := (Length(UpperNextToken) > 2) and
              (Copy(UpperNextToken, 1, 2) = 'FN') and
              (UpperNextToken[3] in ['A'..'Z', '0'..'9']);
  end
  else
  begin
    // Exact match
    Result := UpperNextToken = Constraint.RequiredNext;
  end;

  //if not Result and not Constraint.Optional then
  //begin
  //  WriteLn('CONSTRAINT ERROR: ', Constraint.ErrorMessage);
  //  WriteLn('  Expected: ', Constraint.RequiredNext, ', Got: ', NextToken);
  //end;
end;

procedure TKeywordRegistry.ClearConstraints;
begin
  FConstraints.Clear;
  //WriteLn('DEBUG: All constraints cleared');
end;

// === ANALYSIS & STATISTICS ===

function TKeywordRegistry.GetKeywordCount: Integer;
begin
  Result := FKeywords.Count;
end;

function TKeywordRegistry.GetCategoryCount: Integer;
begin
  Result := FCategories.Count;
end;

function TKeywordRegistry.GetAverageKeywordLength: Double;
var
  i: Integer;
  TotalLength: Integer;
  KeywordInfo: TKeywordInfo;
begin
  Result := 0.0;

  if FKeywords.Count = 0 then
    Exit;

  TotalLength := 0;

  for i := 0 to FKeywords.Count - 1 do
  begin
    KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
    TotalLength := TotalLength + System.Length(KeywordInfo.Keyword);
  end;

  Result := TotalLength / FKeywords.Count;
end;

function TKeywordRegistry.GetKeywordLengthDistribution: TStringList;
var
  i: Integer;
  KeywordInfo: TKeywordInfo;
  LengthStr: string;
  Index: Integer;
  KeywordLength: Integer;
  ACount: PtrInt;
begin
  Result := TStringList.Create;
  Result.Sorted := True;

  for i := 0 to FKeywords.Count - 1 do
  begin
    KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
    KeywordLength := System.Length(KeywordInfo.Keyword);
    LengthStr := IntToStr(KeywordLength);

    Index := Result.IndexOf(LengthStr);
    if Index >= 0 then
    begin
      ACount := PtrInt(Result.Objects[Index]) + 1;
      Result.Objects[Index] := TObject(ACount);
    end
    else
      Result.AddObject(LengthStr, TObject(1));
  end;
end;

function TKeywordRegistry.GetTypeDistribution: TStringList;
var
  i: Integer;
  KeywordInfo: TKeywordInfo;
  TypeName: string;
  Index: Integer;
  ACount: PtrInt;
begin
  Result := TStringList.Create;
  Result.Sorted := True;

  for i := 0 to FKeywords.Count - 1 do
  begin
    KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
    TypeName := GetTokenTypeName(KeywordInfo.TokenType);

    Index := Result.IndexOf(TypeName);
    if Index >= 0 then
    begin
      ACount := PtrInt(Result.Objects[Index]) + 1;
      Result.Objects[Index] := TObject(ACount);
    end
    else
      Result.AddObject(TypeName, TObject(1));
  end;
end;

function TKeywordRegistry.GetCategoryDistribution: TStringList;
var
  i: Integer;
  KeywordInfo: TKeywordInfo;
  Category: string;
  Index: Integer;
  ACount: PtrInt;
begin
  Result := TStringList.Create;
  Result.Sorted := True;

  for i := 0 to FKeywords.Count - 1 do
  begin
    KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
    Category := KeywordInfo.Category;

    if Category = '' then
      Category := '(uncategorized)';

    Index := Result.IndexOf(Category);
    if Index >= 0 then
    begin
      ACount := PtrInt(Result.Objects[Index]) + 1;
      Result.Objects[Index] := TObject(ACount);
    end
    else
      Result.AddObject(Category, TObject(1));
  end;
end;

function TKeywordRegistry.GetStatisticsReport: string;
var
  TypeDist, CategoryDist, LengthDist: TStringList;
  i: Integer;
  Report: TStringList;
begin
  Report := TStringList.Create;
  try
    Report.Add('=== KEYWORD REGISTRY STATISTICS ===');
    Report.Add(Format('Total Keywords: %d', [GetKeywordCount]));
    Report.Add(Format('Categories: %d', [GetCategoryCount]));
    Report.Add(Format('Length Range: %d - %d', [FMinKeywordLength, FMaxKeywordLength]));
    Report.Add(Format('Average Length: %.2f', [GetAverageKeywordLength]));
    Report.Add('');

    Report.Add('Performance Statistics:');
    Report.Add(Format('  Total Registrations: %d', [FTotalRegistrations]));
    Report.Add(Format('  Total Lookups: %d', [FTotalLookups]));
    Report.Add(Format('  Prefix Searches: %d', [FTotalPrefixSearches]));
    Report.Add('');

    // Type distribution
    TypeDist := GetTypeDistribution;
    try
      Report.Add('Token Type Distribution:');
      for i := 0 to TypeDist.Count - 1 do
        Report.Add(Format('  %s: %d', [TypeDist[i], PtrInt(TypeDist.Objects[i])]));
      Report.Add('');
    finally
      TypeDist.Free;
    end;

    // Category distribution
    CategoryDist := GetCategoryDistribution;
    try
      Report.Add('Category Distribution:');
      for i := 0 to CategoryDist.Count - 1 do
        Report.Add(Format('  %s: %d', [CategoryDist[i], PtrInt(CategoryDist.Objects[i])]));
      Report.Add('');
    finally
      CategoryDist.Free;
    end;

    // Length distribution
    LengthDist := GetKeywordLengthDistribution;
    try
      Report.Add('Length Distribution:');
      for i := 0 to LengthDist.Count - 1 do
        Report.Add(Format('  %s chars: %d keywords', [LengthDist[i], PtrInt(LengthDist.Objects[i])]));
    finally
      LengthDist.Free;
    end;

    Result := Report.Text;
  finally
    Report.Free;
  end;
end;

// === OPTIMIZATION ===

procedure TKeywordRegistry.OptimizeForSpeed;
begin
 BuildTrie;
 //WriteLn('DEBUG OptimizeForSpeed: Rebuilt structures');
end;

// === SERIALIZATION ===

procedure TKeywordRegistry.SaveToFile(const FileName: string);
var
 FileStream: TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 try
   SaveToStream(FileStream);
 finally
   FileStream.Free;
 end;
end;

procedure TKeywordRegistry.LoadFromFile(const FileName: string);
var
 FileStream: TFileStream;
begin
 if not FileExists(FileName) then
   raise EKeywordRegistryException.CreateFmt('File not found: %s', [FileName]);

 FileStream := TFileStream.Create(FileName, fmOpenRead);
 try
   LoadFromStream(FileStream);
 finally
   FileStream.Free;
 end;
end;

procedure TKeywordRegistry.SaveToStream(Stream: TStream);
var
 Writer: TWriter;
begin
 Writer := TWriter.Create(Stream, 1024);
 try
   Writer.WriteInteger(GetKeywordCount);
   Writer.WriteBoolean(FCaseSensitive);
   Writer.WriteBoolean(FAllowDuplicates);
   // TODO: Write all keywords
 finally
   Writer.Free;
 end;
end;

procedure TKeywordRegistry.LoadFromStream(Stream: TStream);
var
 Reader: TReader;
 LoadedKeywordCount: Integer;
begin
 Reader := TReader.Create(Stream, 1024);
 try
   Clear;
   LoadedKeywordCount := Reader.ReadInteger;
   FCaseSensitive := Reader.ReadBoolean;
   FAllowDuplicates := Reader.ReadBoolean;
   // TODO: Read all keywords
   //WriteLn('DEBUG LoadFromStream: Loaded ', LoadedKeywordCount, ' keywords (stub)');
 finally
   Reader.Free;
 end;
end;

// === UTILITY METHODS ===

function TKeywordRegistry.Clone: TKeywordRegistry;
begin
 Result := TKeywordRegistry.Create(FCaseSensitive, FAllowDuplicates);
 Result.Assign(Self);
end;

procedure TKeywordRegistry.Assign(Source: TKeywordRegistry);
begin
 if not Assigned(Source) then
   Exit;

 Clear;
 FCaseSensitive := Source.FCaseSensitive;
 FAllowDuplicates := Source.FAllowDuplicates;
 FAutoCategories := Source.FAutoCategories;

 // TODO: Copy all keywords and categories
 //WriteLn('DEBUG Assign: Partial implementation');
end;

procedure TKeywordRegistry.Merge(Other: TKeywordRegistry; AllowConflicts: Boolean);
begin
 if not Assigned(Other) then
   Exit;

 // TODO: Implement merge logic
 //WriteLn('DEBUG Merge: Not implemented yet');
end;

function TKeywordRegistry.IsEqual(Other: TKeywordRegistry): Boolean;
begin
 Result := False;
 if not Assigned(Other) then
   Exit;

 Result := (GetKeywordCount = Other.GetKeywordCount) and
           (FCaseSensitive = Other.FCaseSensitive);
 // TODO: Deep comparison
end;

function TKeywordRegistry.GetChecksum: string;
begin
 Result := Format('CHECKSUM_%d_%d', [GetKeywordCount, FTotalRegistrations]);
end;

// === DEBUG & DIAGNOSTICS ===

function TKeywordRegistry.GetDebugInfo: string;
begin
 Result := Format('TKeywordRegistry Debug Info:' + LineEnding +
                 '  Keywords: %d' + LineEnding +
                 '  Categories: %d' + LineEnding +
                 '  Case Sensitive: %s' + LineEnding +
                 '  Total Lookups: %d',
                 [GetKeywordCount, GetCategoryCount,
                  BoolToStr(FCaseSensitive, True), FTotalLookups]);
end;

procedure TKeywordRegistry.DumpToConsole;
var
 i: Integer;
 KeywordInfo: TKeywordInfo;
begin
 WriteLn('=== KEYWORD REGISTRY DUMP ===');
 WriteLn('Total Keywords: ', GetKeywordCount);

 for i := 0 to FKeywords.Count - 1 do
 begin
   KeywordInfo := TKeywordInfo(FKeywords.Items[i]);
   if Assigned(KeywordInfo) then
     WriteLn(Format('  [%d] %s -> %s', [i, KeywordInfo.Keyword,
                    GetTokenTypeName(KeywordInfo.TokenType)]));
 end;
end;

procedure TKeywordRegistry.DumpTrieToConsole;
begin
 WriteLn('=== TRIE STRUCTURE DUMP ===');
 WriteLn('Root node children: ', FTrieRoot.GetChildrenCount);
 // TODO: Recursive trie dump
end;

// === BASIC KEYWORDS REGISTRATION ===

procedure TKeywordRegistry.RegisterBasicKeywords;
begin
 //WriteLn('DEBUG RegisterBasicKeywords: Starting registration...');

  // === WHITESPACE AND CONTROL CHARS ===
  RegisterKeyword(' ', ttWhitespace, 'Whitespace',      kcWhitespaceAndCtrlChar);
  RegisterKeyword(#9,  ttTabulation, 'Tabulation',      kcWhitespaceAndCtrlChar);
  RegisterKeyword(#10, ttEndOfLine,  'Line feed',       kcWhitespaceAndCtrlChar);
  RegisterKeyword(#13, ttEndOfLine,  'Carriage return', kcWhitespaceAndCtrlChar);
  RegisterKeyword(#0,  ttEndOfFile,  'End of file',     kcWhitespaceAndCtrlChar);

  // === OPERATORS ===
  RegisterKeyword('+',  ttOpAdd, 'Add operator',                   kcOperators);
  RegisterKeyword('-',  ttOpSub, 'Subtract operator',              kcOperators);
  RegisterKeyword('*',  ttOpMul, 'Multiply operator',              kcOperators);
  RegisterKeyword('/',  ttOpDiv, 'Divide operator',                kcOperators);
  RegisterKeyword('^',  ttOpPow, 'Power operator',                 kcOperators);
  RegisterKeyword(kMOD, ttOpMod, 'Modulo operator',                kcOperators);
  RegisterKeyword('=',  ttOpEq,  'Equal operator',                 kcOperators);
  RegisterKeyword('<',  ttOpLt,  'Lesser than operator',           kcOperators);
  RegisterKeyword('>',  ttOpGt,  'Greater than operator',          kcOperators);
  RegisterKeyword('<=', ttOpLe,  'Lesser than or equal operator',  kcOperators);
  RegisterKeyword('>=', ttOpGe,  'Greater than or equal operator', kcOperators);
  RegisterKeyword('<>', ttOpNeq, 'Not equal operator',             kcOperators);

  // === DELIMITERS ===
  RegisterKeyword('(', ttDelimParOpen,    'Opening parenthesis delimiter',   kcDelimiters);
  RegisterKeyword(')', ttDelimParClose,   'Closing parenthesis delimiter',   kcDelimiters);
  RegisterKeyword('[', ttDelimBrackOpen,  'Opening square braket delimiter', kcDelimiters);
  RegisterKeyword(']', ttDelimBrackClose, 'Closing square braket delimiter', kcDelimiters);

  // === SEPARATORS ===
  RegisterKeyword(',', ttSeparParam);
  RegisterKeyword(';', ttSeparOutput);
  RegisterKeyword(':', ttSeparStmt);

 // === BITWISE OPERATORS ===
 RegisterKeyword(kAND, ttBitwiseAND, 'AND operator', kcBitwiseOperators);
 RegisterKeyword(kNOT, ttBitwiseNOT, 'NOT operator', kcBitwiseOperators);
 RegisterKeyword(kOR,  ttBitwiseOR,  'OR operator',  kcBitwiseOperators);
 RegisterKeyword(kXOR, ttBitwiseXOR, 'XOR operator', kcBitwiseOperators);
 // Operatori avanzati (opzionali)
 //RegisterKeyword('NAND', ttBitwiseNAND, 'NAND operator', kcBitwiseOperators);
 //RegisterKeyword('NOR',  ttBitwiseNOR,  'NOR operator',  kcBitwiseOperators);
 //RegisterKeyword('XNOR', ttBitwiseXNOR, 'XNOR operator', kcBitwiseOperators);

 // === FLOW CONTROL - CONDITIONAL ===
 RegisterKeyword(kELSE, ttConditionalElse, 'ELSE statement', kcConditionalFlowControl);
 RegisterKeyword(kIF,   ttConditionalIf,   'IF statement',   kcConditionalFlowControl);
 RegisterKeyword(kTHEN, ttConditionalThen, 'THEN statement', kcConditionalFlowControl);

 // === FLOW CONTROL - JUMP RELATED STATEMENTS ===
 RegisterKeyword(kEXIT,   ttJumpReturn,      'Exit statement',   kcJumpFlowControl);
 RegisterKeyword(kGOSUB,  ttJumpGosub,       'Gosub statement',  kcJumpFlowControl);
 RegisterKeyword(kGOTO,   ttJumpGoto,        'Goto statement',   kcJumpFlowControl);
 RegisterKeyword(kGO_TO,  ttJumpGoto,        'Go to statement',  kcJumpFlowControl);
 RegisterKeyword(kON,     ttJumpConditional, 'Conditional jump', kcJumpFlowControl);
 RegisterKeyword(kRETURN, ttJumpReturn,      'Return from jump', kcJumpFlowControl);

 // === FLOW CONTROL - PROGRAM EXECUTION ===
 RegisterKeyword(kCONT,  ttProgramCont,  'Continue program execution',      kcProgramFlowControl);
 RegisterKeyword(kEND,   ttProgramEnd,   'Ends program execution',          kcProgramFlowControl);
 RegisterKeyword(kFAST,  ttProgramClock, 'Set fast speed clock',            kcProgramFlowControl);
 RegisterKeyword(kRUN,   ttProgramRun,   'Execute program',                 kcProgramFlowControl);
 RegisterKeyword(kSLEEP, ttProgramSleep, 'Delay program for given seconds', kcProgramFlowControl);
 RegisterKeyword(kSLOW,  ttProgramClock, 'Set slow speed clock',            kcProgramFlowControl);
 RegisterKeyword(kSTOP,  ttProgramStop,  'Halt program execution',          kcProgramFlowControl);
 RegisterKeyword(kWAIT,  ttProgramWait,  'Pause until condition satisfied', kcProgramFlowControl);

 // === FLOW CONTROL - LOOP CONSTRUCTS ===
 RegisterKeyword(kDO,    ttLoopBlockStart, 'Starts DO/LOOP cycle',                            kcLoopConstructs);
 RegisterKeyword(kFOR,   ttLoopBlockStart, 'Starts FOR loop',                                 kcLoopConstructs);
 RegisterKeyword(kLOOP,  ttLoopBlockEnd,   'Closes DO/LOOP cycle',                            kcLoopConstructs);
 RegisterKeyword(kNEXT,  ttLoopBlockEnd,   'Closes FOR loop updating counter',                kcLoopConstructs);
 RegisterKeyword(kSTEP,  ttLoopControl,    'Sets FOR loop increment/decrement per iteration', kcLoopConstructs);
 RegisterKeyword(kTO,    ttLoopControl,    'Sets FOR loop end value',                         kcLoopConstructs);
 RegisterKeyword(kUNTIL, ttLoopControl,    'DO/LOOP until condition',                         kcLoopConstructs);
 RegisterKeyword(kWHILE, ttLoopControl,    'DO/LOOP while condition',                         kcLoopConstructs);

 // === CODE BLOCK CONSTRUCTS ===
 RegisterKeyword(kBEGIN, ttBlockBegin, 'Starts code block', kcCodeBlockConstructs);
 RegisterKeyword(kBEND,  ttBlockEnd,   'Ends code block',   kcCodeBlockConstructs);

 // === PROCEDURES HANDLING ===
 RegisterKeyword(kDEF, ttProcedureDefine, 'Define function', kcProcedures);
 RegisterKeyword(kFN,  ttProcedureStart,  'Function call',   kcProcedures);

 // === DATA HANDLING ===
 RegisterKeyword(kCLR,     ttDataClear,       'Clear all variables',  kcData);
 RegisterKeyword(kCONST,   ttConstant,        'Constant assignment',  kcData);
 RegisterKeyword(kDATA,    ttDataConstant,    'Data statement',       kcData);
 RegisterKeyword(kDIM,     ttDataDeclaration, 'Dimension arrays',     kcData);
 RegisterKeyword(kLET,     ttDataAssignment,  'Variable assignment',  kcData);
 RegisterKeyword(kREAD,    ttDataRead,        'Read data',            kcData);
 RegisterKeyword(kRESTORE, ttDataConstant,    'Restore data pointer', kcData);

 // === STANDARD INPUT/OUTPUT ===
 // input
 RegisterKeyword(kGET,    ttInputCommand,  'Get character',                          kcStdIO);
 RegisterKeyword(kGETKEY, ttInputCommand,  'Get keypress',                           kcStdIO);
 RegisterKeyword(kINPUT,  ttInputCommand,  'Input statement',                        kcStdIO);
 // output
 RegisterKeyword(kCHAR,  ttOutputCommand, 'Displays char at the specific position', kcStdIO);
 RegisterKeyword(kPRINT, ttOutputCommand, 'Print statement',                        kcStdIO);
 RegisterKeyword(kPUDEF, ttOutputCommand, 'Redefine symbols in PRINT USING',        kcStdIO);
 RegisterKeyword(kUSING, ttOutputCommand, 'Output using format',                    kcStdIO);

 // === FILE INPUT/OUTPUT ===
 // input
 RegisterKeyword(kGETN,   ttFileInputCommand,  'Get char from file', kcFileIO);
 RegisterKeyword(kINPUTN, ttFileInputCommand,  'Input from file',    kcFileIO);
 // output
 RegisterKeyword(kPRINTN, ttFileOutputCommand, 'Print on file',      kcFileIO);

 // === INPUT/OUTPUT HANDLING===
 RegisterKeyword(kCMD, ttIOCommand, 'Redirect screen output', kcIO);

 // === DOS COMMANDS ===
 RegisterKeyword(kAPPEND,    ttFileOperation,  'Appenda data to sequential file',          kcDOSCommands);
 RegisterKeyword(kBACKUP,    ttFileManagement, 'Copy disk content to another disk',        kcDOSCommands);
 RegisterKeyword(kBLOAD,     ttFileOperation,  'Load binary file',                         kcDOSCommands);
 RegisterKeyword(kBOOT,      ttFileOperation,  'Load and execute binary file',             kcDOSCommands);
 RegisterKeyword(kBSAVE,     ttFileOperation,  'Save binary file',                         kcDOSCommands);
 RegisterKeyword(kCATALOG,   ttFileManagement, 'Display drive directory',                  kcDOSCommands);
 RegisterKeyword(kCLOSE,     ttFileOperation,  'Close file',                               kcDOSCommands);
 RegisterKeyword(kCOLLECT,   ttFileManagement, 'Free inaccessible disk space',             kcDOSCommands);
 RegisterKeyword(kCONCAT,    ttFileManagement, 'Attaches source file to destination file', kcDOSCommands);
 RegisterKeyword(kCOPY,      ttFileManagement, 'Copy file from drive to drive',            kcDOSCommands);
 RegisterKeyword(kCP,        ttFileManagement, 'Copy file (alias for COPY)',               kcDOSCommands);
 RegisterKeyword(kDCLEAR,    ttFileOperation,  'Clear all open channels on disk drive',    kcDOSCommands);
 RegisterKeyword(kDCLOSE,    ttFileOperation,  'Close disk drive file(s)',                 kcDOSCommands);
 RegisterKeyword(kDIR,       ttFileManagement, 'Display drive directory (alias)',          kcDOSCommands);
 RegisterKeyword(kDIRECTORY, ttFileManagement, 'Display drive directory',                  kcDOSCommands);
 RegisterKeyword(kDLOAD,     ttFileOperation,  'Load BASIC file',                          kcDOSCommands);
 RegisterKeyword(kDOPEN,     ttFileOperation,  'Open sequential file for r/w',             kcDOSCommands);
 RegisterKeyword(kDSAVE,     ttFileOperation,  'Save BASIC file',                          kcDOSCommands);
 RegisterKeyword(kDVERIFY,   ttFileOperation,  'Verify saved BASIC file',                  kcDOSCommands);
 RegisterKeyword(kHEADER,    ttFileOperation,  'Formats a diskette',                       kcDOSCommands);
 RegisterKeyword(kLOAD,      ttFileOperation,  'Load program',                             kcDOSCommands);
 RegisterKeyword(kOPEN,      ttFileOperation,  'Open file for input/output',               kcDOSCommands);
 RegisterKeyword(kRECORD,    ttFileOperation,  'Position relative file pointer',           kcDOSCommands);
 RegisterKeyword(kRENAME,    ttFileManagement, 'Rename file on disk',                      kcDOSCommands);
 RegisterKeyword(kSAVE,      ttFileOperation,  'Save program',                             kcDOSCommands);
 RegisterKeyword(kSCRATCH,   ttFileManagement, 'Delete file from drive',                   kcDOSCommands);
 RegisterKeyword(kVERIFY,    ttFileOperation,  'Verify saved file or program',             kcDOSCommands);
 // Extended file management commands (SedaiBasic)
 RegisterKeyword(kMKDIR,     ttFileManagement, 'Create directory',                         kcDOSCommands);
 RegisterKeyword(kMD,        ttFileManagement, 'Create directory (alias for MKDIR)',       kcDOSCommands);
 RegisterKeyword(kCHDIR,     ttFileManagement, 'Change current directory',                 kcDOSCommands);
 RegisterKeyword(kCD,        ttFileManagement, 'Change directory (alias for CHDIR)',       kcDOSCommands);
 RegisterKeyword(kMOVE,      ttFileManagement, 'Move file to another location',            kcDOSCommands);
 RegisterKeyword(kMV,        ttFileManagement, 'Move file (alias for MOVE)',               kcDOSCommands);

 // === STRING FUNCTIONS ===
 RegisterKeyword(kASC,    ttStringFunction, 'Return character code',                            kcStringFunctions);
 RegisterKeyword(kCHRS,   ttStringFunction, 'Return character from code',                       kcStringFunctions);
 RegisterKeyword(kHEXS,   ttStringFunction, 'Hex number string from decimal number',            kcStringFunctions);
 RegisterKeyword(kINSTR,  ttStringFunction, 'Position of source string in destination string',  kcStringFunctions);
 RegisterKeyword(kLEN,    ttStringFunction, 'Return string length',                             kcStringFunctions);
 RegisterKeyword(kLEFTS,  ttStringFunction, 'Return string leftmost chars',                     kcStringFunctions);
 RegisterKeyword(kMIDS,   ttStringFunction, 'Return substring from larger string',              kcStringFunctions);
 RegisterKeyword(kRIGHTS, ttStringFunction, 'Return string rightmost chars',                    kcStringFunctions);
 RegisterKeyword(kSPC,    ttStringFunction, 'Skip spaces on context output',                    kcStringFunctions);
 RegisterKeyword(kSTRS,   ttStringFunction, 'Convert number to string',                         kcStringFunctions);
 RegisterKeyword(kTAB,    ttStringFunction, 'Move cursor forward string from the first column', kcStringFunctions);
 RegisterKeyword(kDEC,    ttStringFunction, 'Convert hex string to decimal integer',            kcStringFunctions);
 RegisterKeyword(kERRS,   ttStringFunction, 'Return error message for error code',               kcStringFunctions);

 // === MEMORY HANDLING (COMMANDS AND FUNCTIONS) ===
 // commands
 RegisterKeyword(kBANK,  ttMemoryCommand,  'Select RAM bank (0-15)',                     kcMemoryHandling);
 RegisterKeyword(kFETCH, ttMemoryCommand,  'Get data from expansion RAM',                kcMemoryHandling);
 RegisterKeyword(kPOKE,  ttMemoryCommand,  'Set content of specific RAM location',       kcMemoryHandling);
 RegisterKeyword(kRREG,  ttMemoryCommand,  'Read contents of accumulator and registers', kcMemoryHandling);
 RegisterKeyword(kSTASH, ttMemoryCommand,  'Move content of host RAM to expansion RAM',  kcMemoryHandling);
 RegisterKeyword(kSWAP,  ttMemoryCommand,  'Swap content of host RAM to expansion RAM',  kcMemoryHandling);
 // functions
 RegisterKeyword(kFRE,     ttMemoryFunction, 'Return RAM bytes free',                      kcMemoryHandling);
 RegisterKeyword(kPEEK,    ttMemoryFunction, 'Return content of specific RAM location',    kcMemoryHandling);
 RegisterKeyword(kPOINTER, ttMemoryFunction, 'Return the address of a variable name',      kcMemoryHandling);

 {$IFNDEF WEB_MODE}
 // === GRAPHICS HANDLING (COMMANDS AND FUNCTION) ===
 // Note: These keywords are NOT available in WEB_MODE (sbw.exe)
 // commands
 RegisterKeyword(kBOX,     ttGraphicsCommand,  'Draw a box',                                       kcGraphicsHandling);
 RegisterKeyword(kCIRCLE,  ttGraphicsCommand,  'Draws circles, ellipses, arcs and polygons',       kcGraphicsHandling);
 RegisterKeyword(kCOLOR,   ttGraphicsCommand,  'Define colors for each screen area',               kcGraphicsHandling);
 RegisterKeyword(kSETCOLOR, ttGraphicsCommand, 'Set color for screen area (0-based index)',        kcGraphicsHandling);
 RegisterKeyword(kDRAW,    ttGraphicsCommand,  'Draw dots, lines and shapes',                      kcGraphicsHandling);
 RegisterKeyword(kGLIST,   ttGraphicsCommand,  'List available SDL2 video modes',                  kcGraphicsHandling);
 RegisterKeyword(kGRAPHIC, ttGraphicsCommand,  'Select a graphic mode',                            kcGraphicsHandling);
 RegisterKeyword(kGSHAPE,  ttGraphicsCommand,  'Retrieve shape from string variable',              kcGraphicsHandling);
 RegisterKeyword(kLOCATE,  ttGraphicsCommand,  'Position the bit map pixel cursor on the screen',  kcGraphicsHandling);
 RegisterKeyword(kPAINT,   ttGraphicsCommand,  'Fill area with color',                             kcGraphicsHandling);
 RegisterKeyword(kSCALE,   ttGraphicsCommand,  'Alter scaling in graphics mode',                   kcGraphicsHandling);
 RegisterKeyword(kSCNCLR,  ttGraphicsCommand,  'Clear screen',                                     kcGraphicsHandling);
 RegisterKeyword(kPLOAD,   ttGraphicsCommand,  'Load palette from JSON file',                      kcGraphicsHandling);
 RegisterKeyword(kPSAVE,   ttGraphicsCommand,  'Save palette to JSON file',                        kcGraphicsHandling);
 RegisterKeyword(kSSHAPE,  ttGraphicsCommand,  'Save shapes to string variable',                   kcGraphicsHandling);
 RegisterKeyword(kWIDTH,   ttGraphicsCommand,  'Set the width of drawn lines',                     kcGraphicsHandling);
 RegisterKeyword(kWINDOW,  ttOutputCommand,    'Defines a text screen window',                     kcStdIO);
 // functions
 RegisterKeyword(kGETCOLOR, ttGraphicsFunction, 'Return color index for screen area (0-based)',    kcGraphicsHandling);
 RegisterKeyword(kPOS,     ttInputFunction,    'Return the current cursor column position',        kcStdIO);
 RegisterKeyword(kRCLR,    ttGraphicsFunction, 'Return color of color source',                     kcGraphicsHandling);
 RegisterKeyword(kRDOT,    ttGraphicsFunction, 'Return current position or color of pixel cursor', kcGraphicsHandling);
 RegisterKeyword(kRGR,     ttGraphicsFunction, 'Return current graphic mode',                      kcGraphicsHandling);
 RegisterKeyword(kRGBA,    ttGraphicsFunction, 'Create 32-bit RGBA color value',                   kcGraphicsHandling);
 RegisterKeyword(kRWINDOW, ttInputFunction,    'Return the size of the current text window',       kcStdIO);

 // === SPRITES HANDLING (COMMANDS AND FUNCTIONS) ===
 // Note: These keywords are NOT available in WEB_MODE (sbw.exe)
 // commands
 RegisterKeyword(kCOLLISION, ttSpriteCommand,  'Define handling for sprite collision interrupt',              kcSpritesHandling);
 RegisterKeyword(kSPRITE,    ttSpriteCommand,  'Set sprite properties',                                       kcSpritesHandling);
 RegisterKeyword(kMOVSPR,    ttSpriteCommand,  'Position or move sprite on the screen',                       kcSpritesHandling);
 RegisterKeyword(kSPRCOLOR,  ttSpriteCommand,  'Set multicolor 1 and/or multicolor 2 colors for all sprites', kcSpritesHandling);
 RegisterKeyword(kSPRDEF,    ttSpriteCommand,  'Enter the SPRite DEFinition mode',                            kcSpritesHandling);
 RegisterKeyword(kSPRSAV,    ttSpriteCommand,  'Store a sprite data from a text string or vice versa',        kcSpritesHandling);
 // functions
 RegisterKeyword(kBUMP,      ttSpriteFunction, 'Return sprite collision information',                         kcSpritesHandling);
 RegisterKeyword(kRSPCOLOR,  ttSpriteFunction, 'Return sprite multicolor values',                             kcSpritesHandling);
 RegisterKeyword(kRSPPOS,    ttSpriteFunction, 'Return the speed and position values of a sprite',            kcSpritesHandling);
 RegisterKeyword(kRSPRITE,   ttSpriteFunction, 'Return sprite characteristics',                               kcSpritesHandling);

 // === SOUND HANDLING (COMMANDS AND FUNCTIONS) ===
 // Note: These keywords are NOT available in WEB_MODE (sbw.exe)
 RegisterKeyword(kENVELOPE, ttSoundCommand, 'Define a musical instrument envelope',       kcSoundHandling);
 RegisterKeyword(kFILTER,   ttSoundCommand, 'Define sound (SID chip) filter parameters',  kcSoundHandling);
 RegisterKeyword(kPLAY,     ttSoundCommand, 'Define and play musical notes and elements', kcSoundHandling);
 RegisterKeyword(kSOUND,    ttSoundCommand, 'Outputs sound effects and musical notes',    kcSoundHandling);
 RegisterKeyword(kTEMPO,    ttSoundCommand, 'Define the speed of the song being played',  kcSoundHandling);
 RegisterKeyword(kVOL,      ttSoundCommand, 'Define output level of sound',               kcSoundHandling);
 {$ENDIF}

 {$IFDEF WEB_MODE}
 // === WEB HANDLING (COMMANDS AND FUNCTIONS) ===
 // Note: These keywords are ONLY available in WEB_MODE (sbw.exe)
 // Input functions (HTML-escaped by default for security)
 RegisterKeyword(kGETS,     ttWebFunction,  'Return HTML-escaped query string parameter',  kcWebHandling);
 RegisterKeyword(kPOSTS,    ttWebFunction,  'Return HTML-escaped POST parameter',          kcWebHandling);
 RegisterKeyword(kGETRAWS,  ttWebFunction,  'Return raw query string parameter (unsafe)',  kcWebHandling);
 RegisterKeyword(kPOSTRAWS, ttWebFunction,  'Return raw POST parameter (unsafe)',          kcWebHandling);
 // Encoding functions
 RegisterKeyword(kHTMLS,    ttWebFunction,  'Escape HTML entities in string',              kcWebHandling);
 RegisterKeyword(kURLS,     ttWebFunction,  'URL encode string',                           kcWebHandling);
 // Environment variables
 RegisterKeyword(kMETHODS,  ttWebVariable,  'Return HTTP method (GET or POST)',            kcWebHandling);
 RegisterKeyword(kPATHS,    ttWebVariable,  'Return requested path',                       kcWebHandling);
 RegisterKeyword(kQUERYS,   ttWebVariable,  'Return full query string',                    kcWebHandling);
 RegisterKeyword(kHEADERS,  ttWebFunction,  'Return HTTP request header value',            kcWebHandling);
 // Response control
 RegisterKeyword(kSETHEADER, ttWebCommand,  'Set HTTP response header',                    kcWebHandling);
 RegisterKeyword(kSTATUS,   ttWebCommand,   'Set HTTP response status code',               kcWebHandling);
 {$ENDIF}

 // === MATH FUNCTIONS ===
 RegisterKeyword(kABS,   ttMathFunction, 'Return absolute value',                                    kcMathFunctions);
 RegisterKeyword(kATN,   ttMathFunction, 'Return arctangent of argument',                            kcMathFunctions);
 RegisterKeyword(kATAN,  ttMathFunction, 'Return arctangent of argument',                            kcMathFunctions);
 RegisterKeyword(kCOS,   ttMathFunction, 'Return cosine of angle of x radians',                      kcMathFunctions);
 // DEC is registered as ttStringFunction because it takes a string argument
 RegisterKeyword(kEXP,   ttMathFunction, 'Return value of e raised to the power x',                  kcMathFunctions);
 RegisterKeyword(kINT,   ttMathFunction, 'Convert float number to integer',                          kcMathFunctions);
 RegisterKeyword(kLN,    ttMathFunction, 'Return natural log of x',                                  kcMathFunctions);
 RegisterKeyword(kLOG,   ttMathFunction, 'Return natural log of x',                                  kcMathFunctions);
 RegisterKeyword(kLOG10, ttMathFunction, 'Return base 10 log of x',                                  kcMathFunctions);
 RegisterKeyword(kLOG2,  ttMathFunction, 'Return base 2 log of x',                                   kcMathFunctions);
 RegisterKeyword(kLOGN,  ttMathFunction, 'Return base n log of x: LOGN(base, x)',                    kcMathFunctions);
 // MOD is now registered as binary operator (ttOpMod), not as function
 RegisterKeyword(kRND,   ttMathFunction, 'Return a random number from 0 (included) to 1 (excluded)', kcMathFunctions);
 RegisterKeyword(kSGN,   ttMathFunction, 'Return sign of argument',                                  kcMathFunctions);
 RegisterKeyword(kSIN,   ttMathFunction, 'Return sine of argument',                                  kcMathFunctions);
 RegisterKeyword(kSQR,   ttMathFunction, 'Return square root of argument',                           kcMathFunctions);
 RegisterKeyword(kTAN,   ttMathFunction, 'Return tangent of argument',                               kcMathFunctions);
 RegisterKeyword(kVAL,   ttMathFunction, 'Return the numeric value of a number string',              kcMathFunctions);

 // === RESERVED VARIABLES ===
 RegisterKeyword(kDS,  ttSpecialVariable, 'Get disk status code',          kcReservedVariables);
 RegisterKeyword(kDSS, ttSpecialVariable, 'Get disk status message',       kcReservedVariables);
 RegisterKeyword(kEL,  ttSpecialVariable, 'Return last error line',        kcReservedVariables);
 RegisterKeyword(kER,  ttSpecialVariable, 'Return last error code',        kcReservedVariables);
 RegisterKeyword(kST,  ttSpecialVariable, 'Get I/O status byte',           kcReservedVariables);
 RegisterKeyword(kTI,  ttSpecialVariable, 'Get time elapse from power on', kcReservedVariables);
 RegisterKeyword(kTIS, ttSpecialVariable, 'Get/set 24h clock',             kcReservedVariables);
 RegisterKeyword(kDTS, ttSpecialVariable, 'Get current date YYYYMMDD',     kcReservedVariables);

  // === ERROR HANDLING (COMMANDS AND FUNCTIONS) ===
  // commands
  RegisterKeyword(kRESUME, ttErrorHandlingCommand,  'Resume from the given line after error', kcErrorHandling);
  RegisterKeyword(kTRAP,   ttErrorHandlingCommand,  'Detect error and go to the given line',  kcErrorHandling);
  // functions - ERR$ is registered in STRING FUNCTIONS section

  // === DEBUG ===
  RegisterKeyword(kHELP,  ttDebugCommand,     'Highlight the line where the error occurred', kcDebug);
  RegisterKeyword(kTRON,  ttDebugTracingMode, 'Set tracing mode on',                         kcDebug);
  RegisterKeyword(kTROFF, ttDebugTracingMode, 'Set tracing mode off',                        kcDebug);

  // === MACHINE LANGUAGE HANDLING ===
  RegisterKeyword(kMONITOR, ttMonitor,     'Enter ML monitor',                 kcMachineLenguage);
  RegisterKeyword(kSYS,     ttSysCommand,  'Execute ML subroutine',            kcMachineLenguage);
  RegisterKeyword(kUSR,     ttUsrFunction, 'Call user-defined ML subfunction', kcMachineLenguage);

  // === PROGRAM EDITING COMMANDS ===
  RegisterKeyword(kAUTO,     ttProgramEditing, 'Auto line numbering',                   kcProgramEditing);
  RegisterKeyword(kDELETE,   ttProgramEditing, 'Delete lines of a BASIC program',       kcProgramEditing);
  RegisterKeyword(kEDIT,     ttProgramEditing, 'Edit a single program line',            kcProgramEditing);
  RegisterKeyword(kHCLEAR,   ttProgramEditing, 'Clear current history',                 kcProgramEditing);
  RegisterKeyword(kHLOAD,    ttProgramEditing, 'Load history from file',                kcProgramEditing);
  RegisterKeyword(kHSAVE,    ttProgramEditing, 'Save history to file',                  kcProgramEditing);
  RegisterKeyword(kLIST,     ttProgramEditing, 'List the BASIC program lines',          kcProgramEditing);
  RegisterKeyword(kNEW,      ttProgramEditing, 'Erase program and clear all variables', kcProgramEditing);
  RegisterKeyword(kRENUMBER, ttProgramEditing, 'Renumber lines of the BASIC program',   kcProgramEditing);

  // === COMMENTS ===
  RegisterKeyword(kREM, ttCommentRemark, 'Starts a comment or remark', kcCommentsAndRemarks);

  // === SPECIAL INPUT DEVICES ===
  RegisterKeyword(kJOY, ttInputFunction, 'Return joystick status',  kcInputDevices);
  RegisterKeyword(kPEN, ttInputFunction, 'Return light pen status', kcInputDevices);
  RegisterKeyword(kPOT, ttInputFunction, 'Return paddle status',    kcInputDevices);

  // === SYSTEM HANDLING ===
  RegisterKeyword(kKEY, ttKeyDefine, 'Define/list function key assignment', kcSystemHandling);

  // === ENVIRONMENT SETUP DIRECTIVES ===
  RegisterKeyword(kEXPNOTATION, ttDirective, 'Setup Directives', kcEnvironmentSetup);

  // === SYNTAX CONSTRAINTS ===
  RegisterConstraint(kGO_TO, 'TO',  False, 'GO must be followed by TO');
  RegisterConstraint(kDEF,   'FN*', False, 'DEF must be followed by FN+function_name');

  //WriteLn('DEBUG RegisterBasicKeywords: Completed registration of ', GetKeywordCount, ' keywords');
end;

// === FACTORY FUNCTIONS ===

function CreateKeywordRegistry(CaseSensitive: Boolean): TKeywordRegistry;
begin
Result := TKeywordRegistry.Create(CaseSensitive, False);
end;

function CreateBasicKeywordRegistry: TKeywordRegistry;
begin
Result := TKeywordRegistry.Create(False, False); // Case insensitive, no duplicates
Result.AutoCategories := True;
end;

function CreateModernKeywordRegistry: TKeywordRegistry;
begin
Result := TKeywordRegistry.Create(True, False); // Case sensitive, no duplicates
Result.AutoCategories := True;
end;

end.
