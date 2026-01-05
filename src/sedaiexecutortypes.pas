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
unit SedaiExecutorTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

uses
  Variants, SysUtils, fgl;

type
  // === ARRAY CONFIGURATION ===
  TArrayIndexMode = (
    aimElementCount,  // DIM A(10) = 10 elements (0..9)
    aimMaxIndex      // DIM A(10) = 11 elements (0..10) - Commodore BASIC style
  );

  // === VARIABLE TYPES ===
  TBasicVariableType = (
    bvtNumber,    // Generic number
    bvtString,    // $ - String
    bvtInteger,   // % - Integer (16 bit)
    bvtSingle,    // ! - Single precision (32 bit)
    bvtDouble,    // # - Double precision (64 bit)
    bvtArray      // () - Array
  );

  // === ARRAY TYPES ===
  TVariantDynArray = array of Variant;
  TArrayDimensions = array of Integer;

  // === DISPLAY MODES ===
  TDisplayMode = (dmConsole, dmBitmap, dmMixed);

  // === INTERRUPT TYPES ===
  TInterruptType = (itVBlank, itKeyboard, itTimer, itCustom);

  // === INTERRUPT EVENT RECORD ===
  TInterruptEvent = record
    IntType: TInterruptType;
    Data: Integer;
    Timestamp: QWord;
  end;

  // === CONSOLE CHARACTER RECORD ===
  TConsoleChar = record
    Ch: Char;
    FgR, FgG, FgB: Byte;
    BgR, BgG, BgB: Byte;
  end;

  // === EXECUTION RESULT RECORD ===
  TExecutionResultData = record
    Success: Boolean;
    ErrorMessage: string;
    ExecutionTime: Double;
    StatementsExecuted: Integer;
  end;

  // === FUNCTION TYPES ===
  TBuiltinFunction = function(const Args: array of Variant): Variant of object;

  // === STORAGE MAPS ===
  TVariableMap = specialize TFPGMap<string, Variant>;
  TFunctionMap = specialize TFPGMap<string, TBuiltinFunction>;

  // === EXECUTOR EXCEPTIONS ===
  TExecutorException = class(Exception);
  TExecutorBreakException = class(TExecutorException);
  TExecutorStopException = class(TExecutorException);

implementation

end.
