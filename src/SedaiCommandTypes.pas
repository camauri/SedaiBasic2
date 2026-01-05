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
unit SedaiCommandTypes;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$codepage UTF8}

interface

type
  // === COMMAND CLASSIFICATION ===
  TCommandType = (
    ctImmediate,      // Execute immediately (PRINT 2+3, LIST, etc.)
    ctProgramLine,    // Numbered line to store in program memory
    ctSystemCommand   // System commands (may be added later)
  );

  // === PIPE MODIFIER ===
  TPipeModifier = (
    pmNone,           // No pipe modifier
    pmMore            // | MORE - paginate output
  );

  // === INPUT PARSING RESULT ===
  TInputParseResult = record
    CommandType: TCommandType;
    LineNumber: Integer;
    Statement: string;
    IsValid: Boolean;
    ErrorMessage: string;
    PipeModifier: TPipeModifier;  // Pipe modifier (e.g., | MORE)
  end;

implementation

end.
