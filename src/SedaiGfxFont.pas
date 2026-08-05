{*
 * SedaiBasic - A BASIC interpreter with bytecode VM
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

{ The built-in 8x8 text font, and the one thing that stood between us and TWO separate gaps.

  DRAW STRING did not exist at all - the statement parsed in one of its two spellings and drew NOTHING,
  in silence, which is how it produced an empty logo mask during the demo work and how it kept a tick in
  BASIC.md it had not earned. And PRINT inside a graphics mode wrote nothing to the framebuffer either:
  our text model and our drawing surface were two separate things, where FreeBASIC in a graphics mode has
  one. Both are the same missing piece - somewhere to get the SHAPE of a character from - so both are
  closed here.

  WHY THE GLYPHS ARE WRITTEN OUT RATHER THAN TAKEN FROM SOMEWHERE
  The obvious shortcuts both have a licence attached. The TTF we ship for sbv (PixelOperatorMono8) is
  under the SIL Open Font License: usable, but rasterising it into this file makes a derivative that
  carries the licence and the reserved-name clause with it, into a source file whose header says
  something else. The IBM PC 8x8 ROM font that every retro project reaches for has a murkier story than
  its ubiquity suggests. Neither is worth the ambiguity for 95 characters of 8 bytes each, so these are
  drawn here and belong to this project outright.

  THE FORM: one line per glyph, eight bytes, MSB = leftmost pixel. Bit 7 is column 0, so a row of $18 is
  "....##.." - two pixels centred in an 8-wide cell. Rows run top to bottom. Every glyph leaves column 7
  and row 7 clear, which is what makes text legible without inter-character spacing logic: the gap is
  built into the cell, exactly as it is on the hardware this dialect grew up on.

  Coverage is ASCII 32..126. Anything outside it renders as the fallback glyph (a hollow box), which is
  deliberate: a missing character that shows up as a visible box is a bug report, and one that renders as
  a blank is a mystery. }
unit SedaiGfxFont;

{$mode ObjFPC}{$H+}

interface

const
  GFX_FONT_W = 8;    // cell width in pixels
  GFX_FONT_H = 8;    // cell height in pixels
  GFX_FONT_FIRST = 32;
  GFX_FONT_LAST  = 126;

type
  TGfxGlyph = array[0..GFX_FONT_H - 1] of Byte;

// The 8 rows of a character's cell. Any code point outside 32..126 answers the fallback box.
function GfxGlyph(Ch: Byte): TGfxGlyph;
// True when the character has a glyph of its own (i.e. is not being drawn as the fallback).
function GfxHasGlyph(Ch: Byte): Boolean;

implementation

const
  // Fallback: a hollow box. Visible on purpose - see the note in the unit header.
  FallbackGlyph: TGfxGlyph = ($7E, $42, $42, $42, $42, $42, $7E, $00);

  Font: array[GFX_FONT_FIRST..GFX_FONT_LAST] of TGfxGlyph = (
    ($00,$00,$00,$00,$00,$00,$00,$00),  // 32 space
    ($18,$18,$18,$18,$18,$00,$18,$00),  // 33 !
    ($6C,$6C,$48,$00,$00,$00,$00,$00),  // 34 "
    ($6C,$6C,$FE,$6C,$FE,$6C,$6C,$00),  // 35 #
    ($18,$3E,$60,$3C,$06,$7C,$18,$00),  // 36 $
    ($00,$C6,$CC,$18,$30,$66,$C6,$00),  // 37 %
    ($38,$6C,$38,$76,$DC,$CC,$76,$00),  // 38 &
    ($18,$18,$30,$00,$00,$00,$00,$00),  // 39 '
    ($0C,$18,$30,$30,$30,$18,$0C,$00),  // 40 (
    ($30,$18,$0C,$0C,$0C,$18,$30,$00),  // 41 )
    ($00,$66,$3C,$FF,$3C,$66,$00,$00),  // 42 *
    ($00,$18,$18,$7E,$18,$18,$00,$00),  // 43 +
    ($00,$00,$00,$00,$00,$18,$18,$30),  // 44 ,
    ($00,$00,$00,$7E,$00,$00,$00,$00),  // 45 -
    ($00,$00,$00,$00,$00,$18,$18,$00),  // 46 .
    ($06,$0C,$18,$30,$60,$C0,$80,$00),  // 47 /
    ($3C,$66,$6E,$7E,$76,$66,$3C,$00),  // 48 0
    ($18,$38,$18,$18,$18,$18,$7E,$00),  // 49 1
    ($3C,$66,$06,$1C,$30,$60,$7E,$00),  // 50 2
    ($3C,$66,$06,$1C,$06,$66,$3C,$00),  // 51 3
    ($0C,$1C,$3C,$6C,$7E,$0C,$0C,$00),  // 52 4
    ($7E,$60,$7C,$06,$06,$66,$3C,$00),  // 53 5
    ($1C,$30,$60,$7C,$66,$66,$3C,$00),  // 54 6
    ($7E,$66,$0C,$18,$18,$18,$18,$00),  // 55 7
    ($3C,$66,$66,$3C,$66,$66,$3C,$00),  // 56 8
    ($3C,$66,$66,$3E,$06,$0C,$38,$00),  // 57 9
    ($00,$18,$18,$00,$00,$18,$18,$00),  // 58 :
    ($00,$18,$18,$00,$00,$18,$18,$30),  // 59 ;
    ($0C,$18,$30,$60,$30,$18,$0C,$00),  // 60 <
    ($00,$00,$7E,$00,$7E,$00,$00,$00),  // 61 =
    ($30,$18,$0C,$06,$0C,$18,$30,$00),  // 62 >
    ($3C,$66,$06,$0C,$18,$00,$18,$00),  // 63 ?
    ($3C,$66,$6E,$6A,$6E,$60,$3C,$00),  // 64 @
    ($18,$3C,$66,$66,$7E,$66,$66,$00),  // 65 A
    ($7C,$66,$66,$7C,$66,$66,$7C,$00),  // 66 B
    ($3C,$66,$60,$60,$60,$66,$3C,$00),  // 67 C
    ($78,$6C,$66,$66,$66,$6C,$78,$00),  // 68 D
    ($7E,$60,$60,$7C,$60,$60,$7E,$00),  // 69 E
    ($7E,$60,$60,$7C,$60,$60,$60,$00),  // 70 F
    ($3C,$66,$60,$6E,$66,$66,$3E,$00),  // 71 G
    ($66,$66,$66,$7E,$66,$66,$66,$00),  // 72 H
    ($7E,$18,$18,$18,$18,$18,$7E,$00),  // 73 I
    ($1E,$0C,$0C,$0C,$0C,$6C,$38,$00),  // 74 J
    ($66,$6C,$78,$70,$78,$6C,$66,$00),  // 75 K
    ($60,$60,$60,$60,$60,$60,$7E,$00),  // 76 L
    ($C6,$EE,$FE,$D6,$C6,$C6,$C6,$00),  // 77 M
    ($66,$76,$7E,$7E,$6E,$66,$66,$00),  // 78 N
    ($3C,$66,$66,$66,$66,$66,$3C,$00),  // 79 O
    ($7C,$66,$66,$7C,$60,$60,$60,$00),  // 80 P
    ($3C,$66,$66,$66,$6E,$6C,$36,$00),  // 81 Q
    ($7C,$66,$66,$7C,$78,$6C,$66,$00),  // 82 R
    ($3C,$66,$60,$3C,$06,$66,$3C,$00),  // 83 S
    ($7E,$18,$18,$18,$18,$18,$18,$00),  // 84 T
    ($66,$66,$66,$66,$66,$66,$3C,$00),  // 85 U
    ($66,$66,$66,$66,$66,$3C,$18,$00),  // 86 V
    ($C6,$C6,$C6,$D6,$FE,$EE,$C6,$00),  // 87 W
    ($66,$66,$3C,$18,$3C,$66,$66,$00),  // 88 X
    ($66,$66,$66,$3C,$18,$18,$18,$00),  // 89 Y
    ($7E,$06,$0C,$18,$30,$60,$7E,$00),  // 90 Z
    ($3C,$30,$30,$30,$30,$30,$3C,$00),  // 91 [
    ($C0,$60,$30,$18,$0C,$06,$02,$00),  // 92 backslash
    ($3C,$0C,$0C,$0C,$0C,$0C,$3C,$00),  // 93 ]
    ($18,$3C,$66,$00,$00,$00,$00,$00),  // 94 ^
    ($00,$00,$00,$00,$00,$00,$00,$FF),  // 95 _
    ($30,$18,$0C,$00,$00,$00,$00,$00),  // 96 `
    ($00,$00,$3C,$06,$3E,$66,$3E,$00),  // 97 a
    ($60,$60,$7C,$66,$66,$66,$7C,$00),  // 98 b
    ($00,$00,$3C,$66,$60,$66,$3C,$00),  // 99 c
    ($06,$06,$3E,$66,$66,$66,$3E,$00),  // 100 d
    ($00,$00,$3C,$66,$7E,$60,$3C,$00),  // 101 e
    ($1C,$30,$7C,$30,$30,$30,$30,$00),  // 102 f
    ($00,$00,$3E,$66,$66,$3E,$06,$3C),  // 103 g
    ($60,$60,$7C,$66,$66,$66,$66,$00),  // 104 h
    ($18,$00,$38,$18,$18,$18,$3C,$00),  // 105 i
    ($0C,$00,$1C,$0C,$0C,$0C,$6C,$38),  // 106 j
    ($60,$60,$66,$6C,$78,$6C,$66,$00),  // 107 k
    ($38,$18,$18,$18,$18,$18,$3C,$00),  // 108 l
    ($00,$00,$EC,$FE,$D6,$C6,$C6,$00),  // 109 m
    ($00,$00,$7C,$66,$66,$66,$66,$00),  // 110 n
    ($00,$00,$3C,$66,$66,$66,$3C,$00),  // 111 o
    ($00,$00,$7C,$66,$66,$7C,$60,$60),  // 112 p
    ($00,$00,$3E,$66,$66,$3E,$06,$06),  // 113 q
    ($00,$00,$6C,$76,$60,$60,$60,$00),  // 114 r
    ($00,$00,$3E,$60,$3C,$06,$7C,$00),  // 115 s
    ($30,$30,$7C,$30,$30,$36,$1C,$00),  // 116 t
    ($00,$00,$66,$66,$66,$66,$3E,$00),  // 117 u
    ($00,$00,$66,$66,$66,$3C,$18,$00),  // 118 v
    ($00,$00,$C6,$C6,$D6,$FE,$6C,$00),  // 119 w
    ($00,$00,$66,$3C,$18,$3C,$66,$00),  // 120 x
    ($00,$00,$66,$66,$66,$3E,$06,$3C),  // 121 y
    ($00,$00,$7E,$0C,$18,$30,$7E,$00),  // 122 z
    ($0E,$18,$18,$70,$18,$18,$0E,$00),  // 123 {
    ($18,$18,$18,$18,$18,$18,$18,$00),  // 124 |
    ($70,$18,$18,$0E,$18,$18,$70,$00),  // 125 }
    ($76,$DC,$00,$00,$00,$00,$00,$00)   // 126 ~
  );

function GfxHasGlyph(Ch: Byte): Boolean;
begin
  Result := (Ch >= GFX_FONT_FIRST) and (Ch <= GFX_FONT_LAST);
end;

function GfxGlyph(Ch: Byte): TGfxGlyph;
begin
  if GfxHasGlyph(Ch) then Result := Font[Ch] else Result := FallbackGlyph;
end;

end.
