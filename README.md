# Sedai Audio Foundation

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Maurizio%20Cammalleri-0077B5?logo=linkedin)](https://www.linkedin.com/in/maurizio-cammalleri-80a89a11/)
[![Substack](https://img.shields.io/badge/Substack-Maurizio%20Cammalleri-FF6719?logo=substack)](https://cammalleri.substack.com/)

> **DISCLAIMER**: This library is in advanced development stage but **not yet ready for any use**. The API is unstable, many features are incomplete or broken, and there are known bugs. However, we encourage you to try it out and report any bugs or suggest improvements! Your feedback is valuable to help us improve the library.

A professional-grade, modular audio synthesis and MIDI playback library written in Free Pascal.

## Overview

Sedai Audio Foundation provides a comprehensive audio synthesis framework with:

- **3 Independent Synthesis Engines**: Classic (Subtractive), FM, and Wavetable
- **Real-time MIDI Playback**: Standard MIDI file support with 16-channel polyphony
- **Audio Effects**: Delay, Reverb, Chorus, Flanger, Distortion
- **Advanced Filters**: 6 filter types with multi-pole cascading
- **40+ Built-in Presets**: Ready-to-use sounds for all synthesis types
- **Cross-Platform**: Works on Linux and Windows via SDL2

---

## Technologies

### Core Technologies

| Technology | Version | Purpose |
|------------|---------|---------|
| **Free Pascal** | 3.0.4+ | Primary programming language |
| **SDL2** | 2.0+ | Cross-platform audio device access |
| **Lazarus** | Optional | IDE support |

### Audio Technologies

| Component | Technology |
|-----------|------------|
| **Sample Rate** | 44100 Hz (CD quality) |
| **Bit Depth** | 32-bit floating point internal processing |
| **Buffer Size** | 1024 samples (~23ms latency) |
| **Voice Polyphony** | 32-128 voices (configurable) |

### Synthesis Technologies

| Engine | Description |
|--------|-------------|
| **Classic/Subtractive** | Analog-style synthesis with oscillators, filters, LFO |
| **FM (Frequency Modulation)** | DX7-style 6-operator FM synthesis |
| **Wavetable** | Modern wavetable synthesis with morphing |
| **Additive** | Harmonic spectrum synthesis |

### Signal Processing

| Technology | Description |
|------------|-------------|
| **ADSR Envelopes** | 4-stage envelopes with 4 curve types (Linear, Exponential, Logarithmic, S-Curve) |
| **Biquad Filters** | Low-pass, High-pass, Band-pass, Notch, Allpass, Peaking |
| **Multi-pole Filters** | 12dB, 24dB, 48dB per octave cascaded filters |
| **Audio Effects** | Delay, Reverb, Chorus, Flanger, Distortion |
| **Stereo Processing** | Panning, stereo width control |

### MIDI Technologies

| Component | Description |
|-----------|-------------|
| **MIDI Parser** | Standard MIDI File (SMF) Format 0 and 1 |
| **MIDI Sequencer** | Real-time event scheduling with tempo control |
| **Channel Support** | 16 MIDI channels with per-channel configuration |
| **Controllers** | Pitch bend, modulation wheel, velocity |

### Wavetable Formats

| Format | Extension | Description |
|--------|-----------|-------------|
| **Serum** | .wav | 2048 samples per frame, industry standard |
| **Vital** | .wav | Serum-compatible format |
| **Surge/SurgeXT** | .wt | Native Surge wavetable format |
| **Generic WAV** | .wav | Standard audio files |

### Platforms

| Platform | Compiler Target | Status |
|----------|-----------------|--------|
| **Windows 10/11** | x86_64-win64, i386-win32 | Supported |
| **Linux** | x86_64-linux, i386-linux | Supported |
| **macOS** | x86_64-darwin, aarch64-darwin | Planned |

---

## Architecture

### Layer Structure

```
┌─────────────────────────────────────────────────────────┐
│  Layer 5: Applications & Demos                          │
│  audiotest.lpr, demo_*.pas                              │
├─────────────────────────────────────────────────────────┤
│  Layer 4: High-Level API                                │
│  SedaiAudioFoundation, SedaiMIDIFoundation              │
├─────────────────────────────────────────────────────────┤
│  Layer 3: Integration                                   │
│  SedaiAudioChip (SDL2), SedaiSynthesisEngine            │
├─────────────────────────────────────────────────────────┤
│  Layer 2: Synthesis Engines                             │
│  SedaiClassicProcessor, SedaiFMProcessor,               │
│  SedaiWavetableProcessor                                │
├─────────────────────────────────────────────────────────┤
│  Layer 1: Foundation                                    │
│  SedaiAudioTypes, SedaiADSRProcessor, SedaiWaveGenerator│
│  SedaiFilters, SedaiAudioEffects, SedaiMIDITypes        │
└─────────────────────────────────────────────────────────┘
```

### Module Overview

| Module | Description |
|--------|-------------|
| **SedaiAudioTypes** | Core types: TWaveType, TADSR, TStereoSample |
| **SedaiAudioFoundation** | Main public API for synthesis and playback |
| **SedaiAudioChip** | SDL2 audio device management |
| **SedaiSynthesisEngine** | Voice allocation and routing |
| **SedaiClassicProcessor** | Subtractive synthesis (3 oscillators, filters, LFO) |
| **SedaiFMProcessor** | FM synthesis (6 operators, 5 algorithms) |
| **SedaiWavetableProcessor** | Wavetable synthesis (4 oscillators, morphing) |
| **SedaiADSRProcessor** | Envelope generator (4 curve types) |
| **SedaiWaveGenerator** | Basic waveform generation |
| **SedaiFilters** | Biquad and multi-pole filters |
| **SedaiAudioEffects** | Delay, Reverb, Chorus, Flanger, Distortion |
| **SedaiStereoProcessor** | Stereo panning and width |
| **SedaiMIDIParser** | Standard MIDI file parser |
| **SedaiMIDISequencer** | Real-time MIDI playback |
| **SedaiMIDIFoundation** | High-level MIDI API |
| **SedaiWavetableLoader** | Multi-format wavetable loader |

---

## Synthesis Engines

### Classic (Subtractive) Synthesis

- Up to 3 oscillators per voice
- 5 waveform types: Sine, Square, Sawtooth, Triangle, Noise
- Subtractive filter with resonance
- LFO modulation (pitch, filter, amplitude)
- Per-oscillator ADSR envelopes

**Presets**: sine, square, saw, triangle, lead, bass, pad, strings, brass, organ, pluck, synthkeys, warmbass

### FM Synthesis

- 6 operators (DX7-style)
- 5 algorithms: Simple, Stack, Parallel, Feedback, Complex
- Per-operator ADSR envelopes
- Operator feedback control
- Modulation depth scaling

**Presets**: epiano, brass, bell, organ, lead, bass, choir, fmstrings, fmpad, marimba, flute, churchbell

### Wavetable Synthesis

- Up to 4 oscillators per voice
- Real-time wavetable morphing with interpolation
- Multiple mix modes: Add, Multiply, Ring Modulation
- Support for external wavetable formats

**Presets**: serum, wasp, ppg, vocal, metallic, glass, organ, evolving, digitalchaos

**Supported Wavetable Formats**: Serum (.wav, 2048 samples/frame), Vital (.wav), Surge/SurgeXT (.wt), Generic WAV

---

## MIDI Support

- Standard MIDI file format 0/1 parsing
- 16-channel support with per-channel configuration
- Real-time tempo control
- Pitch bend and modulation wheel support
- Velocity-sensitive playback
- General MIDI program mapping to wavetable types

---

## System Requirements

### Compiler

- **Free Pascal** 3.0.4 or higher
- **Lazarus** (optional, for IDE support)

### Dependencies

- **SDL2** (Simple DirectMedia Layer 2)

### Operating Systems

- Linux (Ubuntu, Debian, Fedora, Arch)
- Windows 10/11

---

## Installation

### Linux (Debian/Ubuntu)

```bash
sudo apt install fpc fp-units-base fp-units-rtl libsdl2-dev
```

### Linux (Fedora/RHEL)

```bash
sudo dnf install fpc SDL2-devel
```

### Linux (Arch)

```bash
sudo pacman -S fpc sdl2
```

### Windows

1. Download and install [Free Pascal](https://www.freepascal.org/download.html)
2. Download [SDL2 Development Libraries](https://github.com/libsdl-org/SDL/releases)
3. Place `SDL2.dll` in the project directory or system PATH

---

## Building

### Using Build Scripts

The project includes cross-platform build scripts that support custom compiler paths.

#### Linux

```bash
# Make script executable (first time only)
chmod +x build.sh

# Build with system FPC
./build.sh

# Build with custom FPC path
./build.sh --fpc /path/to/fpc

# Build only demos
./build.sh --demos-only

# Clean build artifacts
./build.sh --clean

# Show help
./build.sh --help
```

#### Windows (PowerShell)

```powershell
# Build with system FPC
.\build.ps1

# Build with custom FPC path
.\build.ps1 -FpcPath "C:\FPC\3.2.2\bin\i386-win32\fpc.exe"

# Build only demos
.\build.ps1 -DemosOnly

# Clean build artifacts
.\build.ps1 -Clean

# Show help
.\build.ps1 -Help
```

#### Windows (Command Prompt)

```batch
REM Build with system FPC
build.bat

REM Build with custom FPC path
build.bat --fpc "C:\FPC\3.2.2\bin\i386-win32\fpc.exe"

REM Build only demos
build.bat --demos-only

REM Clean build artifacts
build.bat --clean
```

### Manual Compilation

```bash
# Compile main test program
fpc -Mdelphi audiotest.lpr

# Compile individual demos
fpc -Mdelphi demo_presets.pas
fpc -Mdelphi demo_midi_player.pas
fpc -Mdelphi demo_filters_effects.pas
fpc -Mdelphi demo_additive.pas
```

---

## Demo Programs

### audiotest

The main test program with a comprehensive menu system.

```bash
./audiotest
```

**Features**:
- Classic, FM, and Wavetable synthesis tests
- MIDI file loading and playback
- Channel mapping configuration
- Tempo control
- System status display

### demo_presets

Demonstrates all built-in synthesis presets.

```bash
./demo_presets
```

**Menu Options**:
1. Classic Synthesis Presets (6 presets)
2. FM Synthesis Presets (6 presets)
3. Wavetable Presets (6 presets)
4. Synthesis Engines Comparison
5. Full Demo (all presets)

### demo_midi_player

Interactive MIDI file player with real-time controls.

```bash
./demo_midi_player
```

**Controls**:
- `P` - Play/Pause
- `S` - Stop
- `+`/`-` - Volume control
- `F`/`L` - Faster/Slower tempo
- `I` - File info
- `C` - Channel info
- `T` - Playback status
- `M` - Load another file
- `Q` - Quit

**Note**: Place a `.mid` file in the application directory or provide the full path when prompted.

### demo_filters_effects

Tests audio filters and effects processing.

```bash
./demo_filters_effects
```

**Tests**:
- Biquad filters (LP, HP, BP, Notch, Allpass, Peaking)
- Multi-pole filters (12dB, 24dB, 48dB/octave)
- Effects (Delay, Reverb, Chorus, Flanger, Distortion)

**Note**: This demo does NOT require audio output - it tests mathematical correctness.

### demo_additive

Demonstrates additive synthesis with harmonic control.

```bash
./demo_additive
```

**Features**:
- Pure tone and complex wave generation
- 6 harmonic spectrum presets
- Harmonic vs. inharmonic synthesis comparison
- Custom spectrum creation
- ADSR envelope visualization

**Note**: This demo does NOT require audio output.

---

## Quick Start

### Basic Usage

```pascal
program MyAudioApp;

uses
  SedaiAudioFoundation;

begin
  // Initialize audio system
  if InitAudio(32) then  // 32 voices
  begin
    SetMasterVolume(0.7);

    // Play a simple sine wave
    PlaySine(440.0);  // A4 note
    Sleep(1000);

    // Play a wavetable preset
    PlaySerum(261.63);  // C4 note
    Sleep(1000);

    // Play FM piano
    PlayEPiano(329.63);  // E4 note
    Sleep(1000);

    StopAll;
    ShutdownAudio;
  end;
end.
```

### MIDI Playback

```pascal
program MyMIDIPlayer;

uses
  SedaiAudioFoundation, SedaiMIDIFoundation;

begin
  if InitAudio(64) and InitMidi then
  begin
    SetMasterVolume(0.7);
    SetupMidiGeneralMidi;

    if LoadMidiFile('song.mid') then
    begin
      MidiPlay;

      while IsMidiPlaying do
        Sleep(100);

      MidiStop;
    end;

    ShutdownMidi;
    ShutdownAudio;
  end;
end.
```

### Advanced Voice Control

```pascal
var
  VoiceIndex: Integer;
begin
  // Get voice index for advanced control
  VoiceIndex := PlayWavetableAdv(440.0, 'serum');

  if VoiceIndex >= 0 then
  begin
    SetVoicePan(VoiceIndex, -0.5);  // Pan left
    Sleep(2000);
    NoteOff(VoiceIndex);  // Release with ADSR
  end;
end;
```

---

## API Reference

### Audio Initialization

| Function | Description |
|----------|-------------|
| `InitAudio(VoiceCount)` | Initialize audio system with specified voice count |
| `ShutdownAudio` | Shutdown audio system |
| `SetMasterVolume(Volume)` | Set master volume (0.0-1.0) |
| `GetMasterVolume` | Get current master volume |
| `GetActiveVoices` | Get number of currently active voices |
| `GetMaxVoices` | Get maximum voice count |
| `GetSampleRate` | Get audio sample rate |
| `PrintStatus` | Print voice status information |

### Classic Synthesis

| Function | Description |
|----------|-------------|
| `PlaySine(Freq)` | Play sine wave |
| `PlaySquare(Freq)` | Play square wave |
| `PlaySaw(Freq)` | Play sawtooth wave |
| `PlayTriangle(Freq)` | Play triangle wave |
| `PlayLead(Freq)` | Play lead preset |
| `PlayBass(Freq)` | Play bass preset |
| `PlayPad(Freq)` | Play pad preset |
| `PlayClassic(Freq, Preset)` | Play classic preset |
| `PlayClassicAdv(Freq, Preset)` | Play and return voice index |

### FM Synthesis

| Function | Description |
|----------|-------------|
| `PlayEPiano(Freq)` | Play electric piano |
| `PlayFMBrass(Freq)` | Play FM brass |
| `PlayFMBell(Freq)` | Play FM bell |
| `PlayFMOrgan(Freq)` | Play FM organ |
| `PlayFMLead(Freq)` | Play FM lead |
| `PlayFMBass(Freq)` | Play FM bass |
| `PlayFM(Freq, Preset)` | Play FM preset |
| `PlayFMAdv(Freq, Preset)` | Play and return voice index |

### Wavetable Synthesis

| Function | Description |
|----------|-------------|
| `PlaySerum(Freq)` | Play Serum-style wavetable |
| `PlayWasp(Freq)` | Play WASP-style wavetable |
| `PlayPPG(Freq)` | Play PPG-style wavetable |
| `PlayWavetable(Freq, Type)` | Play wavetable preset |
| `PlayWavetableAdv(Freq, Type)` | Play and return voice index |

### Voice Control

| Function | Description |
|----------|-------------|
| `NoteOff(VoiceIndex)` | Release note (starts ADSR release) |
| `NoteRelease(VoiceIndex)` | Alternative release function |
| `SetVoicePan(VoiceIndex, Pan)` | Set voice pan (-1.0 to 1.0) |
| `StopAll` | Stop all voices immediately |
| `SmoothStopAll(FadeMs)` | Fade out all voices |

### MIDI Voice Management

| Function | Description |
|----------|-------------|
| `MidiAllocateVoice` | Allocate a MIDI voice, returns index or -1 |
| `MidiIsVoiceActive(Index)` | Check if voice is active |
| `MidiGetFreeVoiceCount` | Get number of available voices |
| `MidiVoiceOn(Index)` | Start playing allocated voice |
| `MidiVoiceOff(Index)` | Stop voice (ADSR release) |
| `MidiReleaseVoice(Index)` | Stop and deallocate voice |
| `MidiReleaseAllVoices` | Release all MIDI voices |

### MIDI Voice Configuration

| Function | Description |
|----------|-------------|
| `MidiSetVoiceFrequency(Index, Freq)` | Set voice frequency in Hz |
| `MidiSetVoiceAmplitude(Index, Amp)` | Set amplitude (0.0-1.0) |
| `MidiSetVoiceWavetable(Index, Type)` | Set wavetable preset |
| `MidiSetVoicePan(Index, Pan)` | Set pan (-1.0 to 1.0) |
| `MidiGetVoiceFrequency(Index)` | Get current frequency |
| `MidiGetVoiceAmplitude(Index)` | Get current amplitude |
| `MidiGetVoiceWavetable(Index)` | Get current wavetable |
| `MidiGetVoicePan(Index)` | Get current pan |

### MIDI Utility Functions

| Function | Description |
|----------|-------------|
| `MidiNoteToFreq(Note)` | Convert MIDI note (0-127) to frequency Hz |
| `MidiFreqToNote(Freq)` | Convert frequency to MIDI note |
| `MidiVelocityToAmp(Velocity)` | Convert velocity (0-127) to amplitude |
| `MidiAmpToVelocity(Amp)` | Convert amplitude to velocity |
| `MidiPanToSedai(Pan)` | Convert MIDI pan (0-127) to Sedai (-1 to 1) |
| `SedaiPanToMidi(Pan)` | Convert Sedai pan to MIDI pan |
| `MidiNoteToName(Note)` | Get note name (e.g., "C4", "A#3") |
| `MidiNoteToOctave(Note)` | Get octave number |

### MIDI-Optimized Playback

| Function | Description |
|----------|-------------|
| `PlayWavetableMidi(Freq, Type, Amp)` | Play with MIDI voice allocation |
| `PlayMidiNote(Note, Velocity, Type)` | Play by MIDI note number |

### MIDI File Playback

| Function | Description |
|----------|-------------|
| `InitMidi` | Initialize MIDI system |
| `ShutdownMidi` | Shutdown MIDI system |
| `LoadMidiFile(Filename)` | Load a MIDI file |
| `MidiPlay` | Start playback |
| `MidiPause` | Pause playback |
| `MidiStop` | Stop playback |
| `IsMidiPlaying` | Check if playing |
| `GetMidiProgress` | Get playback progress (0-100%) |
| `SetMidiTempo(Percent)` | Set tempo (1.0 = normal) |
| `SetupMidiGeneralMidi` | Apply General MIDI mapping |

### Musical Helpers

| Function | Description |
|----------|-------------|
| `PlayChordClassic(Note1, Note2, Note3, Preset)` | Play a classic chord |
| `PlayChordFM(Note1, Note2, Note3, Preset)` | Play an FM chord |
| `PlayChordWavetable(Note1, Note2, Note3, Preset)` | Play a wavetable chord |
| `PlayScaleClassic(BaseFreq, Preset)` | Play a classic scale |
| `PlayScaleFM(BaseFreq, Preset)` | Play an FM scale |
| `PlayScaleWavetable(BaseFreq, Preset)` | Play a wavetable scale |

---

## Code Metrics

- **Total Lines**: ~11,500 lines of Pascal code
- **Source Files**: 25 files (22 units + 3 programs)
- **Dependencies**: SDL2 only
- **Voice Polyphony**: 32 default (configurable up to 128)
- **Sample Rate**: 44100 Hz
- **Audio Buffer**: 1024 samples

---

## License

This project is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.

You are free to use, modify, and distribute this software under the terms of the GPL-3.0 license. See the [LICENSE](LICENSE) file for details.

### Commercial Licensing

For commercial use or proprietary licensing options, please contact the author:

**Maurizio Cammalleri**
Email: maurizio.cammalleri@gmail.com

---

## Contributing

For bug reports or suggestions, please open an issue on the project repository.

---

**Enjoy Sedai Audio Foundation!**
