<#
.SYNOPSIS
    Build SedaiBasic projects (sb, sbc, sbd, sbv, sbw)

.DESCRIPTION
    Cross-platform build script for SedaiBasic.
    Uses direct fpc calls with optimized settings.

.PARAMETER Target
    Which target to build: all, sb, sbc, sbd, sbv, sbw (default: all)

.PARAMETER Debug
    Build with debug info instead of release optimizations

.PARAMETER Clean
    Clean build artifacts before building

.PARAMETER CPU
    Target CPU: x86_64, i386, aarch64 (default: x86_64)

.PARAMETER OS
    Target OS: win64, win32, linux, darwin (default: win64)

.PARAMETER WithSedaiAudio
    SedaiAudioFoundation integration:
    - '' (empty/default): auto-detect in deps/ then ..\SedaiAudioFoundation
    - 'no': disable audio support
    - <path>: use specified path to SedaiAudioFoundation
    Audio is enabled for sb and sbv targets only.

.PARAMETER DebugFlags
    Comma-separated list of debug flags to enable at compile time.
    Examples: 'REGALLOC', 'SSA,REGALLOC', 'ALL'
    Available flags: SSA, GVN, CSE, DCE, LICM, ALGEBRAIC, STRENGTH, CONSTPROP,
                     COPYPROP, COPYCOAL, PHIELIM, REGALLOC, PEEPHOLE, SUPERINSTR,
                     DOMTREE, DBE, BYTECODE, VM, CLEANUP, CONSOLE, AUDIO, ALL

.EXAMPLE
    .\build.ps1                    # Build all targets (release)
    .\build.ps1 -Target sb         # Build only sb
    .\build.ps1 -Debug             # Build with debug info
    .\build.ps1 -Clean             # Clean and rebuild
    .\build.ps1 -WithSedaiAudio no                    # Build without audio support
    .\build.ps1 -WithSedaiAudio C:\path\to\audio      # Use specific audio path
    .\build.ps1 -Target sb -DebugFlags REGALLOC       # Build sb with register allocator debug
    .\build.ps1 -Target sb -DebugFlags SSA,REGALLOC   # Build sb with multiple debug flags

.NOTES
    Copyright (c) 2025 Maurizio Cammalleri
    Released under GNU GPL v3 or Commercial License
#>

param(
    [ValidateSet('all', 'sb', 'sbc', 'sbd', 'sbv', 'sbw')]
    [string]$Target = 'all',

    [switch]$Debug,
    [switch]$Clean,
    [switch]$NoBanner,

    # Build the CLI VM (sb) with the optional SDL2 window presenter, enabling `sb --window`.
    # Default off: the headless sb (regression target) takes no SDL2 window dependency.
    [switch]$Window,

    [ValidateSet('x86_64', 'i386', 'aarch64', '')]
    [string]$CPU = 'x86_64',

    [ValidateSet('win64', 'win32', 'linux', 'darwin', '')]
    [string]$OS = 'win64',

    # SedaiAudio integration
    # Values: '' (auto-detect), 'no' (disabled), or path to SedaiAudioFoundation
    [string]$WithSedaiAudio = '',

    # Debug flags - comma-separated list of debug flags to enable
    # Examples: 'REGALLOC', 'SSA,REGALLOC', 'ALL'
    # Available: SSA, GVN, CSE, DCE, LICM, ALGEBRAIC, STRENGTH, CONSTPROP,
    #            COPYPROP, COPYCOAL, PHIELIM, REGALLOC, PEEPHOLE, SUPERINSTR,
    #            DOMTREE, DBE, BYTECODE, VM, CLEANUP, CONSOLE, AUDIO, ALL
    [string]$DebugFlags = ''
)

$ErrorActionPreference = 'Stop'
$Script:ProjectRoot = $PSScriptRoot
$Script:SrcDir = Join-Path $ProjectRoot 'src'
$Script:LibDir = Join-Path $ProjectRoot 'lib'
$Script:BinDir = Join-Path $ProjectRoot 'bin'
$Script:ConfigFile = Join-Path $ProjectRoot 'setup.config.json'

# User configuration (loaded from setup.config.json if exists)
$Script:UserConfig = @{
    FpcPath = $null
    SDL2Path = $null
    RuntimePath = $null
    SedaiAudioPath = $null
}

# SedaiAudio detection result (set later)
$Script:SedaiAudioPath = $null
$Script:SedaiAudioEnabled = $false

# Load configuration from setup.config.json
function Load-BuildConfig {
    if (Test-Path $ConfigFile) {
        try {
            $json = Get-Content $ConfigFile -Raw | ConvertFrom-Json
            if ($json.FpcPath) { $Script:UserConfig.FpcPath = $json.FpcPath }
            if ($json.SDL2Path) { $Script:UserConfig.SDL2Path = $json.SDL2Path }
            if ($json.RuntimePath) { $Script:UserConfig.RuntimePath = $json.RuntimePath }
            if ($json.SedaiAudioPath) { $Script:UserConfig.SedaiAudioPath = $json.SedaiAudioPath }
            return $true
        } catch {
            return $false
        }
    }
    return $false
}

# Detect FPC compiler
function Find-FPC {
    # First check user configuration from setup.config.json
    if ($UserConfig.FpcPath) {
        $configFpc = Join-Path $UserConfig.FpcPath "bin\x86_64-win64\fpc.exe"
        if (Test-Path $configFpc) {
            return $configFpc
        }
    }

    # Project-local FPC (installed by setup.ps1) - always preferred
    $localFpc = Join-Path $ProjectRoot 'fpc\3.2.2\bin\x86_64-win64\fpc.exe'
    if (Test-Path $localFpc) {
        return $localFpc
    }

    # Check common Lazarus installations
    $lazarusPaths = @(
        'C:\lazarus-3.8\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.6\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.4\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.2\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe'
    )
    foreach ($lazPath in $lazarusPaths) {
        if (Test-Path $lazPath) {
            return $lazPath
        }
    }

    # Fallback to system PATH
    $fpc = Get-Command fpc -ErrorAction SilentlyContinue
    if ($fpc) {
        return $fpc.Source
    }

    return $null
}

# Get target platform string
function Get-PlatformDir {
    param([string]$cpu, [string]$os)
    return "$cpu-$os"
}

# Detect SedaiAudioFoundation
function Find-SedaiAudio {
    param([string]$RequestedPath)

    # Explicitly disabled via command line
    if ($RequestedPath -eq 'no') {
        return @{ Enabled = $false; Path = $null; Reason = 'Disabled via -WithSedaiAudio=no' }
    }

    # Check if disabled in config
    if ($UserConfig.SedaiAudioPath -eq 'disabled') {
        return @{ Enabled = $false; Path = $null; Reason = 'Disabled in setup.config.json' }
    }

    # Explicit path provided via command line
    if ($RequestedPath -and $RequestedPath -ne '') {
        $srcPath = Join-Path $RequestedPath 'src'
        if (Test-Path (Join-Path $srcPath 'sedaiaudiofoundation.pas')) {
            return @{ Enabled = $true; Path = $RequestedPath; Reason = "Explicit path: $RequestedPath" }
        } else {
            Write-Host "WARNING: SedaiAudioFoundation not found at: $RequestedPath" -ForegroundColor Yellow
            return @{ Enabled = $false; Path = $null; Reason = "Not found at: $RequestedPath" }
        }
    }

    # Check user configuration from setup.config.json
    if ($UserConfig.SedaiAudioPath -and $UserConfig.SedaiAudioPath -ne 'disabled') {
        $configSrcPath = Join-Path $UserConfig.SedaiAudioPath 'src'
        if (Test-Path (Join-Path $configSrcPath 'sedaiaudiofoundation.pas')) {
            return @{ Enabled = $true; Path = $UserConfig.SedaiAudioPath; Reason = "From config: $($UserConfig.SedaiAudioPath)" }
        }
    }

    # Auto-detect: check deps/ first
    $depsPath = Join-Path $ProjectRoot 'deps\SedaiAudioFoundation'
    if (Test-Path (Join-Path $depsPath 'src\sedaiaudiofoundation.pas')) {
        return @{ Enabled = $true; Path = $depsPath; Reason = 'Found in deps/' }
    }

    # Auto-detect: check sibling folder (same level as SedaiBasic2)
    $defaultPath = Join-Path (Split-Path -Parent $ProjectRoot) 'SedaiAudioFoundation'
    if (Test-Path (Join-Path $defaultPath 'src\sedaiaudiofoundation.pas')) {
        return @{ Enabled = $true; Path = $defaultPath; Reason = "Found at default: $defaultPath" }
    }

    # Not found
    return @{ Enabled = $false; Path = $null; Reason = 'Not found (use -WithSedaiAudio=<path> to specify)' }
}

# What the CPU we are building FOR can actually execute.
#
# ⛔ This exists because the instruction-set flags used to be gated on whether AUDIO was enabled -
# two things that have nothing to do with each other. The effect was that sbc, sbd and sbw (the
# targets without audio) were built with -CpCOREAVX2 -CfAVX2 on a machine whose CPU has AVX but
# NOT AVX2 and NOT FMA (an i7-3630QM, Ivy Bridge). Any AVX2 or FMA instruction FPC chose to emit -
# and -OoFASTMATH encourages FMA - is an illegal instruction at run time on that CPU.
#
# ⚠️ Detection is of the HOST, so it is only meaningful when not cross-compiling; the caller gates
# on $TargetCPU matching. Set SEDAI_CPUOPT=none to force the portable baseline (useful when the
# binaries have to run on an older machine than the one that builds them), or =avx / =avx2 to force
# a level explicitly.
#
# ⛔ THE DEFAULT IS THE PORTABLE BASELINE, and that is a MEASURED decision, not caution.
# Measured 15 Aug 2026: the AVX2/FMA flags buy NOTHING. FPC has no auto-vectorizer - on a
# trivially vectorizable loop, with -O3 -CfAVX2 -OpCOREAVX2 -OoFASTMATH, it emits ZERO %ymm
# registers where gcc -O3 -march=native emits 19 - so -CfAVX2 only gives the VEX encoding of
# SCALAR operations. In the whole 653 943-instruction sb binary: 0 %ymm, and 29 FMAs, all
# scalar. A/B between two sb differing ONLY in the instruction set, best of 7 runs:
#     n-body (N=50M, --aot)   3973 ms AVX2   3986 ms SSE2   +0.3%
#     spectral-norm (N=2000)   160 ms AVX2    158 ms SSE2   -1.2%
# Both gaps are SMALLER than the null A/B (the same binary against a copy of itself: +0.7%
# and +3.8%). There is no signal.
# ⇒ Detecting the host made every build unrunnable on an older CPU - a real cost - in exchange
# for a measured zero. Detection stays available (SEDAI_CPUOPT=avx2) for whoever wants it.
# ⚠️ The vectorization that IS worth having is not here: it is in the code our own AOT/JIT
# emitters generate, which we control.
# ⛔ build.sh does exactly the same thing: the two scripts must behave identically.
function Get-CpuOptLevel {
    $forced = $env:SEDAI_CPUOPT
    if ($forced) {
        switch ($forced.ToLower()) {
            'none' { return 'none' }
            'avx'  { return 'avx' }
            'avx2' { return 'avx2' }
            default { Write-Host "  WARNING: SEDAI_CPUOPT='$forced' not understood, detecting instead" -ForegroundColor Yellow }
        }
    }
    # Nothing forced: the portable baseline. See the note above - the levels are detectable and
    # cost nothing to reach, they simply do not pay.
    return 'none'

    # ---- host detection, reachable only via SEDAI_CPUOPT=avx / =avx2 above ----
    # System.Runtime.Intrinsics needs .NET Core (PowerShell 7+). Under Windows PowerShell 5.1 the
    # type is absent, and then the honest answer is the SAFE one: assume nothing beyond the x86-64
    # baseline rather than emit instructions the CPU may not have.
    try {
        $avx2 = [System.Runtime.Intrinsics.X86.Avx2]::IsSupported
        $fma  = [System.Runtime.Intrinsics.X86.Fma]::IsSupported
        $avx  = [System.Runtime.Intrinsics.X86.Avx]::IsSupported
        if ($avx2 -and $fma) { return 'avx2' }
        if ($avx) { return 'avx' }
        return 'none'
    } catch {
        return 'none'
    }
}

# One unit directory per BUILD CONFIGURATION, not one per platform.
#
# lib\<platform> used to be a single directory shared by every target, so the shared units (VM,
# SSA, register allocator, lexer) were compiled by whichever target ran FIRST and every later
# target REUSED them - instruction set and defines included. FPC does not recompile a unit when
# only -Cp/-Cf/-d change, so the reuse is silent.
#
# Measured 12 Aug 2026: compiling sb (audio, no AVX flags) and then sbc (no audio, -CfAVX2) into
# the same unit directory leaves SedaiBytecodeVM.o BYTE-IDENTICAL, so the engine kept the flags
# of the first target while the banner announced "AVX2 + FMA".
#
# WEB_MODE already had its own directory for exactly this reason; the rule is now general.
# Must stay identical to unit_dir_for() in build.sh.
function Get-UnitDir {
    param(
        [string]$PlatformDir,
        [bool]$IsWeb,
        [bool]$WithAudio,
        [bool]$IsDebug,
        [string[]]$DebugDefines = @()
    )

    $name = $PlatformDir
    if ($IsWeb) { $name = "$name-web" }
    if ($WithAudio) { $name = "$name-audio" }

    if ($IsDebug) {
        $name = "$name-debug"
    } elseif ($Script:CpuOptLevel -ne 'none') {
        $name = "$name-$($Script:CpuOptLevel)"
    }

    # Defines change unit CONTENT, so they belong in the key too - that is what used to make a
    # forgotten -Clean produce a build with half the units on the old define.
    if ($DebugDefines.Count -gt 0) {
        $joined = ($DebugDefines -join ',') + ','
        $md5 = [System.Security.Cryptography.MD5]::Create()
        $hash = $md5.ComputeHash([System.Text.Encoding]::ASCII.GetBytes($joined))
        $md5.Dispose()
        $name = "$name-" + (($hash | ForEach-Object { $_.ToString('x2') }) -join '').Substring(0, 6)
    }

    return $name
}

# The banner says which instruction set was CHOSEN. It used to say "AVX2 + FMA" over binaries that
# contained not one AVX instruction, and nothing in the build made that visible. So look at the
# binary that was actually produced and say what is in it. Needs objdump; stays quiet without it.
function Test-InstructionSet {
    param([string]$Binary, [bool]$IsDebug, [string]$TargetCPU)

    if ($IsDebug -or $TargetCPU -ne 'x86_64') { return }
    if (-not (Get-Command objdump -ErrorAction SilentlyContinue)) { return }

    $vex = @(& objdump -d $Binary 2>$null | Select-String -Pattern "`tv[a-z0-9]+" -AllMatches).Count

    if ($Script:CpuOptLevel -eq 'none') {
        Write-Host "    baseline x86-64, $vex AVX instructions" -ForegroundColor Gray
    } elseif ($vex -eq 0) {
        Write-Host "    WARNING: built for $($Script:CpuOptLevel) but the binary has NO AVX instruction." -ForegroundColor Yellow
        Write-Host "    Shared units were reused from another configuration - build with -Clean." -ForegroundColor Yellow
    } else {
        Write-Host "    $vex AVX instructions" -ForegroundColor Gray
    }
}

# Build a single target
function Build-Target {
    param(
        [string]$LprFile,
        [string]$OutputName,
        [string]$FPC,
        [string]$PlatformDir,
        [string]$TargetCPU,
        [string]$TargetOS,
        [bool]$IsDebug,
        [string[]]$ExtraUnitPaths = @(),
        [bool]$WithAudio = $false,
        [string]$AudioPath = '',
        [string[]]$DebugDefines = @(),
        [bool]$IsWeb = $false
    )

    $srcPath = Join-Path $SrcDir $LprFile
    if (-not (Test-Path $srcPath)) {
        Write-Host "ERROR: Source file not found: $srcPath" -ForegroundColor Red
        return $false
    }

    # Create output directories - one unit directory per build configuration
    $libSubDir = Get-UnitDir -PlatformDir $PlatformDir -IsWeb $IsWeb -WithAudio $WithAudio `
                             -IsDebug $IsDebug -DebugDefines $DebugDefines
    $libPath = Join-Path $LibDir $libSubDir
    $binPath = Join-Path $BinDir $PlatformDir

    if (-not (Test-Path $libPath)) { New-Item -ItemType Directory -Path $libPath -Force | Out-Null }
    if (-not (Test-Path $binPath)) { New-Item -ItemType Directory -Path $binPath -Force | Out-Null }

    # Build compiler options
    $opts = @()

    # Output name
    $opts += "-o`"$OutputName`""

    # Target platform
    $opts += "-P$TargetCPU"
    $opts += "-T$TargetOS"

    # Mode
    $opts += '-MObjFPC'

    if (-not $IsDebug) {
        # Release optimizations
        $opts += '-O1'

        # Instruction set from what the CPU HAS. Audio does NOT decide it, on any platform.
        #
        # The old rule skipped these flags whenever audio was on, citing an "SDL2 audio API
        # conflict" (a one-line note from 5 Jan 2026, never verified). Since sb and sbv are the
        # audio targets AND sb is the first one built, that rule left every shared unit - the
        # whole engine - at the SSE2 baseline.
        #
        # Measured 12 Aug 2026 on Linux: sb built with audio AND -CpCOREAVX2 -OpCOREAVX2 -CfAVX2
        # compiles, links and runs, with 1352 AVX instructions in SedaiBytecodeVM alone and 34
        # FMA in the binary. No conflict. build.sh does the same thing: the two scripts must
        # behave identically, so the exclusion is gone on both rather than kept on one.
        if ($TargetCPU -eq 'x86_64') {
            switch ($script:CpuOptLevel) {
                'avx2' {
                    $opts += '-CpCOREAVX2'
                    $opts += '-OpCOREAVX2'
                    $opts += '-CfAVX2'
                }
                'avx' {
                    $opts += '-CpCOREAVX'
                    $opts += '-OpCOREAVX'
                    $opts += '-CfAVX'
                }
                default { }   # portable x86-64 baseline (SSE2): nothing to add
            }
        }

        # Additional optimizations
        $opts += '-OoREGVAR'
        $opts += '-OoCSE'
        $opts += '-OoDFA'
        $opts += '-OoFASTMATH'
        $opts += '-OoCONSTPROP'

        # Strip and smart linking
        $opts += '-Xs'
        $opts += '-XX'
    }
    else {
        # Debug options
        $opts += '-g'
        $opts += '-gl'
        $opts += '-gw'
        $opts += '-Ci'
        $opts += '-Cr'
        $opts += '-Co'
    }

    # Paths - $libSubDir already set above for WEB_MODE separation
    $opts += "-Fusrc"
    $opts += "-Fulib\$libSubDir"
    $opts += "-FUlib\$libSubDir"
    $opts += "-FEbin\$PlatformDir"

    # Extra unit paths
    foreach ($extraPath in $ExtraUnitPaths) {
        $opts += "-Fu$extraPath"
    }

    # Debug defines (from -DebugFlags parameter)
    foreach ($define in $DebugDefines) {
        $opts += "-d$define"
    }

    # Web mode (sbw target)
    if ($IsWeb) {
        $opts += '-dWEB_MODE'
    }

    # SedaiAudio integration
    if ($WithAudio -and $AudioPath) {
        $audioSrcPath = Join-Path $AudioPath 'src'
        # SDL2 path: use config if set, otherwise default to deps\sdl2
        $sdl2Path = if ($UserConfig.SDL2Path) { $UserConfig.SDL2Path } else { Join-Path $ProjectRoot 'deps\sdl2' }
        # SAF main src folder
        $opts += "-Fu`"$audioSrcPath`""
        # SAF subdirectories (new structure)
        $opts += "-Fu`"$audioSrcPath\Core`""
        $opts += "-Fu`"$audioSrcPath\Platform`""
        $opts += "-Fu`"$audioSrcPath\Generators`""
        $opts += "-Fu`"$audioSrcPath\Modulators`""
        $opts += "-Fu`"$audioSrcPath\Processors`""
        $opts += "-Fu`"$audioSrcPath\Effects`""
        $opts += "-Fu`"$audioSrcPath\Voice`""
        $opts += "-Fu`"$audioSrcPath\Mixer`""
        $opts += "-Fu`"$audioSrcPath\Transport`""
        $opts += "-Fu`"$audioSrcPath\SID`""
        $opts += "-Fu`"$audioSrcPath\Players`""
        $opts += "-Fu`"$audioSrcPath\FileIO`""
        $opts += "-Fu`"$audioSrcPath\Engine`""
        $opts += "-Fu`"$audioSrcPath\Wavetable`""
        $opts += "-Fu`"$audioSrcPath\Project`""
        # SDL2 bindings
        $opts += "-Fu`"$sdl2Path`""
        # Enable audio support flag
        $opts += '-dWITH_SEDAI_AUDIO'
    }

    # Build command line
    $cmdArgs = $opts + @($srcPath)

    Write-Host "  Building $OutputName..." -ForegroundColor White -NoNewline

    # Execute compiler using System.Diagnostics.Process for proper output capture
    $pinfo = New-Object System.Diagnostics.ProcessStartInfo
    $pinfo.FileName = $FPC
    $pinfo.Arguments = $cmdArgs -join ' '
    $pinfo.RedirectStandardOutput = $true
    $pinfo.RedirectStandardError = $true
    $pinfo.UseShellExecute = $false
    $pinfo.CreateNoWindow = $true
    $pinfo.WorkingDirectory = $ProjectRoot

    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = $pinfo
    $process.Start() | Out-Null
    $stdout = $process.StandardOutput.ReadToEnd()
    $stderr = $process.StandardError.ReadToEnd()
    $process.WaitForExit()

    if ($process.ExitCode -eq 0) {
        Write-Host " OK" -ForegroundColor Green
        Write-Host "    units: lib\$libSubDir" -ForegroundColor Gray
        Test-InstructionSet -Binary (Join-Path $binPath $OutputName) -IsDebug $IsDebug -TargetCPU $TargetCPU
        return $true
    } else {
        Write-Host " FAILED" -ForegroundColor Red
        if ($stderr) { Write-Host $stderr -ForegroundColor DarkRed }
        if ($stdout) { Write-Host $stdout -ForegroundColor DarkRed }
        return $false
    }
}

# Clean build artifacts
function Clean-Build {
    param([string]$PlatformDir)

    Write-Host "Cleaning build artifacts..." -ForegroundColor Yellow

    $binPath = Join-Path $Script:BinDir $PlatformDir

    # Every configuration variant: lib\<platform>, -web, -audio, -avx2, -debug, ...
    Get-ChildItem -Path $Script:LibDir -Directory -Filter "$PlatformDir*" -ErrorAction SilentlyContinue |
        ForEach-Object {
            Get-ChildItem -Path $_.FullName -File | Remove-Item -Force -ErrorAction SilentlyContinue
            Write-Host "  Cleaned: $($_.FullName)" -ForegroundColor Gray
        }

    # Don't delete executables, just .ppu/.o files
    if (Test-Path $binPath) {
        Remove-Item -Path "$binPath\*.ppu" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$binPath\*.o" -Force -ErrorAction SilentlyContinue
        Write-Host "  Cleaned: $binPath (units only)" -ForegroundColor Gray
    }
}

# Main script
if (-not $NoBanner) {
    Write-Host ""
    Write-Host "SedaiBasic Build System" -ForegroundColor Cyan
    Write-Host "=======================" -ForegroundColor Cyan
    Write-Host ""
}

# Load configuration from setup.config.json if exists
if (Load-BuildConfig) {
    Write-Host "Config: setup.config.json loaded" -ForegroundColor DarkGray
}

# Find FPC
$fpc = Find-FPC
if (-not $fpc) {
    Write-Host "ERROR: Free Pascal Compiler (fpc) not found!" -ForegroundColor Red
    Write-Host "Please install FPC or add it to PATH." -ForegroundColor Yellow
    exit 1
}
Write-Host "Compiler: FPC $(& $fpc -iV 2>$null)" -ForegroundColor Gray
Write-Host "Platform: $(Get-PlatformDir -cpu $CPU -os $OS)" -ForegroundColor Gray
Write-Host "Mode: $(if ($Debug) { 'Debug' } else { 'Release' })" -ForegroundColor Gray

# Get platform
$platformDir = Get-PlatformDir -cpu $CPU -os $OS

# Detect SedaiAudio
$audioResult = Find-SedaiAudio -RequestedPath $WithSedaiAudio
$Script:SedaiAudioEnabled = $audioResult.Enabled
$Script:SedaiAudioPath = $audioResult.Path

if ($SedaiAudioEnabled) {
    Write-Host "SedaiAudio: ENABLED" -ForegroundColor Green
} else {
    Write-Host "SedaiAudio: disabled ($($audioResult.Reason))" -ForegroundColor Gray
}

# Decide the instruction set ONCE and say so: a binary built for a CPU feature the machine lacks
# fails as an illegal instruction at run time, with nothing in the build output to explain it.
$Script:CpuOptLevel = Get-CpuOptLevel
switch ($Script:CpuOptLevel) {
    'avx2'  { Write-Host "CPU opt:    AVX2 + FMA" -ForegroundColor Green }
    'avx'   { Write-Host "CPU opt:    AVX (no AVX2/FMA on this CPU)" -ForegroundColor Green }
    default { Write-Host "CPU opt:    baseline x86-64 (SSE2) - no AVX detected, or PowerShell 5.1" -ForegroundColor Gray }
}
# The level is a property of the machine that COMPILES, not of the project. The flags reach the
# shared units, so binaries built with them really do carry those instructions - and die with an
# illegal instruction on a CPU that lacks them. Only reachable now by asking for it explicitly.
if ($Script:CpuOptLevel -ne 'none') {
    Write-Host "            FORCED via SEDAI_CPUOPT - binaries will not run on a CPU without it" -ForegroundColor Gray
    Write-Host "            (measured 15 Aug: worth 0% here, FPC emits no vector code)" -ForegroundColor Gray
}
Write-Host ""

# Clean if requested
if ($Clean) {
    Clean-Build -PlatformDir $platformDir
    Write-Host ""
}

# Define targets with their extra dependencies
# SupportsAudio: targets that can use SedaiAudioFoundation (sb and sbv)
# SDL2 path for sbv: use config if set, otherwise default
$sdl2PathForTargets = if ($UserConfig.SDL2Path) { $UserConfig.SDL2Path } else { '.\deps\sdl2' }
$targets = @{
    'sb'  = @{ Lpr = 'SedaiBasicVM.lpr';           Output = 'sb';  ExtraPaths = @(); SupportsAudio = $true;  IsWeb = $false }
    'sbc' = @{ Lpr = 'SedaiBasicCompiler.lpr';     Output = 'sbc'; ExtraPaths = @(); SupportsAudio = $false; IsWeb = $false }
    'sbd' = @{ Lpr = 'SedaiBasicDisassembler.lpr'; Output = 'sbd'; ExtraPaths = @(); SupportsAudio = $false; IsWeb = $false }
    'sbv' = @{ Lpr = 'SedaiVision.lpr';            Output = 'sbv'; ExtraPaths = @($sdl2PathForTargets); SupportsAudio = $true; IsWeb = $false }
    'sbw' = @{ Lpr = 'SedaiBasicWeb.lpr';          Output = 'sbw'; ExtraPaths = @(); SupportsAudio = $false; IsWeb = $true }
}

# Add .exe extension on Windows
if ($OS -match 'win') {
    foreach ($key in @($targets.Keys)) {
        $targets[$key].Output += '.exe'
    }
}

# Build targets
$buildTargets = if ($Target -eq 'all') { @('sb', 'sbc', 'sbd', 'sbv', 'sbw') } else { @($Target) }
$success = 0
$failed = 0

# Parse debug flags into defines
$debugDefines = @()
if ($DebugFlags -and $DebugFlags -ne '') {
    $flags = $DebugFlags.ToUpper() -split ','
    foreach ($flag in $flags) {
        $flag = $flag.Trim()
        if ($flag -eq 'ALL') {
            $debugDefines += 'DEBUG_ALL'
        } else {
            $debugDefines += "DEBUG_$flag"
        }
    }
    Write-Host "Debug flags: $($debugDefines -join ', ')" -ForegroundColor Magenta
}

# Optional SDL2 window presenter for the CLI VM (sb --window)
if ($Window) {
    $debugDefines += 'WITH_WINDOW'
    Write-Host "Window presenter: ENABLED (sb --window available)" -ForegroundColor Magenta
}

Write-Host "Building Targets..." -ForegroundColor Cyan
Write-Host "===================" -ForegroundColor Cyan

foreach ($t in $buildTargets) {
    $info = $targets[$t]

    # Enable audio only for targets that support it
    $useAudio = $SedaiAudioEnabled -and $info.SupportsAudio

    $result = Build-Target -LprFile $info.Lpr -OutputName $info.Output `
        -FPC $fpc -PlatformDir $platformDir `
        -TargetCPU $CPU -TargetOS $OS `
        -IsDebug $Debug -ExtraUnitPaths $info.ExtraPaths `
        -WithAudio $useAudio -AudioPath $SedaiAudioPath `
        -DebugDefines $debugDefines -IsWeb $info.IsWeb

    if ($result) { $success++ } else { $failed++ }
}

# Summary
Write-Host ""
Write-Host "Build Summary" -ForegroundColor Cyan
Write-Host "=============" -ForegroundColor Cyan
Write-Host "  Successful: $success" -ForegroundColor Green
if ($failed -gt 0) {
    Write-Host "  Failed: $failed" -ForegroundColor Red
}
Write-Host ""

if ($failed -eq 0) {
    Write-Host "Build completed successfully!" -ForegroundColor Green
} else {
    Write-Host "Build completed with errors." -ForegroundColor Yellow
}

exit $failed
