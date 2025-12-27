<#
.SYNOPSIS
    Build SedaiBasic projects (sb, sbc, sbd, sbv)

.DESCRIPTION
    Cross-platform build script for SedaiBasic.
    Uses direct fpc calls with optimized settings.

.PARAMETER Target
    Which target to build: all, sb, sbc, sbd, sbv (default: all)

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

.EXAMPLE
    .\build.ps1                    # Build all targets (release)
    .\build.ps1 -Target sb         # Build only sb
    .\build.ps1 -Debug             # Build with debug info
    .\build.ps1 -Clean             # Clean and rebuild
    .\build.ps1 -WithSedaiAudio no                    # Build without audio support
    .\build.ps1 -WithSedaiAudio C:\path\to\audio      # Use specific audio path

.NOTES
    Copyright (c) 2025 Maurizio Cammalleri
    Released under GNU GPL v3 or Commercial License
#>

param(
    [ValidateSet('all', 'sb', 'sbc', 'sbd', 'sbv')]
    [string]$Target = 'all',

    [switch]$Debug,
    [switch]$Clean,

    [ValidateSet('x86_64', 'i386', 'aarch64', '')]
    [string]$CPU = 'x86_64',

    [ValidateSet('win64', 'win32', 'linux', 'darwin', '')]
    [string]$OS = 'win64',

    # SedaiAudio integration
    # Values: '' (auto-detect), 'no' (disabled), or path to SedaiAudioFoundation
    [string]$WithSedaiAudio = ''
)

$ErrorActionPreference = 'Stop'
$Script:ProjectRoot = $PSScriptRoot
$Script:SrcDir = Join-Path $ProjectRoot 'src'
$Script:LibDir = Join-Path $ProjectRoot 'lib'
$Script:BinDir = Join-Path $ProjectRoot 'bin'

# SedaiAudio detection result (set later)
$Script:SedaiAudioPath = $null
$Script:SedaiAudioEnabled = $false

# Detect FPC compiler
function Find-FPC {
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

    # Explicitly disabled
    if ($RequestedPath -eq 'no') {
        return @{ Enabled = $false; Path = $null; Reason = 'Disabled via -WithSedaiAudio=no' }
    }

    # Explicit path provided
    if ($RequestedPath -and $RequestedPath -ne '') {
        $srcPath = Join-Path $RequestedPath 'src'
        if (Test-Path (Join-Path $srcPath 'sedaiaudiofoundation.pas')) {
            return @{ Enabled = $true; Path = $RequestedPath; Reason = "Explicit path: $RequestedPath" }
        } else {
            Write-Host "WARNING: SedaiAudioFoundation not found at: $RequestedPath" -ForegroundColor Yellow
            return @{ Enabled = $false; Path = $null; Reason = "Not found at: $RequestedPath" }
        }
    }

    # Auto-detect: check deps/ first
    $depsPath = Join-Path $ProjectRoot 'deps\SedaiAudioFoundation'
    if (Test-Path (Join-Path $depsPath 'src\sedaiaudiofoundation.pas')) {
        return @{ Enabled = $true; Path = $depsPath; Reason = 'Found in deps/' }
    }

    # Auto-detect: check default development path
    $defaultPath = '..\SedaiAudioFoundation'
    if (Test-Path (Join-Path $defaultPath 'src\sedaiaudiofoundation.pas')) {
        return @{ Enabled = $true; Path = $defaultPath; Reason = "Found at default: $defaultPath" }
    }

    # Not found
    return @{ Enabled = $false; Path = $null; Reason = 'Not found (use -WithSedaiAudio=<path> to specify)' }
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
        [string]$AudioPath = ''
    )

    $srcPath = Join-Path $SrcDir $LprFile
    if (-not (Test-Path $srcPath)) {
        Write-Host "ERROR: Source file not found: $srcPath" -ForegroundColor Red
        return $false
    }

    # Create output directories
    $libPath = Join-Path $LibDir $PlatformDir
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

        # CPU-specific optimizations (for x86_64)
        # AVX2 disabled when audio is enabled (SDL2 audio API conflict)
        if ($TargetCPU -eq 'x86_64' -and -not $WithAudio) {
            $opts += '-CpCOREAVX2'
            $opts += '-OpCOREAVX2'
            $opts += '-CfAVX2'
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

    # Paths
    $opts += "-Fusrc"
    $opts += "-Fulib\$PlatformDir"
    $opts += "-FUlib\$PlatformDir"
    $opts += "-FEbin\$PlatformDir"

    # Extra unit paths
    foreach ($extraPath in $ExtraUnitPaths) {
        $opts += "-Fu$extraPath"
    }

    # SedaiAudio integration
    if ($WithAudio -and $AudioPath) {
        $audioSrcPath = Join-Path $AudioPath 'src'
        $audioSdl2Path = Join-Path $AudioPath 'deps\sdl2'
        $opts += "-Fu`"$audioSrcPath`""
        $opts += "-Fu`"$audioSdl2Path`""
        $opts += '-dWITH_SEDAI_AUDIO'
    }

    # Build command line
    $cmdArgs = $opts + @($srcPath)

    Write-Host "Building $OutputName..." -ForegroundColor Cyan
    Write-Host "  $FPC $($cmdArgs -join ' ')" -ForegroundColor DarkGray

    $process = Start-Process -FilePath $FPC -ArgumentList $cmdArgs -NoNewWindow -Wait -PassThru -WorkingDirectory $ProjectRoot

    if ($process.ExitCode -eq 0) {
        Write-Host "  OK: $binPath\$OutputName" -ForegroundColor Green
        return $true
    } else {
        Write-Host "  FAILED (exit code: $($process.ExitCode))" -ForegroundColor Red
        return $false
    }
}

# Clean build artifacts
function Clean-Build {
    param([string]$PlatformDir)

    Write-Host "Cleaning build artifacts..." -ForegroundColor Yellow

    $libPath = Join-Path $LibDir $PlatformDir
    $binPath = Join-Path $BinDir $PlatformDir

    if (Test-Path $libPath) {
        Remove-Item -Path "$libPath\*" -Force -ErrorAction SilentlyContinue
        Write-Host "  Cleaned: $libPath" -ForegroundColor Gray
    }

    # Don't delete executables, just .ppu/.o files
    if (Test-Path $binPath) {
        Remove-Item -Path "$binPath\*.ppu" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$binPath\*.o" -Force -ErrorAction SilentlyContinue
        Write-Host "  Cleaned: $binPath (units only)" -ForegroundColor Gray
    }
}

# Main script
Write-Host ""
Write-Host "SedaiBasic Build System" -ForegroundColor Cyan
Write-Host "=======================" -ForegroundColor Cyan
Write-Host ""

# Find FPC
$fpc = Find-FPC
if (-not $fpc) {
    Write-Host "ERROR: Free Pascal Compiler (fpc) not found!" -ForegroundColor Red
    Write-Host "Please install FPC or add it to PATH." -ForegroundColor Yellow
    exit 1
}
Write-Host "Using FPC: $fpc" -ForegroundColor Gray

# Get platform
$platformDir = Get-PlatformDir -cpu $CPU -os $OS
Write-Host "Platform: $platformDir" -ForegroundColor Gray

# Detect SedaiAudio
$audioResult = Find-SedaiAudio -RequestedPath $WithSedaiAudio
$Script:SedaiAudioEnabled = $audioResult.Enabled
$Script:SedaiAudioPath = $audioResult.Path

if ($SedaiAudioEnabled) {
    Write-Host "SedaiAudio: ENABLED ($($audioResult.Reason))" -ForegroundColor Green
} else {
    Write-Host "SedaiAudio: disabled ($($audioResult.Reason))" -ForegroundColor Gray
}
Write-Host ""

# Clean if requested
if ($Clean) {
    Clean-Build -PlatformDir $platformDir
    Write-Host ""
}

# Define targets with their extra dependencies
# SupportsAudio: targets that can use SedaiAudioFoundation (sb and sbv)
$targets = @{
    'sb'  = @{ Lpr = 'SedaiBasicVM.lpr';           Output = 'sb';  ExtraPaths = @(); SupportsAudio = $true }
    'sbc' = @{ Lpr = 'SedaiBasicCompiler.lpr';     Output = 'sbc'; ExtraPaths = @(); SupportsAudio = $false }
    'sbd' = @{ Lpr = 'SedaiBasicDisassembler.lpr'; Output = 'sbd'; ExtraPaths = @(); SupportsAudio = $false }
    'sbv' = @{ Lpr = 'SedaiVision.lpr';            Output = 'sbv'; ExtraPaths = @('.\deps\sdl2'); SupportsAudio = $true }
}

# Add .exe extension on Windows
if ($OS -match 'win') {
    foreach ($key in @($targets.Keys)) {
        $targets[$key].Output += '.exe'
    }
}

# Build targets
$buildTargets = if ($Target -eq 'all') { @('sb', 'sbc', 'sbd', 'sbv') } else { @($Target) }
$success = 0
$failed = 0

foreach ($t in $buildTargets) {
    $info = $targets[$t]

    # Enable audio only for targets that support it
    $useAudio = $SedaiAudioEnabled -and $info.SupportsAudio

    $result = Build-Target -LprFile $info.Lpr -OutputName $info.Output `
        -FPC $fpc -PlatformDir $platformDir `
        -TargetCPU $CPU -TargetOS $OS `
        -IsDebug $Debug -ExtraUnitPaths $info.ExtraPaths `
        -WithAudio $useAudio -AudioPath $SedaiAudioPath

    if ($result) { $success++ } else { $failed++ }
}

Write-Host ""
Write-Host "Build complete: $success succeeded, $failed failed" -ForegroundColor $(if ($failed -eq 0) { 'Green' } else { 'Yellow' })
exit $failed
