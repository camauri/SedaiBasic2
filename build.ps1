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

.EXAMPLE
    .\build.ps1                    # Build all targets (release)
    .\build.ps1 -Target sb         # Build only sb
    .\build.ps1 -Debug             # Build with debug info
    .\build.ps1 -Clean             # Clean and rebuild

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
    [string]$OS = 'win64'
)

$ErrorActionPreference = 'Stop'
$Script:ProjectRoot = $PSScriptRoot
$Script:SrcDir = Join-Path $ProjectRoot 'src'
$Script:LibDir = Join-Path $ProjectRoot 'lib'
$Script:BinDir = Join-Path $ProjectRoot 'bin'

# Detect FPC compiler
function Find-FPC {
    # Try common locations
    $locations = @(
        'C:\lazarus-3.6\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\FPC\3.2.2\bin\x86_64-win64\fpc.exe',
        '/usr/bin/fpc',
        '/usr/local/bin/fpc'
    )

    foreach ($loc in $locations) {
        if (Test-Path $loc) {
            return $loc
        }
    }

    # Try PATH
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
        [string[]]$ExtraUnitPaths = @()
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
        if ($TargetCPU -eq 'x86_64') {
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
Write-Host ""

# Clean if requested
if ($Clean) {
    Clean-Build -PlatformDir $platformDir
    Write-Host ""
}

# Define targets with their extra dependencies
$targets = @{
    'sb'  = @{ Lpr = 'SedaiBasicVM.lpr';           Output = 'sb';  ExtraPaths = @() }
    'sbc' = @{ Lpr = 'SedaiBasicCompiler.lpr';     Output = 'sbc'; ExtraPaths = @() }
    'sbd' = @{ Lpr = 'SedaiBasicDisassembler.lpr'; Output = 'sbd'; ExtraPaths = @() }
    'sbv' = @{ Lpr = 'SedaiVision.lpr';            Output = 'sbv'; ExtraPaths = @('.\deps\sdl2') }
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
    $result = Build-Target -LprFile $info.Lpr -OutputName $info.Output `
        -FPC $fpc -PlatformDir $platformDir `
        -TargetCPU $CPU -TargetOS $OS `
        -IsDebug $Debug -ExtraUnitPaths $info.ExtraPaths

    if ($result) { $success++ } else { $failed++ }
}

Write-Host ""
Write-Host "Build complete: $success succeeded, $failed failed" -ForegroundColor $(if ($failed -eq 0) { 'Green' } else { 'Yellow' })
exit $failed
