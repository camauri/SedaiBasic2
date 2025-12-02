<#
.SYNOPSIS
    SedaiBasic2 Setup Script for Windows

.DESCRIPTION
    This script downloads and installs Free Pascal Compiler (FPC) and builds
    the SedaiBasic2 interpreter (sb.exe).

    Copyright (c) 2025 Maurizio Cammalleri
    Released under GNU GPL v3

.PARAMETER Help
    Show help message

.PARAMETER FpcOnly
    Download and install FPC only, do not compile SedaiBasic2

.PARAMETER BuildOnly
    Compile SedaiBasic2 only, skip FPC installation (requires FPC already installed)

.PARAMETER ForceFpc
    Force reinstallation of FPC even if already present

.PARAMETER Clean
    Clean build directories before compilation

.EXAMPLE
    .\setup.ps1

.EXAMPLE
    .\setup.ps1 -FpcOnly

.EXAMPLE
    .\setup.ps1 -BuildOnly

.EXAMPLE
    .\setup.ps1 -ForceFpc -Clean
#>

param(
    [switch]$Help,
    [switch]$FpcOnly,
    [switch]$BuildOnly,
    [switch]$ForceFpc,
    [switch]$Clean
)

# ============================================================================
#  CONFIGURATION
# ============================================================================

$Script:ProjectRoot = $PSScriptRoot
$Script:FpcVersion = "3.2.2"
$Script:FpcArch = "x86_64-win64"
$Script:FpcDir = Join-Path $ProjectRoot "fpc\$FpcVersion"
$Script:FpcExe = Join-Path $FpcDir "bin\$FpcArch\fpc.exe"
$Script:OutputExe = "sb.exe"
$Script:SourceFile = "src\SedaiBasicVM.lpr"
$Script:BinDir = "bin\$FpcArch"
$Script:LibDir = "lib\$FpcArch"

# ============================================================================
#  DISPLAY FUNCTIONS
# ============================================================================

function Show-Help {
    Write-Host ""
    Write-Host "SedaiBasic2 Setup Script" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "USAGE:" -ForegroundColor Yellow
    Write-Host "    .\setup.ps1 [options]"
    Write-Host ""
    Write-Host "OPTIONS:" -ForegroundColor Yellow
    Write-Host "    -Help           Show this help message"
    Write-Host "    -FpcOnly        Download and install FPC only, do not compile"
    Write-Host "    -BuildOnly      Compile SedaiBasic2 only, skip FPC installation"
    Write-Host "    -ForceFpc       Force reinstallation of FPC even if present"
    Write-Host "    -Clean          Clean build directories before compilation"
    Write-Host ""
    Write-Host "EXAMPLES:" -ForegroundColor Yellow
    Write-Host "    .\setup.ps1                  # Full setup: install FPC + compile"
    Write-Host "    .\setup.ps1 -FpcOnly         # Only download and install FPC"
    Write-Host "    .\setup.ps1 -BuildOnly       # Only compile (FPC must be installed)"
    Write-Host "    .\setup.ps1 -Clean           # Clean build and recompile"
    Write-Host "    .\setup.ps1 -ForceFpc        # Force FPC reinstallation"
    Write-Host ""
    Write-Host "OUTPUT:" -ForegroundColor Yellow
    Write-Host "    FPC is installed to: fpc\$FpcVersion\"
    Write-Host "    Executable output:   $BinDir\$OutputExe"
    Write-Host ""
}

function Show-Banner {
    $width = 70
    $border = "=" * $width

    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "   ____           _       _ ____            _      ____  " -ForegroundColor White
    Write-Host "  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \ " -ForegroundColor White
    Write-Host "  \___ \ / _ \/ _`` |/ _`` | |  _ \ / _`` / __| |/ __| __) |" -ForegroundColor White
    Write-Host "   ___) |  __/ (_| | (_| | | |_) | (_| \__ \ | (__ / __/ " -ForegroundColor White
    Write-Host "  |____/ \___|\__,_|\__,_|_|____/ \__,_|___/_|\___|_____|" -ForegroundColor White
    Write-Host ""
    Write-Host "                   SETUP SCRIPT" -ForegroundColor Yellow
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  Copyright (c) 2025 Maurizio Cammalleri" -ForegroundColor Gray
    Write-Host "  Released under GNU GPL v3" -ForegroundColor Gray
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
}

function Show-Step {
    param(
        [int]$Number,
        [int]$Total,
        [string]$Title
    )

    Write-Host ""
    Write-Host ("  [{0}/{1}] {2}" -f $Number, $Total, $Title) -ForegroundColor Cyan
    Write-Host ("  " + "-" * 60) -ForegroundColor DarkGray
}

function Show-Status {
    param(
        [string]$Message,
        [string]$Type = "Info"
    )

    $prefix = "      "
    switch ($Type) {
        "Success" {
            Write-Host "$prefix[OK] " -ForegroundColor Green -NoNewline
            Write-Host $Message
        }
        "Error" {
            Write-Host "$prefix[ERROR] " -ForegroundColor Red -NoNewline
            Write-Host $Message
        }
        "Warning" {
            Write-Host "$prefix[!] " -ForegroundColor Yellow -NoNewline
            Write-Host $Message
        }
        "Skip" {
            Write-Host "$prefix[SKIP] " -ForegroundColor DarkYellow -NoNewline
            Write-Host $Message
        }
        default {
            Write-Host "$prefix" -NoNewline
            Write-Host $Message -ForegroundColor Gray
        }
    }
}

function Show-Summary {
    param(
        [bool]$Success,
        [string]$ExePath,
        [string]$Mode = "full"
    )

    $width = 70
    $border = "=" * $width

    Write-Host ""
    Write-Host $border -ForegroundColor $(if ($Success) { "Green" } else { "Red" })
    Write-Host ""

    if ($Success) {
        switch ($Mode) {
            "fpc" {
                Write-Host "  FPC INSTALLATION COMPLETED!" -ForegroundColor Green
                Write-Host ""
                Write-Host "  Free Pascal Compiler has been installed successfully." -ForegroundColor White
                Write-Host ""
                Write-Host "  Location: " -NoNewline -ForegroundColor Gray
                Write-Host $FpcExe -ForegroundColor Yellow
                Write-Host ""
                Write-Host "  To compile SedaiBasic2, run:" -ForegroundColor Gray
                Write-Host "    .\setup.ps1 -BuildOnly" -ForegroundColor White
            }
            "build" {
                Write-Host "  BUILD COMPLETED!" -ForegroundColor Green
                Write-Host ""
                Write-Host "  SedaiBasic2 has been compiled successfully." -ForegroundColor White
                Write-Host ""
                Write-Host "  Executable: " -NoNewline -ForegroundColor Gray
                Write-Host $ExePath -ForegroundColor Yellow
                Write-Host ""
                Write-Host "  Usage:" -ForegroundColor Gray
                Write-Host "    .\$BinDir\$OutputExe program.bas" -ForegroundColor White
                Write-Host "    .\$BinDir\$OutputExe --help" -ForegroundColor White
            }
            default {
                Write-Host "  SETUP COMPLETED SUCCESSFULLY!" -ForegroundColor Green
                Write-Host ""
                Write-Host "  SedaiBasic2 has been built successfully." -ForegroundColor White
                Write-Host ""
                Write-Host "  Executable: " -NoNewline -ForegroundColor Gray
                Write-Host $ExePath -ForegroundColor Yellow
                Write-Host ""
                Write-Host "  Usage:" -ForegroundColor Gray
                Write-Host "    .\$BinDir\$OutputExe program.bas" -ForegroundColor White
                Write-Host "    .\$BinDir\$OutputExe --help" -ForegroundColor White
            }
        }
    } else {
        Write-Host "  SETUP FAILED!" -ForegroundColor Red
        Write-Host ""
        Write-Host "  Please check the error messages above." -ForegroundColor White
    }

    Write-Host ""
    Write-Host $border -ForegroundColor $(if ($Success) { "Green" } else { "Red" })
    Write-Host ""
}

# ============================================================================
#  MAIN SETUP LOGIC
# ============================================================================

function Install-Fpc {
    $installScript = Join-Path $ProjectRoot "scripts\windows\install-fpc.ps1"

    if (!(Test-Path $installScript)) {
        Show-Status "FPC install script not found: $installScript" -Type "Error"
        return $false
    }

    Show-Status "Running FPC installer..."

    $params = @{}
    if ($ForceFpc) { $params["Force"] = $true }

    & $installScript @params
    $exitCode = $LASTEXITCODE

    switch ($exitCode) {
        0 {
            Show-Status "FPC installed successfully" -Type "Success"
            return $true
        }
        5 {
            Show-Status "FPC already installed" -Type "Success"
            return $true
        }
        default {
            Show-Status "FPC installation failed (exit code: $exitCode)" -Type "Error"
            return $false
        }
    }
}

function Test-FpcInstallation {
    if (!(Test-Path $FpcExe)) {
        return $false
    }

    try {
        $version = & $FpcExe -iV 2>&1
        return $true
    } catch {
        return $false
    }
}

function Initialize-BuildDirs {
    $binPath = Join-Path $ProjectRoot $BinDir
    $libPath = Join-Path $ProjectRoot $LibDir

    if ($Clean) {
        Show-Status "Cleaning build directories..."
        if (Test-Path $binPath) { Remove-Item -Path $binPath -Recurse -Force -ErrorAction SilentlyContinue }
        if (Test-Path $libPath) { Remove-Item -Path $libPath -Recurse -Force -ErrorAction SilentlyContinue }
    }

    if (!(Test-Path $binPath)) {
        New-Item -ItemType Directory -Path $binPath -Force | Out-Null
        Show-Status "Created: $BinDir"
    }

    if (!(Test-Path $libPath)) {
        New-Item -ItemType Directory -Path $libPath -Force | Out-Null
        Show-Status "Created: $LibDir"
    }

    return $true
}

function Build-SedaiBasic {
    $sourcePath = Join-Path $ProjectRoot $SourceFile

    if (!(Test-Path $sourcePath)) {
        Show-Status "Source file not found: $SourceFile" -Type "Error"
        return $false
    }

    Show-Status "Compiling SedaiBasic2..."
    Show-Status "Source: $SourceFile"
    Show-Status "Output: $BinDir\$OutputExe"

    $compilerArgs = @(
        "-o$OutputExe"
        "-Px86_64"
        "-Twin64"
        "-MObjFPC"
        "-Scghi"
        "-CX"
        "-Si"
        "-O3"
        "-CpCOREAVX2"
        "-OpCOREAVX2"
        "-CfAVX2"
        "-OoREGVAR"
        "-OoCSE"
        "-OoDFA"
        "-OoFASTMATH"
        "-OoCONSTPROP"
        "-Xs"
        "-XX"
        "-ve"           # Show only errors (suppress warnings)
        "-Fusrc"
        "-Fu$LibDir"
        "-FU$LibDir"
        "-FE$BinDir"
        $SourceFile
    )

    Push-Location $ProjectRoot
    try {
        Write-Host ""
        $output = & $FpcExe @compilerArgs 2>&1
        $exitCode = $LASTEXITCODE

        # Show compiler output (filtered for important lines)
        $output | ForEach-Object {
            $line = $_
            if ($line -match "Error|Fatal|Linking|lines compiled") {
                if ($line -match "Error|Fatal") {
                    Write-Host "      $line" -ForegroundColor Red
                } elseif ($line -match "Linking") {
                    Write-Host "      $line" -ForegroundColor Green
                } elseif ($line -match "lines compiled") {
                    Write-Host "      $line" -ForegroundColor Green
                }
            }
        }
        Write-Host ""

        if ($exitCode -eq 0) {
            $exePath = Join-Path $ProjectRoot "$BinDir\$OutputExe"
            if (Test-Path $exePath) {
                Show-Status "Build successful!" -Type "Success"
                return $true
            }
        }

        Show-Status "Compilation failed" -Type "Error"
        return $false

    } finally {
        Pop-Location
    }
}

function Invoke-Setup {
    # Handle -Help parameter
    if ($Help) {
        Show-Help
        return 0
    }

    # Validate mutually exclusive options
    if ($FpcOnly -and $BuildOnly) {
        Write-Host ""
        Write-Host "  Error: -FpcOnly and -BuildOnly cannot be used together." -ForegroundColor Red
        Write-Host "  Use -Help for usage information." -ForegroundColor Gray
        Write-Host ""
        return 1
    }

    Show-Banner

    # Show mode
    if ($FpcOnly) {
        Write-Host "  Mode: FPC INSTALLATION ONLY" -ForegroundColor Yellow
    } elseif ($BuildOnly) {
        Write-Host "  Mode: BUILD ONLY (skip FPC installation)" -ForegroundColor Yellow
    } else {
        Write-Host "  Mode: FULL SETUP (FPC + Build)" -ForegroundColor Green
    }
    Write-Host ""

    # Determine steps
    $doFpc = -not $BuildOnly
    $doBuild = -not $FpcOnly

    $totalSteps = 0
    if ($doFpc) { $totalSteps += 2 }  # Install + Verify
    if ($doBuild) { $totalSteps += 1 }  # Build

    $currentStep = 0

    # Step: Install FPC
    if ($doFpc) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing Free Pascal Compiler"

        if (Test-FpcInstallation -and !$ForceFpc) {
            Show-Status "FPC $FpcVersion is already installed" -Type "Skip"
        } else {
            if (!(Install-Fpc)) {
                Show-Summary -Success $false
                return 1
            }
        }

        # Verify FPC Installation
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Verifying FPC Installation"

        if (!(Test-FpcInstallation)) {
            Show-Status "FPC is not properly installed" -Type "Error"
            Show-Status "Expected: $FpcExe" -Type "Error"
            Show-Summary -Success $false
            return 1
        }

        $version = & $FpcExe -iV 2>&1
        Show-Status "FPC Version: $version" -Type "Success"
        Show-Status "Location: $FpcExe"
    }

    # Step: Build SedaiBasic2
    if ($doBuild) {
        # If BuildOnly, verify FPC first
        if ($BuildOnly) {
            if (!(Test-FpcInstallation)) {
                Write-Host ""
                Show-Status "FPC is not installed" -Type "Error"
                Show-Status "Run setup.ps1 without -BuildOnly to install FPC first" -Type "Info"
                Show-Summary -Success $false
                return 1
            }
        }

        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Building SedaiBasic2"

        if (!(Initialize-BuildDirs)) {
            Show-Summary -Success $false
            return 1
        }

        if (!(Build-SedaiBasic)) {
            Show-Summary -Success $false
            return 1
        }
    }

    # Done!
    $exePath = Join-Path $ProjectRoot "$BinDir\$OutputExe"
    $mode = if ($FpcOnly) { "fpc" } elseif ($BuildOnly) { "build" } else { "full" }
    Show-Summary -Success $true -ExePath $exePath -Mode $mode

    return 0
}

# ============================================================================
#  ENTRY POINT
# ============================================================================

$exitCode = Invoke-Setup
exit $exitCode
