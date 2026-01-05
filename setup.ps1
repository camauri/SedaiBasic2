<#
.SYNOPSIS
    SedaiBasic2 Setup Script for Windows

.DESCRIPTION
    Interactive setup script that guides users through dependency configuration.
    Allows using existing installations or downloading official Sedai packages.
    User-provided paths are saved in setup.config.json for future builds.

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

.PARAMETER NonInteractive
    Skip interactive prompts and use saved configuration or defaults

.PARAMETER ResetConfig
    Reset saved configuration and prompt for all paths again

.EXAMPLE
    .\setup.ps1                    # Interactive setup

.EXAMPLE
    .\setup.ps1 -NonInteractive    # Use saved config without prompts

.EXAMPLE
    .\setup.ps1 -ResetConfig       # Reset config and prompt again

.EXAMPLE
    .\setup.ps1 -FpcOnly

.EXAMPLE
    .\setup.ps1 -BuildOnly
#>

param(
    [switch]$Help,
    [switch]$FpcOnly,
    [switch]$BuildOnly,
    [switch]$ForceFpc,
    [switch]$Clean,
    [switch]$NonInteractive,
    [switch]$ResetConfig
)

# ============================================================================
#  CONFIGURATION
# ============================================================================

$Script:ProjectRoot = $PSScriptRoot
$Script:ConfigFile = Join-Path $ProjectRoot "setup.config.json"

# User configuration (loaded from file or set interactively)
$Script:UserConfig = @{
    FpcPath = $null           # Custom FPC path (null = use default/download)
    SDL2Path = $null          # Custom SDL2 bindings path
    RuntimePath = $null       # Custom runtime path
    SedaiAudioPath = $null    # Custom SedaiAudioFoundation path
}
$Script:FpcVersion = "3.2.2"
$Script:FpcArch = "x86_64-win64"
$Script:FpcDir = Join-Path $ProjectRoot "fpc\$FpcVersion"
$Script:FpcExe = Join-Path $FpcDir "bin\$FpcArch\fpc.exe"
$Script:OutputExe = "sb.exe"
$Script:SourceFile = "src\SedaiBasicVM.lpr"
$Script:BinDir = "bin\$FpcArch"
$Script:LibDir = "lib\$FpcArch"

# SDL2 for Pascal paths
$Script:SDL2Dir = Join-Path $ProjectRoot "deps\sdl2"
$Script:SDL2Marker = Join-Path $SDL2Dir "sdl2.pas"

# Runtime paths (SDL2 DLLs, fonts, etc.)
$Script:RuntimeDir = Join-Path $ProjectRoot "bin\$FpcArch"
$Script:RuntimeMarker = Join-Path $RuntimeDir "SDL2.dll"

# SedaiAudioFoundation paths
$Script:SedaiAudioDir = Join-Path $ProjectRoot "deps\SedaiAudioFoundation"
$Script:SedaiAudioMarker = Join-Path $SedaiAudioDir "src\sedaiaudiofoundation.pas"

# ============================================================================
#  CONFIGURATION FILE FUNCTIONS
# ============================================================================

function Load-Config {
    if (Test-Path $ConfigFile) {
        try {
            $json = Get-Content $ConfigFile -Raw | ConvertFrom-Json
            if ($json.FpcPath) { $Script:UserConfig.FpcPath = $json.FpcPath }
            if ($json.SDL2Path) { $Script:UserConfig.SDL2Path = $json.SDL2Path }
            if ($json.RuntimePath) { $Script:UserConfig.RuntimePath = $json.RuntimePath }
            if ($json.SedaiAudioPath) { $Script:UserConfig.SedaiAudioPath = $json.SedaiAudioPath }
            return $true
        } catch {
            Write-Host "  [!] Warning: Could not load config file, using defaults" -ForegroundColor Yellow
            return $false
        }
    }
    return $false
}

function Save-Config {
    try {
        $config = @{
            FpcPath = $Script:UserConfig.FpcPath
            SDL2Path = $Script:UserConfig.SDL2Path
            RuntimePath = $Script:UserConfig.RuntimePath
            SedaiAudioPath = $Script:UserConfig.SedaiAudioPath
            LastUpdated = (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
        }
        $config | ConvertTo-Json -Depth 3 | Set-Content $ConfigFile -Encoding UTF8
        return $true
    } catch {
        Write-Host "  [!] Warning: Could not save config file" -ForegroundColor Yellow
        return $false
    }
}

function Reset-Config {
    if (Test-Path $ConfigFile) {
        Remove-Item $ConfigFile -Force
    }
    $Script:UserConfig = @{
        FpcPath = $null
        SDL2Path = $null
        RuntimePath = $null
        SedaiAudioPath = $null
    }
}

# ============================================================================
#  INTERACTIVE PROMPTS
# ============================================================================

function Prompt-ForPath {
    param(
        [string]$ComponentName,
        [string]$Description,
        [string]$ValidationFile,
        [string]$DefaultDownloadPath,
        [string]$CurrentValue = $null
    )

    Write-Host ""
    Write-Host "  $ComponentName" -ForegroundColor Cyan
    Write-Host "  $Description" -ForegroundColor Gray
    Write-Host ""

    if ($CurrentValue) {
        Write-Host "  Current configuration: " -NoNewline -ForegroundColor Gray
        Write-Host $CurrentValue -ForegroundColor Yellow
        Write-Host ""
    }

    Write-Host "  Options:" -ForegroundColor White
    Write-Host "    [1] Download official Sedai package (Recommended)" -ForegroundColor Green
    Write-Host "    [2] Use existing installation (specify path)" -ForegroundColor White
    if ($CurrentValue) {
        Write-Host "    [3] Keep current configuration" -ForegroundColor DarkGray
    }
    Write-Host ""

    do {
        $maxOption = if ($CurrentValue) { 3 } else { 2 }
        Write-Host "  Choice [1-$maxOption]: " -NoNewline -ForegroundColor Yellow
        $choice = Read-Host

        switch ($choice) {
            "1" {
                return @{ UseDefault = $true; Path = $null }
            }
            "2" {
                Write-Host ""
                Write-Host "  Enter full path: " -NoNewline -ForegroundColor Yellow
                $path = Read-Host

                if ([string]::IsNullOrWhiteSpace($path)) {
                    Write-Host "  [!] Invalid path" -ForegroundColor Red
                    continue
                }

                # Remove quotes if user included them
                $path = $path.Trim('"', "'", ' ')

                # Validate path
                $testPath = if ($ValidationFile) {
                    Join-Path $path $ValidationFile
                } else {
                    $path
                }

                if (Test-Path $testPath) {
                    Write-Host "  [OK] Valid path" -ForegroundColor Green
                    return @{ UseDefault = $false; Path = $path }
                } else {
                    Write-Host "  [!] Path not found: $testPath" -ForegroundColor Red
                    Write-Host "  Use this path anyway? [y/N]: " -NoNewline -ForegroundColor Yellow
                    $confirm = Read-Host
                    if ($confirm -ieq 'y') {
                        return @{ UseDefault = $false; Path = $path }
                    }
                    continue
                }
            }
            "3" {
                if ($CurrentValue) {
                    return @{ UseDefault = $false; Path = $CurrentValue; KeepCurrent = $true }
                } else {
                    Write-Host "  [!] Invalid choice" -ForegroundColor Red
                }
            }
            default {
                Write-Host "  [!] Invalid choice" -ForegroundColor Red
            }
        }
    } while ($true)
}

function Run-InteractiveSetup {
    Write-Host ""
    Write-Host "  ╔════════════════════════════════════════════════════════════════╗" -ForegroundColor Cyan
    Write-Host "  ║            INTERACTIVE DEPENDENCY CONFIGURATION                ║" -ForegroundColor Cyan
    Write-Host "  ╚════════════════════════════════════════════════════════════════╝" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  For each component, you can choose to:" -ForegroundColor White
    Write-Host "    - Download official Sedai package (recommended)" -ForegroundColor Gray
    Write-Host "    - Use an existing installation on your system" -ForegroundColor Gray
    Write-Host ""

    # 1. FPC Configuration
    $fpcResult = Prompt-ForPath `
        -ComponentName "1. FREE PASCAL COMPILER (FPC 3.2.2)" `
        -Description "Compiler required to build SedaiBasic2" `
        -ValidationFile "bin\x86_64-win64\fpc.exe" `
        -DefaultDownloadPath $FpcDir `
        -CurrentValue $UserConfig.FpcPath

    if (-not $fpcResult.UseDefault -and $fpcResult.Path) {
        $Script:UserConfig.FpcPath = $fpcResult.Path
    } elseif ($fpcResult.UseDefault) {
        $Script:UserConfig.FpcPath = $null
    }

    # 2. Runtime Configuration (DLLs, fonts)
    $runtimeResult = Prompt-ForPath `
        -ComponentName "2. RUNTIME (SDL2 DLLs, fonts)" `
        -Description "DLL files and fonts required for execution" `
        -ValidationFile "SDL2.dll" `
        -DefaultDownloadPath $RuntimeDir `
        -CurrentValue $UserConfig.RuntimePath

    if (-not $runtimeResult.UseDefault -and $runtimeResult.Path) {
        $Script:UserConfig.RuntimePath = $runtimeResult.Path
    } elseif ($runtimeResult.UseDefault) {
        $Script:UserConfig.RuntimePath = $null
    }

    # 3. SDL2 Bindings Configuration
    $sdl2Result = Prompt-ForPath `
        -ComponentName "3. SDL2 FOR PASCAL (Binding units)" `
        -Description "Pascal units for SDL2, required for sbv (graphics)" `
        -ValidationFile "sdl2.pas" `
        -DefaultDownloadPath $SDL2Dir `
        -CurrentValue $UserConfig.SDL2Path

    if (-not $sdl2Result.UseDefault -and $sdl2Result.Path) {
        $Script:UserConfig.SDL2Path = $sdl2Result.Path
    } elseif ($sdl2Result.UseDefault) {
        $Script:UserConfig.SDL2Path = $null
    }

    # 4. SedaiAudioFoundation Configuration (optional)
    Write-Host ""
    Write-Host "  4. SEDAIAUDIOFOUNDATION (optional)" -ForegroundColor Cyan
    Write-Host "  Audio library for advanced sound features" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  Options:" -ForegroundColor White
    Write-Host "    [1] Download official Sedai package" -ForegroundColor Green
    Write-Host "    [2] Use existing installation" -ForegroundColor White
    Write-Host "    [3] Do not install (audio disabled)" -ForegroundColor DarkGray
    if ($UserConfig.SedaiAudioPath) {
        Write-Host "    [4] Keep current configuration: $($UserConfig.SedaiAudioPath)" -ForegroundColor DarkGray
    }
    Write-Host ""

    do {
        $maxOption = if ($UserConfig.SedaiAudioPath) { 4 } else { 3 }
        Write-Host "  Choice [1-$maxOption]: " -NoNewline -ForegroundColor Yellow
        $audioChoice = Read-Host

        switch ($audioChoice) {
            "1" {
                $Script:UserConfig.SedaiAudioPath = $null  # Will use default download
                break
            }
            "2" {
                Write-Host ""
                Write-Host "  Enter full path: " -NoNewline -ForegroundColor Yellow
                $path = Read-Host

                # Remove quotes if user included them
                $path = $path.Trim('"', "'", ' ')

                $testFile = Join-Path $path "src\sedaiaudiofoundation.pas"
                if (Test-Path $testFile) {
                    Write-Host "  [OK] Valid path" -ForegroundColor Green
                    $Script:UserConfig.SedaiAudioPath = $path
                } else {
                    Write-Host "  [!] File not found: $testFile" -ForegroundColor Red
                    Write-Host "  Use this path anyway? [y/N]: " -NoNewline -ForegroundColor Yellow
                    $confirm = Read-Host
                    if ($confirm -ieq 'y') {
                        $Script:UserConfig.SedaiAudioPath = $path
                    } else {
                        continue
                    }
                }
                break
            }
            "3" {
                $Script:UserConfig.SedaiAudioPath = "disabled"
                break
            }
            "4" {
                if ($UserConfig.SedaiAudioPath) {
                    # Keep current
                    break
                } else {
                    Write-Host "  [!] Invalid choice" -ForegroundColor Red
                    continue
                }
            }
            default {
                Write-Host "  [!] Invalid choice" -ForegroundColor Red
                continue
            }
        }
        break
    } while ($true)

    # Save configuration
    Write-Host ""
    Write-Host "  Saving configuration..." -ForegroundColor Gray
    if (Save-Config) {
        Write-Host "  [OK] Configuration saved to: setup.config.json" -ForegroundColor Green
    }
    Write-Host ""
}

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
                Write-Host "  Output: " -NoNewline -ForegroundColor Gray
                Write-Host "$BinDir\" -ForegroundColor Yellow
                Write-Host ""
                Write-Host "  See README.md for usage information." -ForegroundColor Gray
            }
            default {
                Write-Host "  SETUP COMPLETED SUCCESSFULLY!" -ForegroundColor Green
                Write-Host ""
                Write-Host "  SedaiBasic2 has been built successfully." -ForegroundColor White
                Write-Host ""
                Write-Host "  Output: " -NoNewline -ForegroundColor Gray
                Write-Host "$BinDir\" -ForegroundColor Yellow
                Write-Host ""
                Write-Host "  See README.md for usage information." -ForegroundColor Gray
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

function Test-SDL2Installation {
    if (!(Test-Path $SDL2Marker)) {
        return $false
    }
    return $true
}

function Install-SDL2 {
    $installScript = Join-Path $ProjectRoot "scripts\windows\install-sdl2.ps1"

    if (!(Test-Path $installScript)) {
        Show-Status "SDL2 install script not found: $installScript" -Type "Error"
        return $false
    }

    Show-Status "Running SDL2 installer..."

    $params = @{}
    if ($ForceSDL2) { $params["Force"] = $true }

    & $installScript @params
    $exitCode = $LASTEXITCODE

    switch ($exitCode) {
        0 {
            Show-Status "SDL2 installed successfully" -Type "Success"
            return $true
        }
        5 {
            Show-Status "SDL2 already installed" -Type "Success"
            return $true
        }
        default {
            Show-Status "SDL2 installation failed (exit code: $exitCode)" -Type "Error"
            return $false
        }
    }
}

function Test-RuntimeInstallation {
    if (!(Test-Path $RuntimeMarker)) {
        return $false
    }
    return $true
}

# Required runtime files
$Script:RuntimeFiles = @(
    'SDL2.dll',
    'SDL2_image.dll',
    'SDL2_ttf.dll',
    'zlib1.dll',
    'freetype.dll',
    'libjpeg-8.dll',
    'libpng16-16.dll'
)
$Script:RuntimeFontDir = 'font'
$Script:RuntimeFontFile = 'PixelOperatorMono8-Bold.ttf'

function Copy-RuntimeFromPath {
    param([string]$SourcePath)

    $destPath = Join-Path $ProjectRoot "bin\x86_64-win64"

    # Create destination directory if needed
    if (!(Test-Path $destPath)) {
        New-Item -ItemType Directory -Path $destPath -Force | Out-Null
    }

    $missingFiles = @()
    $copiedFiles = 0

    # Check and copy DLL files
    foreach ($file in $RuntimeFiles) {
        $srcFile = Join-Path $SourcePath $file
        $dstFile = Join-Path $destPath $file

        if (!(Test-Path $srcFile)) {
            $missingFiles += $file
        } else {
            Copy-Item -Path $srcFile -Destination $dstFile -Force
            $copiedFiles++
        }
    }

    # Check and copy font directory
    $srcFontDir = Join-Path $SourcePath $RuntimeFontDir
    $srcFontFile = Join-Path $srcFontDir $RuntimeFontFile
    $dstFontDir = Join-Path $destPath $RuntimeFontDir
    $dstFontFile = Join-Path $dstFontDir $RuntimeFontFile

    if (!(Test-Path $srcFontFile)) {
        $missingFiles += "$RuntimeFontDir\$RuntimeFontFile"
    } else {
        if (!(Test-Path $dstFontDir)) {
            New-Item -ItemType Directory -Path $dstFontDir -Force | Out-Null
        }
        Copy-Item -Path $srcFontFile -Destination $dstFontFile -Force
        $copiedFiles++
    }

    if ($missingFiles.Count -gt 0) {
        Show-Status "Missing runtime files in source path:" -Type "Error"
        foreach ($f in $missingFiles) {
            Show-Status "  - $f" -Type "Error"
        }
        return $false
    }

    Show-Status "Copied $copiedFiles runtime files to bin\x86_64-win64\" -Type "Success"
    return $true
}

function Install-Runtime {
    $installScript = Join-Path $ProjectRoot "scripts\windows\install-runtime-x86_64.ps1"

    if (!(Test-Path $installScript)) {
        Show-Status "Runtime install script not found: $installScript" -Type "Error"
        return $false
    }

    Show-Status "Running Runtime installer..."

    $params = @{}
    if ($ForceRuntime) { $params["Force"] = $true }

    & $installScript @params
    $exitCode = $LASTEXITCODE

    switch ($exitCode) {
        0 {
            Show-Status "Runtime installed successfully" -Type "Success"
            return $true
        }
        5 {
            Show-Status "Runtime already installed" -Type "Success"
            return $true
        }
        default {
            Show-Status "Runtime installation failed (exit code: $exitCode)" -Type "Error"
            return $false
        }
    }
}

function Test-SedaiAudioInstallation {
    if (!(Test-Path $SedaiAudioMarker)) {
        return $false
    }
    return $true
}

function Install-SedaiAudio {
    $installScript = Join-Path $ProjectRoot "scripts\windows\install-sedaiaudio.ps1"

    if (!(Test-Path $installScript)) {
        Show-Status "SedaiAudio install script not found: $installScript" -Type "Error"
        return $false
    }

    Show-Status "Running SedaiAudioFoundation installer..."

    $params = @{}
    if ($ForceSedaiAudio) { $params["Force"] = $true }

    & $installScript @params
    $exitCode = $LASTEXITCODE

    switch ($exitCode) {
        0 {
            Show-Status "SedaiAudioFoundation installed successfully" -Type "Success"
            return $true
        }
        5 {
            Show-Status "SedaiAudioFoundation already installed" -Type "Success"
            return $true
        }
        default {
            Show-Status "SedaiAudioFoundation installation failed (exit code: $exitCode)" -Type "Error"
            return $false
        }
    }
}

function Initialize-BuildDirs {
    $binPath = Join-Path $ProjectRoot $BinDir
    $libPath = Join-Path $ProjectRoot $LibDir
    $benchmarkDir = Join-Path $ProjectRoot "benchmarks"
    $benchmarkTempDir = Join-Path $benchmarkDir ".temp"
    $benchmarkHistoryDir = Join-Path $benchmarkDir ".history"

    if ($Clean) {
        Show-Status "Cleaning build directories..."
        if (Test-Path $binPath) { Remove-Item -Path $binPath -Recurse -Force -ErrorAction SilentlyContinue }
        if (Test-Path $libPath) { Remove-Item -Path $libPath -Recurse -Force -ErrorAction SilentlyContinue }

        # Clean old benchmark hidden directories (current version)
        if (Test-Path $benchmarkTempDir) {
            Remove-Item -Path $benchmarkTempDir -Recurse -Force -ErrorAction SilentlyContinue
            Show-Status "Removed: benchmarks\.temp"
        }
        if (Test-Path $benchmarkHistoryDir) {
            Remove-Item -Path $benchmarkHistoryDir -Recurse -Force -ErrorAction SilentlyContinue
            Show-Status "Removed: benchmarks\.history"
        }

        # Clean old benchmark hidden directories (legacy version - in project root)
        $legacyTempDir = Join-Path $ProjectRoot ".benchmark_temp"
        $legacyDataDir = Join-Path $ProjectRoot ".benchmark_data"
        if (Test-Path $legacyTempDir) {
            Remove-Item -Path $legacyTempDir -Recurse -Force -ErrorAction SilentlyContinue
            Show-Status "Removed: .benchmark_temp (legacy)"
        }
        if (Test-Path $legacyDataDir) {
            Remove-Item -Path $legacyDataDir -Recurse -Force -ErrorAction SilentlyContinue
            Show-Status "Removed: .benchmark_data (legacy)"
        }
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
    $buildScript = Join-Path $ProjectRoot "build.ps1"

    if (!(Test-Path $buildScript)) {
        Show-Status "Build script not found: $buildScript" -Type "Error"
        return $false
    }

    Show-Status "Building SedaiBasic2 using build.ps1 -Clean..."
    Show-Status "Targets: sb, sbc, sbd, sbv"
    Show-Status "Output: $BinDir\"

    Push-Location $ProjectRoot
    try {
        Write-Host ""

        # Use build.ps1 -Clean to compile all targets (suppress banner as setup shows its own)
        & $buildScript -Clean -NoBanner
        $exitCode = $LASTEXITCODE

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

    # Handle -ResetConfig
    if ($ResetConfig) {
        Write-Host "  Resetting saved configuration..." -ForegroundColor Yellow
        Reset-Config
        Write-Host "  [OK] Configuration reset" -ForegroundColor Green
        Write-Host ""
    }

    # Load existing configuration
    $configLoaded = Load-Config
    if ($configLoaded) {
        Write-Host "  Configuration loaded from: setup.config.json" -ForegroundColor Gray
    }

    # Run interactive setup if not in NonInteractive mode and no config exists (or ResetConfig)
    if (-not $NonInteractive) {
        if (-not $configLoaded -or $ResetConfig) {
            Run-InteractiveSetup
        } else {
            # Config exists, show current config and ask if user wants to change
            Write-Host ""
            Write-Host "  Current configuration:" -ForegroundColor Cyan
            if ($UserConfig.FpcPath) {
                Write-Host "    FPC: $($UserConfig.FpcPath)" -ForegroundColor Gray
            } else {
                Write-Host "    FPC: (official package)" -ForegroundColor Gray
            }
            if ($UserConfig.SDL2Path) {
                Write-Host "    SDL2: $($UserConfig.SDL2Path)" -ForegroundColor Gray
            } else {
                Write-Host "    SDL2: (official package)" -ForegroundColor Gray
            }
            if ($UserConfig.RuntimePath) {
                Write-Host "    Runtime: $($UserConfig.RuntimePath)" -ForegroundColor Gray
            } else {
                Write-Host "    Runtime: (official package)" -ForegroundColor Gray
            }
            if ($UserConfig.SedaiAudioPath -eq "disabled") {
                Write-Host "    SedaiAudio: (disabled)" -ForegroundColor DarkGray
            } elseif ($UserConfig.SedaiAudioPath) {
                Write-Host "    SedaiAudio: $($UserConfig.SedaiAudioPath)" -ForegroundColor Gray
            } else {
                Write-Host "    SedaiAudio: (official package)" -ForegroundColor Gray
            }
            Write-Host ""
            Write-Host "  Do you want to modify the configuration? [y/N]: " -NoNewline -ForegroundColor Yellow
            $change = Read-Host
            if ($change -ieq 'y') {
                Run-InteractiveSetup
            }
        }
    }

    # Update paths based on user configuration
    if ($UserConfig.FpcPath) {
        $Script:FpcExe = Join-Path $UserConfig.FpcPath "bin\$FpcArch\fpc.exe"
        $Script:FpcDir = $UserConfig.FpcPath
    }
    if ($UserConfig.SDL2Path) {
        $Script:SDL2Dir = $UserConfig.SDL2Path
        $Script:SDL2Marker = Join-Path $SDL2Dir "sdl2.pas"
    }
    if ($UserConfig.RuntimePath) {
        $Script:RuntimeDir = $UserConfig.RuntimePath
        $Script:RuntimeMarker = Join-Path $RuntimeDir "SDL2.dll"
    }
    if ($UserConfig.SedaiAudioPath -and $UserConfig.SedaiAudioPath -ne "disabled") {
        $Script:SedaiAudioDir = $UserConfig.SedaiAudioPath
        $Script:SedaiAudioMarker = Join-Path $SedaiAudioDir "src\sedaiaudiofoundation.pas"
    }

    # Show mode
    Write-Host ""
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
    $doSDL2 = -not $FpcOnly         # Install SDL2 bindings unless FpcOnly
    $doRuntime = -not $FpcOnly      # Install runtime (SDL2 DLLs, fonts) unless FpcOnly
    $doSedaiAudio = (-not $FpcOnly) -and ($UserConfig.SedaiAudioPath -ne "disabled")

    # Skip download steps if user provided custom paths
    $skipFpcDownload = ($UserConfig.FpcPath -ne $null)
    $skipSDL2Download = ($UserConfig.SDL2Path -ne $null)
    $skipRuntimeDownload = ($UserConfig.RuntimePath -ne $null)
    $skipSedaiAudioDownload = ($UserConfig.SedaiAudioPath -ne $null)

    $totalSteps = 0
    if ($doFpc) { $totalSteps += 2 }        # Install + Verify
    if ($doRuntime) { $totalSteps += 1 }    # Runtime (DLLs, fonts)
    if ($doSDL2) { $totalSteps += 1 }       # SDL2 bindings
    if ($doSedaiAudio) { $totalSteps += 1 } # SedaiAudio (if not disabled)
    if ($doBuild) { $totalSteps += 1 }      # Build

    $currentStep = 0

    # Step: Install FPC
    if ($doFpc) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing Free Pascal Compiler"

        if ($skipFpcDownload) {
            Show-Status "Using custom FPC path: $($UserConfig.FpcPath)" -Type "Info"
        } elseif (Test-FpcInstallation -and !$ForceFpc) {
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

    # Step: Install Runtime (SDL2 DLLs, fonts)
    if ($doRuntime) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing Runtime (SDL2 DLLs, fonts)"

        if ($skipRuntimeDownload) {
            # Copy runtime files from custom path to bin directory
            Show-Status "Copying runtime from: $($UserConfig.RuntimePath)" -Type "Info"
            if (!(Copy-RuntimeFromPath -SourcePath $UserConfig.RuntimePath)) {
                Show-Summary -Success $false
                return 1
            }
        } elseif (Test-RuntimeInstallation) {
            Show-Status "Runtime is already installed" -Type "Skip"
        } else {
            if (!(Install-Runtime)) {
                Show-Summary -Success $false
                return 1
            }
        }
    }

    # Step: Install SDL2 for Pascal
    if ($doSDL2) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing SDL2 for Pascal"

        if ($skipSDL2Download) {
            Show-Status "Using custom SDL2 path: $($UserConfig.SDL2Path)" -Type "Info"
        } elseif (Test-SDL2Installation) {
            Show-Status "SDL2 for Pascal is already installed" -Type "Skip"
        } else {
            if (!(Install-SDL2)) {
                Show-Summary -Success $false
                return 1
            }
        }
    }

    # Step: Install SedaiAudioFoundation
    if ($doSedaiAudio) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing SedaiAudioFoundation"

        if ($skipSedaiAudioDownload) {
            Show-Status "Using custom SedaiAudio path: $($UserConfig.SedaiAudioPath)" -Type "Info"
        } elseif (Test-SedaiAudioInstallation) {
            Show-Status "SedaiAudioFoundation is already installed" -Type "Skip"
        } else {
            if (!(Install-SedaiAudio)) {
                Show-Status "SedaiAudioFoundation installation failed (audio support disabled)" -Type "Warning"
                # Don't fail the setup, just warn - audio is optional
            }
        }
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
    $mode = if ($FpcOnly) { "fpc" } elseif ($BuildOnly) { "build" } else { "full" }
    Show-Summary -Success $true -Mode $mode

    return 0
}

# ============================================================================
#  ENTRY POINT
# ============================================================================

$exitCode = Invoke-Setup
exit $exitCode
