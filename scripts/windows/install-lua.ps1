<#
.SYNOPSIS
    Downloads and installs Lua 5.4 portable for Windows

.DESCRIPTION
    This script downloads Lua 5.4 portable from the SedaiBasic2-Deps repository
    and extracts it to the benchmarks/runtime/lua subfolder of the project root.

    Exit codes:
        0 = Success
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space
        5 = Lua already installed (skipped)

.PARAMETER Force
    Overwrite existing Lua installation

.PARAMETER SkipVerify
    Skip SHA256 hash verification

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.EXAMPLE
    .\install-lua.ps1

.EXAMPLE
    .\install-lua.ps1 -Force -Quiet
#>

param(
    [switch]$Force,
    [switch]$SkipVerify,
    [switch]$Quiet
)

# Exit codes
$EXIT_SUCCESS = 0
$EXIT_NETWORK_ERROR = 1
$EXIT_EXTRACTION_ERROR = 2
$EXIT_HASH_MISMATCH = 3
$EXIT_DISK_SPACE = 4
$EXIT_ALREADY_INSTALLED = 5

# Configuration
$LUA_VERSION = "5.4"
$DOWNLOAD_URL = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/lua-5.4-win64/lua-5.4-win64.zip"
$EXPECTED_HASH = "456da3694774ee64cd63fad459eedeedc4153f3f0cf9bde19ee4985093f0509e"
$REQUIRED_SPACE_MB = 20  # Approximate space needed

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$LuaDir = Join-Path $ProjectRoot "benchmarks\runtime\lua"
$LuaExe = Join-Path $LuaDir "lua54.exe"
$RuntimeDir = Join-Path $ProjectRoot "benchmarks\runtime"
$TempDir = Join-Path $env:TEMP "lua-install"
$ZipFile = Join-Path $TempDir "lua-$LUA_VERSION-win64.zip"

# Import utilities
$UtilsPath = Join-Path $ScriptDir "..\lib\download-utils.ps1"
if (!(Test-Path $UtilsPath)) {
    Write-Host "ERROR: download-utils.ps1 not found at: $UtilsPath" -ForegroundColor Red
    exit $EXIT_EXTRACTION_ERROR
}
. $UtilsPath

# Helper function for output
function Write-Status {
    param([string]$Message, [string]$Color = "White")
    if (!$Quiet) {
        Write-Host $Message -ForegroundColor $Color
    }
}

function Write-Step {
    param([string]$Message)
    Write-Status "`n[$((Get-Date).ToString('HH:mm:ss'))] $Message" -Color Cyan
}

function Write-Success {
    param([string]$Message)
    Write-Status $Message -Color Green
}

function Write-Error {
    param([string]$Message)
    Write-Host "ERROR: $Message" -ForegroundColor Red
}

# Main installation logic
function Install-Lua {
    Write-Status "============================================" -Color Cyan
    Write-Status "  Lua $LUA_VERSION Portable Installer" -Color Cyan
    Write-Status "  Target: x86_64-win64" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    if (Test-Path $LuaExe) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
            Write-Status "Removing existing installation..." -Color Yellow
            Remove-Item -Path $LuaDir -Recurse -Force -ErrorAction SilentlyContinue
        } else {
            Write-Success "Lua $LUA_VERSION already installed at: $LuaDir"
            Write-Status "Use -Force to reinstall." -Color Yellow
            return $EXIT_ALREADY_INSTALLED
        }
    } else {
        Write-Status "No existing installation found." -Color Gray
    }

    # Step 2: Check disk space
    Write-Step "Checking disk space..."

    $spaceCheck = Test-DiskSpace -Path $ProjectRoot -RequiredBytes ($REQUIRED_SPACE_MB * 1MB)
    if ($spaceCheck.Status -ne 0) {
        Write-Error $spaceCheck.Message
        return $EXIT_DISK_SPACE
    }
    Write-Success $spaceCheck.Message

    # Step 3: Check internet connection
    Write-Step "Checking internet connection..."

    $netCheck = Test-InternetConnection -TestUrl "https://github.com"
    if ($netCheck.Status -ne 0) {
        Write-Error $netCheck.Message
        return $EXIT_NETWORK_ERROR
    }
    Write-Success $netCheck.Message

    # Step 4: Create temp directory
    Write-Step "Preparing download..."

    if (!(Test-Path $TempDir)) {
        New-Item -ItemType Directory -Path $TempDir -Force | Out-Null
    }

    # Ensure runtime directory exists
    if (!(Test-Path $RuntimeDir)) {
        New-Item -ItemType Directory -Path $RuntimeDir -Force | Out-Null
    }

    # Step 5: Download
    Write-Step "Downloading Lua $LUA_VERSION..."
    Write-Status "URL: $DOWNLOAD_URL" -Color Gray

    $downloadResult = Get-FileWithProgress -Url $DOWNLOAD_URL -OutFile $ZipFile -Quiet:$Quiet
    if ($downloadResult.Status -ne 0) {
        Write-Error $downloadResult.Message
        return $EXIT_NETWORK_ERROR
    }

    $sizeMB = [math]::Round($downloadResult.BytesDownloaded / 1MB, 2)
    Write-Success "Download completed: $sizeMB MB"

    # Step 6: Verify hash
    if (!$SkipVerify) {
        Write-Step "Verifying file integrity (SHA256)..."

        $hashResult = Test-FileHash -FilePath $ZipFile -ExpectedHash $EXPECTED_HASH
        if ($hashResult.Status -ne 0) {
            Write-Error $hashResult.Message
            Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
            return $EXIT_HASH_MISMATCH
        }
        Write-Success $hashResult.Message
    } else {
        Write-Status "Hash verification skipped (-SkipVerify)" -Color Yellow
    }

    # Step 7: Extract
    Write-Step "Extracting to: $LuaDir"

    $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $RuntimeDir -Quiet:$Quiet
    if ($extractResult.Status -ne 0) {
        Write-Error $extractResult.Message
        return $EXIT_EXTRACTION_ERROR
    }
    Write-Success $extractResult.Message

    # Step 8: Verify installation
    Write-Step "Verifying installation..."

    if (!(Test-Path $LuaExe)) {
        # Check if files are in a subdirectory
        $possibleExe = Get-ChildItem -Path $RuntimeDir -Recurse -Filter "lua54.exe" | Select-Object -First 1
        if ($possibleExe) {
            Write-Status "Found lua54.exe at: $($possibleExe.FullName)" -Color Gray
            $LuaExe = $possibleExe.FullName
        } else {
            Write-Error "lua54.exe not found after extraction"
            return $EXIT_EXTRACTION_ERROR
        }
    }

    # Test Lua works
    try {
        $version = & $LuaExe -v 2>&1
        Write-Success "Lua responds: $version"
    } catch {
        Write-Error "Lua installed but not responding: $_"
        return $EXIT_EXTRACTION_ERROR
    }

    # Step 9: Cleanup
    Write-Step "Cleaning up..."
    Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
    Write-Status "Temporary files removed." -Color Gray

    # Done
    Write-Status "`n============================================" -Color Green
    Write-Success "  Lua $LUA_VERSION installed successfully!"
    Write-Status "  Location: $LuaDir" -Color Green
    Write-Status "  Executable: $LuaExe" -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-Lua
exit $exitCode
