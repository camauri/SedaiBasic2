<#
.SYNOPSIS
    Downloads and installs SDL2 for Pascal bindings for Windows

.DESCRIPTION
    This script downloads SDL2 for Pascal bindings from the SedaiBasic2-Deps repository
    and extracts them to the deps/sdl2 subfolder of the project root.

    Exit codes:
        0 = Success
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space
        5 = SDL2 already installed (skipped)

.PARAMETER Force
    Overwrite existing SDL2 installation

.PARAMETER SkipVerify
    Skip SHA256 hash verification

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.EXAMPLE
    .\install-sdl2.ps1

.EXAMPLE
    .\install-sdl2.ps1 -Force -Quiet
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
$SDL2_VERSION = "2.3"
$DOWNLOAD_URL = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/SDL2-for-Pascal-v2.3/SDL2-for-Pascal-v2.3.zip"
$EXPECTED_HASH = "829dd68bebfe7756bf037160e7cc268c115976d640480d73ebb8badaa46a9e47"
$REQUIRED_SPACE_MB = 50  # Approximate space needed

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$DepsDir = Join-Path $ProjectRoot "deps"
$SDL2Dir = Join-Path $DepsDir "sdl2"
$SDL2Marker = Join-Path $SDL2Dir "sdl2.pas"  # File to check for existing installation
$TempDir = Join-Path $env:TEMP "sdl2-install"
$ZipFile = Join-Path $TempDir "SDL2-for-Pascal-v$SDL2_VERSION.zip"

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
function Install-SDL2 {
    Write-Status "============================================" -Color Cyan
    Write-Status "  SDL2 for Pascal v$SDL2_VERSION Installer" -Color Cyan
    Write-Status "  Target: deps/sdl2" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    if (Test-Path $SDL2Marker) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
            Write-Status "Removing existing installation..." -Color Yellow
            Remove-Item -Path $SDL2Dir -Recurse -Force -ErrorAction SilentlyContinue
        } else {
            Write-Success "SDL2 for Pascal v$SDL2_VERSION already installed at: $SDL2Dir"
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

    # Ensure deps directory exists
    if (!(Test-Path $DepsDir)) {
        New-Item -ItemType Directory -Path $DepsDir -Force | Out-Null
    }

    # Step 5: Download
    Write-Step "Downloading SDL2 for Pascal v$SDL2_VERSION..."
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
    Write-Step "Extracting to: $SDL2Dir"

    $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $DepsDir -Quiet:$Quiet
    if ($extractResult.Status -ne 0) {
        Write-Error $extractResult.Message
        return $EXIT_EXTRACTION_ERROR
    }
    Write-Success $extractResult.Message

    # Step 8: Verify installation
    Write-Step "Verifying installation..."

    if (!(Test-Path $SDL2Marker)) {
        # Check if files are in a subdirectory
        $possibleMarker = Get-ChildItem -Path $DepsDir -Recurse -Filter "sdl2.pas" | Select-Object -First 1
        if ($possibleMarker) {
            Write-Status "Found sdl2.pas at: $($possibleMarker.FullName)" -Color Gray
        } else {
            Write-Error "sdl2.pas not found after extraction"
            return $EXIT_EXTRACTION_ERROR
        }
    }

    Write-Success "SDL2 Pascal bindings verified"

    # Step 9: Cleanup
    Write-Step "Cleaning up..."
    Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
    Write-Status "Temporary files removed." -Color Gray

    # Done
    Write-Status "`n============================================" -Color Green
    Write-Success "  SDL2 for Pascal v$SDL2_VERSION installed successfully!"
    Write-Status "  Location: $SDL2Dir" -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-SDL2
exit $exitCode
