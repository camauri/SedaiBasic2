<#
.SYNOPSIS
    Downloads and installs SedaiAudioFoundation for Windows

.DESCRIPTION
    This script downloads SedaiAudioFoundation from GitHub repository
    and extracts it to the deps/SedaiAudioFoundation subfolder.

    Exit codes:
        0 = Success
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space
        5 = Already installed (skipped)

.PARAMETER Force
    Overwrite existing installation

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.PARAMETER Branch
    GitHub branch to download (default: main)

.EXAMPLE
    .\install-sedaiaudio.ps1

.EXAMPLE
    .\install-sedaiaudio.ps1 -Force -Quiet
#>

param(
    [switch]$Force,
    [switch]$Quiet,
    [string]$Branch = "main"
)

# Exit codes
$EXIT_SUCCESS = 0
$EXIT_NETWORK_ERROR = 1
$EXIT_EXTRACTION_ERROR = 2
$EXIT_HASH_MISMATCH = 3
$EXIT_DISK_SPACE = 4
$EXIT_ALREADY_INSTALLED = 5

# Configuration
$REPO_NAME = "SedaiAudio"
$DOWNLOAD_URL = "https://github.com/camauri/$REPO_NAME/archive/refs/heads/$Branch.zip"
$REQUIRED_SPACE_MB = 20  # Approximate space needed

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$DepsDir = Join-Path $ProjectRoot "deps"
$TargetDir = Join-Path $DepsDir "SedaiAudioFoundation"
$Marker = Join-Path $TargetDir "src\sedaiaudiofoundation.pas"  # File to check for existing installation
$TempDir = Join-Path $env:TEMP "sedaiaudio-install"
$ZipFile = Join-Path $TempDir "SedaiAudio-$Branch.zip"

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
function Install-SedaiAudio {
    Write-Status "============================================" -Color Cyan
    Write-Status "  SedaiAudioFoundation Installer" -Color Cyan
    Write-Status "  Branch: $Branch" -Color Cyan
    Write-Status "  Target: deps/SedaiAudioFoundation" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    if (Test-Path $Marker) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
            Write-Status "Removing existing installation..." -Color Yellow
            Remove-Item -Path $TargetDir -Recurse -Force -ErrorAction SilentlyContinue
        } else {
            Write-Success "SedaiAudioFoundation already installed at: $TargetDir"
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
    Write-Step "Downloading SedaiAudioFoundation ($Branch branch)..."
    Write-Status "URL: $DOWNLOAD_URL" -Color Gray

    $downloadResult = Get-FileWithProgress -Url $DOWNLOAD_URL -OutFile $ZipFile -Quiet:$Quiet
    if ($downloadResult.Status -ne 0) {
        Write-Error $downloadResult.Message
        return $EXIT_NETWORK_ERROR
    }

    $sizeMB = [math]::Round($downloadResult.BytesDownloaded / 1MB, 2)
    Write-Success "Download completed: $sizeMB MB"

    # Step 6: Extract to temp location first
    Write-Step "Extracting..."

    $extractTempDir = Join-Path $TempDir "extracted"
    if (Test-Path $extractTempDir) {
        Remove-Item -Path $extractTempDir -Recurse -Force -ErrorAction SilentlyContinue
    }

    $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $extractTempDir -Quiet:$Quiet
    if ($extractResult.Status -ne 0) {
        Write-Error $extractResult.Message
        return $EXIT_EXTRACTION_ERROR
    }
    Write-Success $extractResult.Message

    # Step 7: Rename extracted folder from SedaiAudio-<branch> to SedaiAudioFoundation
    Write-Step "Installing to: $TargetDir"

    # Find the extracted folder (SedaiAudio-main or SedaiAudio-<branch>)
    $extractedFolder = Get-ChildItem -Path $extractTempDir -Directory | Select-Object -First 1
    if (!$extractedFolder) {
        Write-Error "No folder found after extraction"
        return $EXIT_EXTRACTION_ERROR
    }

    # Move to final location with correct name
    Move-Item -Path $extractedFolder.FullName -Destination $TargetDir -Force
    Write-Success "Installed to: $TargetDir"

    # Step 8: Verify installation
    Write-Step "Verifying installation..."

    if (!(Test-Path $Marker)) {
        Write-Error "sedaiaudiofoundation.pas not found after installation"
        return $EXIT_EXTRACTION_ERROR
    }

    Write-Success "SedaiAudioFoundation verified"

    # Step 9: Cleanup
    Write-Step "Cleaning up..."
    Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
    Remove-Item $extractTempDir -Recurse -Force -ErrorAction SilentlyContinue
    Write-Status "Temporary files removed." -Color Gray

    # Done
    Write-Status "`n============================================" -Color Green
    Write-Success "  SedaiAudioFoundation installed successfully!"
    Write-Status "  Location: $TargetDir" -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-SedaiAudio
exit $exitCode
