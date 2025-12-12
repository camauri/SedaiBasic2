<#
.SYNOPSIS
    Downloads and installs SedaiBasic2 runtime dependencies for Windows

.DESCRIPTION
    This script downloads the SedaiBasic2 runtime package (SDL2 DLLs, fonts, etc.)
    from the SedaiBasic2-Deps repository and extracts it to the bin/x86_64-win64 folder.

    Exit codes:
        0 = Success
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space
        5 = Runtime already installed (skipped)

.PARAMETER Force
    Overwrite existing runtime installation

.PARAMETER SkipVerify
    Skip SHA256 hash verification

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.EXAMPLE
    .\install-runtime-x86_64.ps1

.EXAMPLE
    .\install-runtime-x86_64.ps1 -Force -Quiet
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
$RUNTIME_VERSION = "x86_64-win64"
$DOWNLOAD_URL = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/Runtime-x86_64-win64/sedai_runtime-x86_64-win64.zip"
$EXPECTED_HASH = "0da2172731dd90ca4eac0c21fc31aa6e89debb4b6e8504214c1a3ce23f025967"
$REQUIRED_SPACE_MB = 30  # Approximate space needed

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$BinDir = Join-Path $ProjectRoot "bin"  # Extract here, zip contains x86_64-win64 folder
$RuntimeDir = Join-Path $BinDir "x86_64-win64"
$RuntimeMarker = Join-Path $RuntimeDir "SDL2.dll"  # File to check for existing installation
$TempDir = Join-Path $env:TEMP "runtime-install"
$ZipFile = Join-Path $TempDir "sedai_runtime-$RUNTIME_VERSION.zip"

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
function Install-Runtime {
    Write-Status "============================================" -Color Cyan
    Write-Status "  SedaiBasic2 Runtime Installer" -Color Cyan
    Write-Status "  Target: bin/x86_64-win64" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    if (Test-Path $RuntimeMarker) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
        } else {
            Write-Success "Runtime already installed at: $RuntimeDir"
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

    # Ensure bin directory exists
    if (!(Test-Path $BinDir)) {
        New-Item -ItemType Directory -Path $BinDir -Force | Out-Null
    }

    # Step 5: Download
    Write-Step "Downloading SedaiBasic2 Runtime..."
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
    Write-Step "Extracting to: $RuntimeDir"

    $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $BinDir -Quiet:$Quiet
    if ($extractResult.Status -ne 0) {
        Write-Error $extractResult.Message
        return $EXIT_EXTRACTION_ERROR
    }
    Write-Success $extractResult.Message

    # Step 8: Verify installation - check all required files
    Write-Step "Verifying installation..."

    $requiredFiles = @(
        "SDL2.dll",
        "SDL2_ttf.dll",
        "zlib1.dll",
        "fonts\PixelOperatorMono8-Bold.ttf"
    )

    $missingFiles = @()
    foreach ($file in $requiredFiles) {
        $filePath = Join-Path $RuntimeDir $file
        if (!(Test-Path $filePath)) {
            $missingFiles += $file
        }
    }

    if ($missingFiles.Count -gt 0) {
        Write-Error "Missing files after extraction:"
        foreach ($file in $missingFiles) {
            Write-Status "  - $file" -Color Red
        }
        return $EXIT_EXTRACTION_ERROR
    }

    Write-Success "All runtime files verified ($($requiredFiles.Count) files)"

    # Step 9: Cleanup
    Write-Step "Cleaning up..."
    Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
    Write-Status "Temporary files removed." -Color Gray

    # Done
    Write-Status "`n============================================" -Color Green
    Write-Success "  SedaiBasic2 Runtime installed successfully!"
    Write-Status "  Location: $RuntimeDir" -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-Runtime
exit $exitCode
