<#
.SYNOPSIS
    Downloads and installs Python 3.13 portable for Windows

.DESCRIPTION
    This script downloads Python 3.13 portable from the SedaiBasic2-Deps repository
    and extracts it to the benchmarks/runtime/python subfolder of the project root.

    Exit codes:
        0 = Success
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space
        5 = Python already installed (skipped)

.PARAMETER Force
    Overwrite existing Python installation

.PARAMETER SkipVerify
    Skip SHA256 hash verification

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.EXAMPLE
    .\install-python.ps1

.EXAMPLE
    .\install-python.ps1 -Force -Quiet
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
$PYTHON_VERSION = "3.13"
$DOWNLOAD_URL = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/python-3.13.5-win64/python.zip"
$EXPECTED_HASH = "c96accb74322d7876e1d1d26c902f30bc3db233d7d2002de116a44e5b8940665"
$REQUIRED_SPACE_MB = 100  # Approximate space needed

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$PythonDir = Join-Path $ProjectRoot "benchmarks\runtime\python"
$PythonExe = Join-Path $PythonDir "python.exe"
$RuntimeDir = Join-Path $ProjectRoot "benchmarks\runtime"
$TempDir = Join-Path $env:TEMP "python-install"
$ZipFile = Join-Path $TempDir "python-$PYTHON_VERSION-portable.zip"

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
function Install-Python {
    Write-Status "============================================" -Color Cyan
    Write-Status "  Python $PYTHON_VERSION Portable Installer" -Color Cyan
    Write-Status "  Target: x86_64-win64" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    if (Test-Path $PythonExe) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
            Write-Status "Removing existing installation..." -Color Yellow
            Remove-Item -Path $PythonDir -Recurse -Force -ErrorAction SilentlyContinue
        } else {
            Write-Success "Python $PYTHON_VERSION already installed at: $PythonDir"
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
    Write-Step "Downloading Python $PYTHON_VERSION..."
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
    Write-Step "Extracting to: $PythonDir"

    $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $RuntimeDir -Quiet:$Quiet
    if ($extractResult.Status -ne 0) {
        Write-Error $extractResult.Message
        return $EXIT_EXTRACTION_ERROR
    }
    Write-Success $extractResult.Message

    # Step 8: Verify installation
    Write-Step "Verifying installation..."

    if (!(Test-Path $PythonExe)) {
        # Check if files are in a subdirectory
        $possibleExe = Get-ChildItem -Path $RuntimeDir -Recurse -Filter "python.exe" | Select-Object -First 1
        if ($possibleExe) {
            Write-Status "Found python.exe at: $($possibleExe.FullName)" -Color Gray
            $PythonExe = $possibleExe.FullName
        } else {
            Write-Error "python.exe not found after extraction"
            return $EXIT_EXTRACTION_ERROR
        }
    }

    # Test Python works
    try {
        $version = & $PythonExe --version 2>&1
        Write-Success "Python responds: $version"
    } catch {
        Write-Error "Python installed but not responding: $_"
        return $EXIT_EXTRACTION_ERROR
    }

    # Step 9: Cleanup
    Write-Step "Cleaning up..."
    Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
    Write-Status "Temporary files removed." -Color Gray

    # Done
    Write-Status "`n============================================" -Color Green
    Write-Success "  Python $PYTHON_VERSION installed successfully!"
    Write-Status "  Location: $PythonDir" -Color Green
    Write-Status "  Executable: $PythonExe" -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-Python
exit $exitCode
