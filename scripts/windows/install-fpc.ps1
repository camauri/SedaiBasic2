<#
.SYNOPSIS
    Downloads and installs Free Pascal 3.2.2 x86_64 for Windows

.DESCRIPTION
    This script downloads FPC 3.2.2 from the SedaiBasic2-Deps repository
    and extracts it to the fpc/ subfolder of the project root.

    Exit codes:
        0 = Success (install or fpc.cfg regeneration)
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space

.PARAMETER Force
    Overwrite existing FPC installation

.PARAMETER SkipVerify
    Skip SHA256 hash verification

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.EXAMPLE
    .\install-fpc.ps1

.EXAMPLE
    .\install-fpc.ps1 -Force -Quiet
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

# Configuration
$FPC_VERSION = "3.2.2"
$FPC_ARCH = "x86_64-win64"
$DOWNLOAD_URL = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/fpc-3.2.2-x86_64-win64/fpc-3.2.2-x86_64-win64.zip"
$EXPECTED_HASH = "bb4fb67ac9f533bae15479307bac6eaaaae3b1799f7f8064bef04b430b8ae99a"
$REQUIRED_SPACE_MB = 500  # Approximate space needed

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$FpcDir = Join-Path $ProjectRoot "fpc\$FPC_VERSION"
$FpcExe = Join-Path $FpcDir "bin\x86_64-win64\fpc.exe"
$FpcRootDir = Join-Path $ProjectRoot "fpc"
$TempDir = Join-Path $env:TEMP "fpc-install"
$ZipFile = Join-Path $TempDir "fpc-$FPC_VERSION-$FPC_ARCH.zip"

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
function Install-FPC {
    Write-Status "============================================" -Color Cyan
    Write-Status "  Free Pascal $FPC_VERSION Installer" -Color Cyan
    Write-Status "  Target: $FPC_ARCH" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    $skipDownload = $false
    if (Test-Path $FpcExe) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
            Write-Status "Removing existing installation..." -Color Yellow
            Remove-Item -Path $FpcRootDir -Recurse -Force -ErrorAction SilentlyContinue
        } else {
            Write-Success "FPC $FPC_VERSION already installed at: $FpcDir"
            Write-Status "Regenerating fpc.cfg..." -Color Yellow
            $skipDownload = $true
        }
    } else {
        Write-Status "No existing installation found." -Color Gray
    }

    if (-not $skipDownload) {
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

        # Step 5: Download
        Write-Step "Downloading FPC $FPC_VERSION..."
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

        # Step 7: Extract (zip contains fpc/3.2.2/... so extract to project root)
        Write-Step "Extracting to: $FpcRootDir"

        $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $ProjectRoot -Quiet:$Quiet
        if ($extractResult.Status -ne 0) {
            Write-Error $extractResult.Message
            return $EXIT_EXTRACTION_ERROR
        }
        Write-Success $extractResult.Message

        # Step 8: Verify installation
        Write-Step "Verifying installation..."

        if (!(Test-Path $FpcExe)) {
            # Check if files are in a subdirectory
            $possibleExe = Get-ChildItem -Path $FpcDir -Recurse -Filter "fpc.exe" | Select-Object -First 1
            if ($possibleExe) {
                Write-Status "Found fpc.exe at: $($possibleExe.FullName)" -Color Gray
                # Files might be nested, that's ok
                $FpcExe = $possibleExe.FullName
            } else {
                Write-Error "fpc.exe not found after extraction"
                return $EXIT_EXTRACTION_ERROR
            }
        }

        # Test FPC works
        try {
            $version = & $FpcExe -iV 2>&1
            Write-Success "FPC responds: $version"
        } catch {
            Write-Error "FPC installed but not responding: $_"
            return $EXIT_EXTRACTION_ERROR
        }
    }

    # Step 9: Generate fpc.cfg
    Write-Step "Generating fpc.cfg..."

    $fpcBinDir = Split-Path -Parent $FpcExe
    $fpcmkcfg = Join-Path $fpcBinDir "fpcmkcfg.exe"
    $fpcCfg = Join-Path $fpcBinDir "fpc.cfg"

    if (Test-Path $fpcmkcfg) {
        $cfgProcess = Start-Process -FilePath $fpcmkcfg -ArgumentList @('-d', "basepath=$FpcDir", '-o', $fpcCfg) -NoNewWindow -Wait -PassThru
        if ($cfgProcess.ExitCode -eq 0) {
            Write-Success "fpc.cfg generated successfully"
        } else {
            Write-Status "Warning: fpcmkcfg returned exit code $($cfgProcess.ExitCode)" -Color Yellow
        }
    } else {
        Write-Status "Warning: fpcmkcfg not found, skipping fpc.cfg generation" -Color Yellow
    }

    # Step 10: Cleanup (only if we downloaded)
    if (-not $skipDownload) {
        Write-Step "Cleaning up..."
        Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
        Write-Status "Temporary files removed." -Color Gray
    }

    # Done
    Write-Status "`n============================================" -Color Green
    if ($skipDownload) {
        Write-Success "  fpc.cfg regenerated successfully!"
    } else {
        Write-Success "  FPC $FPC_VERSION installed successfully!"
    }
    Write-Status "  Location: $FpcDir" -Color Green
    Write-Status "  Compiler: $FpcExe" -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-FPC
exit $exitCode
