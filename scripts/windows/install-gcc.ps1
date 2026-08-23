<#
.SYNOPSIS
    Downloads and installs a MinGW-w64 GCC for Windows (the C hot loop)

.DESCRIPTION
    The interpreter's hot dispatch arms are compiled by a C compiler rather than by FPC
    (src/hotdisp.c). Measured on the same dispatch loop, gcc -O2 runs it in 253 ms against
    FPC's 443, and the whole feature is worth 27-45% wherever it applies. This script fetches
    the compiler that builds it.

    ⛔ GCC, not "a C compiler". The flag set is GCC's and is not decoration:
    -fno-crossjumping alone is worth spectral-norm -16.1%, because it stops the compiler
    merging the replicated dispatch tails that give each arm its own branch-predictor
    history. MSVC has no equivalent spelling; clang accepts most of the flags but not that
    one. So this component installs gcc.

    ⭐ ONLY THE COMPILER PROPER IS NEEDED. The build never LINKS with it - it runs
    "gcc -c" and hands the object to FPC's {$L} - so no linker, no CRT and no import
    libraries are required. A trimmed MinGW-w64 (or w64devkit) is enough.

    Exit codes:
        0 = Success
        1 = Network/download error
        2 = Extraction error
        3 = File corrupted (hash mismatch)
        4 = Insufficient disk space
        5 = GCC already installed (skipped)

.PARAMETER Force
    Overwrite existing GCC installation

.PARAMETER SkipVerify
    Skip SHA256 hash verification

.PARAMETER Quiet
    Minimal output (for use from other scripts)

.EXAMPLE
    .\install-gcc.ps1

.EXAMPLE
    .\install-gcc.ps1 -Force -Quiet
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
$GCC_VERSION = "14.2.0"
$DOWNLOAD_URL = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/gcc-mingw64-$GCC_VERSION/gcc-mingw64-$GCC_VERSION-x86_64-win64.zip"

# ⛔ FILL THIS IN WHEN THE RELEASE IS CUT. It is deliberately a recognisable placeholder rather
# than a plausible-looking string: a wrong-but-plausible hash would fail every install with
# "file corrupted" and send whoever hits it looking for a network problem. The check below
# refuses to run against the placeholder instead, and says exactly what to do.
$EXPECTED_HASH = "0000000000000000000000000000000000000000000000000000000000000000"
$REQUIRED_SPACE_MB = 400  # a trimmed MinGW-w64 unpacks to roughly 300 MB

# Determine paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Get-Item "$ScriptDir\..\..").FullName
$DepsDir = Join-Path $ProjectRoot "deps"
$GccDir = Join-Path $DepsDir "gcc"
$GccMarker = Join-Path $GccDir "bin\gcc.exe"   # File to check for existing installation
$TempDir = Join-Path $env:TEMP "gcc-install"
$ZipFile = Join-Path $TempDir "gcc-mingw64-$GCC_VERSION-x86_64-win64.zip"

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
function Install-Gcc {
    Write-Status "============================================" -Color Cyan
    Write-Status "  MinGW-w64 GCC $GCC_VERSION Installer" -Color Cyan
    Write-Status "  Target: deps/gcc" -Color Cyan
    Write-Status "============================================" -Color Cyan

    # Step 1: Check if already installed
    Write-Step "Checking existing installation..."

    if (Test-Path $GccMarker) {
        if ($Force) {
            Write-Status "Existing installation found. -Force specified, will reinstall." -Color Yellow
            Write-Status "Removing existing installation..." -Color Yellow
            Remove-Item -Path $GccDir -Recurse -Force -ErrorAction SilentlyContinue
        } else {
            Write-Success "GCC $GCC_VERSION already installed at: $GccDir"
            Write-Status "Use -Force to reinstall." -Color Yellow
            return $EXIT_ALREADY_INSTALLED
        }
    } else {
        Write-Status "No existing installation found." -Color Gray
    }

    # Step 1b: refuse a placeholder hash rather than fail later as "corrupted"
    if (!$SkipVerify -and $EXPECTED_HASH -eq "0000000000000000000000000000000000000000000000000000000000000000") {
        Write-Error "the expected SHA256 for the GCC package has not been filled in yet."
        Write-Status "  Cut the release, then put its hash in EXPECTED_HASH in this script." -Color Yellow
        Write-Status "  To install without verification in the meantime: -SkipVerify" -Color Yellow
        return $EXIT_HASH_MISMATCH
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
    if (!(Test-Path $DepsDir)) {
        New-Item -ItemType Directory -Path $DepsDir -Force | Out-Null
    }

    # Step 5: Download
    Write-Step "Downloading MinGW-w64 GCC $GCC_VERSION..."
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

    # Step 7: Extract (the zip carries a gcc/ root, so it lands in deps/gcc)
    Write-Step "Extracting to: $GccDir"

    $extractResult = Expand-ArchiveWithProgress -Path $ZipFile -DestinationPath $DepsDir -Quiet:$Quiet
    if ($extractResult.Status -ne 0) {
        Write-Error $extractResult.Message
        return $EXIT_EXTRACTION_ERROR
    }
    Write-Success $extractResult.Message

    # Step 8: Verify installation - and PROVE IT COMPILES, do not just look for the file.
    # A compiler that is present and cannot produce an object is the failure this project has
    # already been bitten by once (see the fpc probe in build.ps1), and it costs one second.
    Write-Step "Verifying installation..."

    if (!(Test-Path $GccMarker)) {
        $found = Get-ChildItem -Path $DepsDir -Recurse -Filter "gcc.exe" -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($found) {
            Write-Status "Found gcc.exe at: $($found.FullName)" -Color Gray
        } else {
            Write-Error "gcc.exe not found after extraction"
            return $EXIT_EXTRACTION_ERROR
        }
    }

    $probeDir = Join-Path $TempDir "probe"
    New-Item -ItemType Directory -Path $probeDir -Force | Out-Null
    $probeSrc = Join-Path $probeDir "probe.c"
    $probeObj = Join-Path $probeDir "probe.o"
    Set-Content -Path $probeSrc -Value 'int probe_ok(void){return 0;}' -Encoding ASCII
    $prev = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    $global:LASTEXITCODE = 0
    & $GccMarker "-c" "-o" $probeObj $probeSrc > (Join-Path $probeDir "probe.log") 2>&1
    $compiled = (($LASTEXITCODE -eq 0) -and (Test-Path $probeObj))
    $ErrorActionPreference = $prev
    if (!$compiled) {
        Write-Error "gcc.exe was installed but cannot compile a trivial C file."
        if (Test-Path (Join-Path $probeDir "probe.log")) {
            Get-Content (Join-Path $probeDir "probe.log") | Select-Object -First 4 | ForEach-Object {
                Write-Status "       $_" -Color Yellow
            }
        }
        Remove-Item -Path $probeDir -Recurse -Force -ErrorAction SilentlyContinue
        return $EXIT_EXTRACTION_ERROR
    }
    Remove-Item -Path $probeDir -Recurse -Force -ErrorAction SilentlyContinue
    Write-Success "GCC verified: it compiles."

    # Step 9: Cleanup
    Write-Step "Cleaning up..."
    Remove-Item $ZipFile -Force -ErrorAction SilentlyContinue
    Write-Status "Temporary files removed." -Color Gray

    # Done
    Write-Status "`n============================================" -Color Green
    Write-Success "  MinGW-w64 GCC $GCC_VERSION installed successfully!"
    Write-Status "  Location: $GccDir" -Color Green
    Write-Status "  The C hot loop will be built with it (27-45% where it applies)." -Color Green
    Write-Status "============================================" -Color Green

    return $EXIT_SUCCESS
}

# Run installation
$exitCode = Install-Gcc
exit $exitCode
