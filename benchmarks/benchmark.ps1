<#
.SYNOPSIS
    SedaiBasic2 Benchmark Script

.DESCRIPTION
    Runs standardized benchmarks from The Computer Language Benchmarks Game:
    - fannkuch-redux
    - n-body
    - spectral-norm

    Results are saved to BENCHMARKS.md
    Run times are accumulated in a history file for cumulative statistics.

    Copyright (c) 2025 Maurizio Cammalleri
    Released under GNU GPL v3

.PARAMETER Force
    Force re-run of all benchmarks, ignoring previous results

.PARAMETER Quick
    Quick test mode: use N values from source files instead of standard benchmark values

.PARAMETER Runs
    Number of runs per benchmark (default: 1).
    Clears history and runs fresh tests. Ignored if -Update or -Runtime is used
    (in that case, runs are read from history).

.PARAMETER ClearHistory
    Clear all accumulated benchmark history and start fresh

.PARAMETER Report
    Generate markdown report from existing history without running benchmarks.
    Exits with error if no history data exists.

.PARAMETER Output
    Custom output filename for the results (default: BENCHMARKS.md)

.PARAMETER Update
    Re-run specific benchmark(s) and replace their history data.
    Accepts: fannkuch-redux, n-body, spectral-norm
    Number of runs is read from history (or uses -Runs if history is empty).

.PARAMETER Runtime
    Run only specific runtime(s): sedai, python, lua
    Number of runs is read from history (or uses -Runs if history is empty).

.EXAMPLE
    .\benchmark.ps1

.EXAMPLE
    .\benchmark.ps1 -Quick

.EXAMPLE
    .\benchmark.ps1 -Runs 3

.EXAMPLE
    .\benchmark.ps1 -ClearHistory

.EXAMPLE
    .\benchmark.ps1 -Report

.EXAMPLE
    .\benchmark.ps1 -Output "results.md"

.EXAMPLE
    .\benchmark.ps1 -Update fannkuch-redux

.EXAMPLE
    .\benchmark.ps1 -Update fannkuch-redux,spectral-norm

.EXAMPLE
    .\benchmark.ps1 -Runtime sedai

.EXAMPLE
    .\benchmark.ps1 -Update fannkuch-redux -Runtime sedai,lua
#>

param(
    [switch]$Help,
    [switch]$Force,
    [switch]$Quick,
    [switch]$ClearHistory,
    [switch]$Report,
    [switch]$ResetConfig,
    [string]$Output = "",
    [int]$Runs = 1,
    [string[]]$Update = @(),
    [string[]]$Runtime = @()
)

# ============================================================================
#  CONFIGURATION
# ============================================================================

$Script:BenchmarkDir = $PSScriptRoot
$Script:ProjectRoot = Split-Path -Parent $BenchmarkDir
$Script:TempDir = Join-Path $BenchmarkDir ".temp"
$Script:HistoryDir = Join-Path $BenchmarkDir ".history"
$Script:ResultsDir = Join-Path $BenchmarkDir "results"
$Script:StatusFile = Join-Path $TempDir "status.json"
$Script:HistoryFile = Join-Path $HistoryDir "history.json"    # Permanent - cumulative data
$Script:PendingFile = Join-Path $HistoryDir "pending.json"    # Permanent - incomplete session
$Script:ConfigFile = Join-Path $BenchmarkDir "benchmark.config.json"  # User runtime configuration
# Results file will be set dynamically based on timestamp
$Script:SbExe = Join-Path $ProjectRoot "bin\x86_64-win64\sb.exe"

# Default paths for Sedai-provided runtimes
$Script:DefaultPythonExe = Join-Path $ProjectRoot "benchmarks\runtime\python\python.exe"
$Script:DefaultLuaExe = Join-Path $ProjectRoot "benchmarks\runtime\lua\lua54.exe"

# Active paths (will be set from config or defaults)
$Script:PythonExe = $DefaultPythonExe
$Script:PyTestsDir = Join-Path $BenchmarkDir "tests\py"
$Script:LuaExe = $DefaultLuaExe
$Script:LuaTestsDir = Join-Path $BenchmarkDir "tests\lua"

# User configuration structure
# PythonPath/LuaPath values:
#   $null = use Sedai package (download if needed)
#   "disabled" = skip this runtime
#   <path> = use custom path
$Script:UserConfig = @{
    PythonPath = $null
    LuaPath = $null
}
$Script:ConfigurationComplete = $false  # Set to true after user completes setup

# Benchmark configurations
# StandardN = values from The Computer Language Benchmarks Game
# NPattern = regex to extract N from source file (for Quick mode)
$Script:Benchmarks = @(
    @{
        Name = "fannkuch-redux"
        Source = "bas\07_benchmarks\fannkuch-redux.bas"
        NPattern = "N%\s*=\s*(\d+)"
        NReplace = "N% = {0}"
        StandardN = 12
        Description = "Indexed-access to tiny integer-sequence"
    },
    @{
        Name = "n-body"
        Source = "bas\07_benchmarks\n-body.bas"
        NPattern = "N%\s*=\s*(\d+)"
        NReplace = "N%={0}"
        StandardN = 50000000
        Description = "Double-precision N-body simulation"
    },
    @{
        Name = "spectral-norm"
        Source = "bas\07_benchmarks\spectral-norm.bas"
        NPattern = "N%\s*=\s*(\d+)"
        NReplace = "N% = {0}"
        StandardN = 5500
        Description = "Eigenvalue using the power method"
    }
)

# Python benchmark configurations
# PyScript = single-threaded version (for fair comparison)
# PyScriptMulti = multi-threaded version (for reference, null if not available)
$Script:PythonBenchmarks = @(
    @{
        Name = "fannkuch-redux"
        PyScript = "fannkuch-redux_single.py"
        PyScriptMulti = "fannkuch-redux.py"
        StandardN = 12
        Description = "Indexed-access to tiny integer-sequence"
    },
    @{
        Name = "n-body"
        PyScript = "n-body_single.py"
        PyScriptMulti = $null  # n-body.py is also single-thread
        StandardN = 50000000
        Description = "Double-precision N-body simulation"
    },
    @{
        Name = "spectral-norm"
        PyScript = "spectral-norm_single.py"
        PyScriptMulti = "spectral-norm.py"
        StandardN = 5500
        Description = "Eigenvalue using the power method"
    }
)

# Lua benchmark configurations
# LuaScript = Lua script filename (all single-threaded)
$Script:LuaBenchmarks = @(
    @{
        Name = "fannkuch-redux"
        LuaScript = "fannkuch-redux.lua"
        StandardN = 12
        Description = "Indexed-access to tiny integer-sequence"
    },
    @{
        Name = "n-body"
        LuaScript = "n-body.lua"
        StandardN = 50000000
        Description = "Double-precision N-body simulation"
    },
    @{
        Name = "spectral-norm"
        LuaScript = "spectral-norm.lua"
        StandardN = 5500
        Description = "Eigenvalue using the power method"
    }
)

# Save original benchmark lists for report generation (before filtering)
$Script:AllBenchmarks = $Script:Benchmarks
$Script:AllPythonBenchmarks = $Script:PythonBenchmarks
$Script:AllLuaBenchmarks = $Script:LuaBenchmarks

# Filter benchmarks if -Update parameter is specified
if ($Update.Count -gt 0) {
    $validNames = $Script:Benchmarks | ForEach-Object { $_.Name }
    $invalidNames = $Update | Where-Object { $_ -notin $validNames }
    if ($invalidNames.Count -gt 0) {
        $invalidList = $invalidNames -join ', '
        Write-Host "ERROR: Unknown benchmark: $invalidList" -ForegroundColor Red
        Write-Host "Valid names: $($validNames -join ', ')" -ForegroundColor Yellow
        exit 1
    }
    $Script:Benchmarks = $Script:Benchmarks | Where-Object { $_.Name -in $Update }
    $Script:PythonBenchmarks = $Script:PythonBenchmarks | Where-Object { $_.Name -in $Update }
    $Script:LuaBenchmarks = $Script:LuaBenchmarks | Where-Object { $_.Name -in $Update }
}

# ============================================================================
#  CONFIGURATION MANAGEMENT
# ============================================================================

function Load-Config {
    if (Test-Path $ConfigFile) {
        try {
            $config = Get-Content $ConfigFile -Raw | ConvertFrom-Json
            # Load Python path (can be null, "disabled", or a path)
            if ($null -ne $config.PythonPath) {
                $Script:UserConfig.PythonPath = $config.PythonPath
            }
            # Load Lua path (can be null, "disabled", or a path)
            if ($null -ne $config.LuaPath) {
                $Script:UserConfig.LuaPath = $config.LuaPath
            }
            $Script:ConfigurationComplete = $true
            return $true
        } catch {
            Write-Host "  [!] Error reading config file: $($_.Exception.Message)" -ForegroundColor Red
            return $false
        }
    }
    return $false
}

function Save-Config {
    try {
        $config = @{
            PythonPath = $UserConfig.PythonPath
            LuaPath = $UserConfig.LuaPath
        }
        $config | ConvertTo-Json -Depth 2 | Set-Content $ConfigFile -Encoding UTF8
        return $true
    } catch {
        Write-Host "  [!] Error saving config file: $($_.Exception.Message)" -ForegroundColor Red
        return $false
    }
}

function Reset-Config {
    $Script:UserConfig.PythonPath = $null
    $Script:UserConfig.LuaPath = $null
    if (Test-Path $ConfigFile) {
        Remove-Item $ConfigFile -Force
    }
}

function Prompt-ForRuntimePath {
    param(
        [string]$RuntimeName,
        [string]$Description,
        [string]$ExecutableName,
        [string]$DefaultDownloadPath,
        [string]$CurrentValue
    )

    Write-Host ""
    Write-Host "  $RuntimeName" -ForegroundColor Cyan
    Write-Host "  $Description" -ForegroundColor Gray
    Write-Host ""

    if ($CurrentValue -and $CurrentValue -ne "disabled") {
        Write-Host "  Current configuration: " -NoNewline -ForegroundColor Gray
        Write-Host $CurrentValue -ForegroundColor Yellow
        Write-Host ""
    } elseif ($CurrentValue -eq "disabled") {
        Write-Host "  Current configuration: " -NoNewline -ForegroundColor Gray
        Write-Host "(disabled)" -ForegroundColor DarkGray
        Write-Host ""
    }

    Write-Host "  Options:" -ForegroundColor White
    Write-Host '    [1] Download official Sedai package (Recommended)' -ForegroundColor Green
    Write-Host '    [2] Use existing installation (specify path)' -ForegroundColor White
    Write-Host '    [3] Do not use (skip this runtime)' -ForegroundColor DarkGray
    if ($CurrentValue) {
        Write-Host "    [4] Keep current configuration" -ForegroundColor DarkGray
    }
    Write-Host ""

    do {
        $maxOption = if ($CurrentValue) { 4 } else { 3 }
        Write-Host "  Choice [1-$maxOption]: " -NoNewline -ForegroundColor Yellow
        $choice = Read-Host

        switch ($choice) {
            "1" {
                return @{ UseDefault = $true; Path = $null }
            }
            "2" {
                Write-Host ""
                Write-Host "  Enter full path to $ExecutableName (or folder containing it): " -NoNewline -ForegroundColor Yellow
                $path = Read-Host

                if ([string]::IsNullOrWhiteSpace($path)) {
                    Write-Host "  [!] Invalid path" -ForegroundColor Red
                    continue
                }

                # Remove quotes if user included them
                $path = $path.Trim('"', "'", ' ')

                # Check if path is directly to executable or to folder
                $testExe = $path
                if (Test-Path $path -PathType Container) {
                    $testExe = Join-Path $path $ExecutableName
                }

                if (Test-Path $testExe) {
                    Write-Host "  [OK] Valid path: $testExe" -ForegroundColor Green
                    # Return the executable path, not the folder
                    return @{ UseDefault = $false; Path = $testExe }
                } else {
                    Write-Host "  [!] Executable not found: $testExe" -ForegroundColor Red
                    Write-Host "  Use this path anyway? [y/N]: " -NoNewline -ForegroundColor Yellow
                    $confirm = Read-Host
                    if ($confirm -ieq 'y') {
                        return @{ UseDefault = $false; Path = $path }
                    }
                    continue
                }
            }
            "3" {
                return @{ UseDefault = $false; Path = "disabled" }
            }
            "4" {
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

function Run-RuntimeSetup {
    Write-Host ""
    Write-Host "  ================================================================" -ForegroundColor Cyan
    Write-Host "           COMPARISON RUNTIME CONFIGURATION                      " -ForegroundColor Cyan
    Write-Host "  ================================================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  Configure Python and Lua interpreters for benchmark comparisons." -ForegroundColor White
    Write-Host "  You can use the official Sedai packages or your own installations." -ForegroundColor Gray
    Write-Host ""

    # 1. Python Configuration
    $pythonResult = Prompt-ForRuntimePath `
        -RuntimeName "1. PYTHON" `
        -Description "Python interpreter for comparison benchmarks" `
        -ExecutableName "python.exe" `
        -DefaultDownloadPath $DefaultPythonExe `
        -CurrentValue $UserConfig.PythonPath

    if ($pythonResult.UseDefault) {
        $Script:UserConfig.PythonPath = $null
    } elseif ($pythonResult.Path) {
        $Script:UserConfig.PythonPath = $pythonResult.Path
    }

    # 2. Lua Configuration
    $luaResult = Prompt-ForRuntimePath `
        -RuntimeName "2. LUA" `
        -Description "Lua interpreter for comparison benchmarks" `
        -ExecutableName "lua54.exe" `
        -DefaultDownloadPath $DefaultLuaExe `
        -CurrentValue $UserConfig.LuaPath

    if ($luaResult.UseDefault) {
        $Script:UserConfig.LuaPath = $null
    } elseif ($luaResult.Path) {
        $Script:UserConfig.LuaPath = $luaResult.Path
    }

    # Save configuration
    Write-Host ""
    Write-Host "  Saving configuration..." -ForegroundColor Gray
    if (Save-Config) {
        Write-Host "  [OK] Configuration saved to: benchmark.config.json" -ForegroundColor Green
        $Script:ConfigurationComplete = $true
    }
    Write-Host ""
}

function Apply-RuntimeConfig {
    # Apply Python configuration
    if ($UserConfig.PythonPath -eq "disabled") {
        $Script:RunPython = $false
    } elseif ($UserConfig.PythonPath) {
        $Script:PythonExe = $UserConfig.PythonPath
    } else {
        $Script:PythonExe = $DefaultPythonExe
    }

    # Apply Lua configuration
    if ($UserConfig.LuaPath -eq "disabled") {
        $Script:RunLua = $false
    } elseif ($UserConfig.LuaPath) {
        $Script:LuaExe = $UserConfig.LuaPath
    } else {
        $Script:LuaExe = $DefaultLuaExe
    }
}

# Runtime flags (default: all enabled)
$Script:RunSedai = $true
$Script:RunPython = $true
$Script:RunLua = $true

# Filter runtimes if -Runtime parameter is specified
if ($Runtime.Count -gt 0) {
    $validRuntimes = @("sedai", "python", "lua")
    $normalizedRuntimes = $Runtime | ForEach-Object { $_.ToLower() }
    $invalidRuntimes = $normalizedRuntimes | Where-Object { $_ -notin $validRuntimes }
    if ($invalidRuntimes.Count -gt 0) {
        $invalidList = $invalidRuntimes -join ', '
        $validList = $validRuntimes -join ', '
        Write-Host "ERROR: Unknown runtime: $invalidList" -ForegroundColor Red
        Write-Host "Valid runtimes: $validList" -ForegroundColor Yellow
        exit 1
    }
    $Script:RunSedai = "sedai" -in $normalizedRuntimes
    $Script:RunPython = "python" -in $normalizedRuntimes
    $Script:RunLua = "lua" -in $normalizedRuntimes
}

# Function to read N value from source file
function Get-SourceN {
    param([hashtable]$Benchmark)

    $sourcePath = Join-Path $ProjectRoot $Benchmark.Source
    if (!(Test-Path $sourcePath)) {
        return 0
    }

    $content = Get-Content $sourcePath -Raw
    if ($content -match $Benchmark.NPattern) {
        return [long]$Matches[1]
    }
    return 0
}

# Function to get N value based on mode
function Get-BenchmarkN {
    param(
        [hashtable]$Benchmark,
        [bool]$QuickMode
    )

    if ($QuickMode) {
        return Get-SourceN $Benchmark
    } else {
        return $Benchmark.StandardN
    }
}

# ============================================================================
#  DISPLAY FUNCTIONS
# ============================================================================

function Show-Help {
    Write-Host ""
    Write-Host "SedaiBasic2 Benchmark Script" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "USAGE:" -ForegroundColor Yellow
    Write-Host "    .\benchmark.ps1 [options]"
    Write-Host ""
    Write-Host "OPTIONS:" -ForegroundColor Yellow
    Write-Host "    -Help           Show this help message"
    Write-Host "    -Force          Force re-run of all benchmarks, ignoring previous results"
    Write-Host '    -Quick          Use N values from source files (fast test mode)'
    Write-Host '    -Runs <n>       Number of runs (clears history). Ignored with -Update/-Runtime'
    Write-Host "    -ClearHistory   Clear accumulated benchmark history and start fresh"
    Write-Host '    -Report         Generate report from existing history (no benchmark run)'
    Write-Host '    -Output <file>  Custom output filename (default: auto-generated)'
    Write-Host '    -Update <names> Re-run specific benchmark(s), replacing history data'
    Write-Host "                    Valid: fannkuch-redux, n-body, spectral-norm"
    Write-Host '    -Runtime <rt>   Run only specific runtime(s): sedai, python, lua'
    Write-Host ""
    Write-Host "RUN COUNT BEHAVIOR:" -ForegroundColor Yellow
    Write-Host "    - With -Runs only: clears history and runs N fresh tests for all benchmarks"
    Write-Host '    - With -Update or -Runtime: reads run count from history (ignores -Runs)'
    Write-Host '    - If history is empty with -Update/-Runtime: uses -Runs (default: 1)'
    Write-Host "    - Misaligned run counts between runtimes: uses max and shows warning"
    Write-Host ""
    Write-Host "SESSION RESUME:" -ForegroundColor Yellow
    Write-Host "    If a session was interrupted, the next run will automatically resume"
    Write-Host "    from where it left off, running only the incomplete benchmarks."
    Write-Host ""
    Write-Host "BENCHMARKS:" -ForegroundColor Yellow
    Write-Host '    fannkuch-redux  Indexed-access to tiny integer-sequence (N=12)'
    Write-Host '    n-body          Double-precision N-body simulation (N=50000000)'
    Write-Host '    spectral-norm   Eigenvalue using the power method (N=5500)'
    Write-Host ""
    Write-Host "    Standard N values from The Computer Language Benchmarks Game."
    Write-Host "    Use -Quick to run with N values from source files instead."
    Write-Host ""
    Write-Host "EXAMPLES:" -ForegroundColor Yellow
    Write-Host '    .\benchmark.ps1                 # Run all benchmarks (uses history run count)'
    Write-Host "    .\benchmark.ps1 -Runs 5         # Clear history, run 5 times for all"
    Write-Host "    .\benchmark.ps1 -Quick          # Run with N from source files"
    Write-Host "    .\benchmark.ps1 -Force          # Re-run all, ignore cached session"
    Write-Host "    .\benchmark.ps1 -ClearHistory   # Reset all accumulated data"
    Write-Host "    .\benchmark.ps1 -Report         # Generate report from history only"
    Write-Host "    .\benchmark.ps1 -Output out.md  # Use custom output filename"
    Write-Host "    .\benchmark.ps1 -Update fannkuch-redux            # Re-run single benchmark"
    Write-Host "    .\benchmark.ps1 -Update fannkuch-redux,n-body     # Re-run multiple"
    Write-Host "    .\benchmark.ps1 -Runtime sedai                    # Re-run all on SedaiBasic2"
    Write-Host "    .\benchmark.ps1 -Update fannkuch-redux -Runtime sedai  # Combined"
    Write-Host ""
    Write-Host "OUTPUT:" -ForegroundColor Yellow
    Write-Host "    Results are saved to BENCHMARK_YYYY-MM-DD-SSSSS.md and displayed on screen."
    Write-Host "    SSSSS = seconds since midnight (0-86399)."
    Write-Host "    Use -Output to specify a custom filename."
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
    Write-Host "                   BENCHMARK SUITE" -ForegroundColor Yellow
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
}

function Show-Warning {
    $width = 70
    $border = "-" * $width

    Write-Host ""
    Write-Host $border -ForegroundColor Yellow
    Write-Host ""
    Write-Host "  WARNING: These benchmarks require significant CPU time!" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "  Before proceeding, please:" -ForegroundColor White
    Write-Host "    - Close all web browsers" -ForegroundColor Gray
    Write-Host "    - Close all unnecessary applications" -ForegroundColor Gray
    Write-Host "    - Disable background processes if possible" -ForegroundColor Gray
    Write-Host '    - Ensure your computer is plugged in (not on battery)' -ForegroundColor Gray
    Write-Host ""
    Write-Host '  Estimated total time: 10-30 minutes (depending on CPU)' -ForegroundColor White
    Write-Host ""
    Write-Host $border -ForegroundColor Yellow
    Write-Host ""
}

function Show-Status {
    param(
        [string]$Message,
        [string]$Type = "Info"
    )

    $prefix = "  "
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
        "Run" {
            Write-Host "$prefix[RUN] " -ForegroundColor Cyan -NoNewline
            Write-Host $Message
        }
        default {
            Write-Host "$prefix" -NoNewline
            Write-Host $Message -ForegroundColor Gray
        }
    }
}

# ============================================================================
#  FILENAME GENERATION
# ============================================================================

function Get-TimestampedResultsFile {
    # Generate filename: BENCHMARK_YYYY-MM-DD-SSSSS.md
    # where SSSSS is seconds since midnight (0-86399)
    $now = Get-Date
    $year = $now.ToString("yyyy")
    $month = $now.ToString("MM")
    $day = $now.ToString("dd")
    $secondsOfDay = [int]($now.Hour * 3600 + $now.Minute * 60 + $now.Second)
    $secondsStr = $secondsOfDay.ToString("00000")

    $filename = "BENCHMARK_$year-$month-$day-$secondsStr.md"
    return Join-Path $ResultsDir $filename
}

# ============================================================================
#  SYSTEM INFORMATION
# ============================================================================

function Get-SystemInfo {
    $info = @{}

    # OS
    $os = Get-CimInstance Win32_OperatingSystem
    $info.OS = $os.Caption
    $info.OSVersion = $os.Version
    $info.Architecture = $os.OSArchitecture

    # CPU
    $cpu = Get-CimInstance Win32_Processor
    $info.CPU = $cpu.Name.Trim()
    $info.CPUCores = $cpu.NumberOfCores
    $info.CPUThreads = $cpu.NumberOfLogicalProcessors
    $info.CPUMaxClock = "{0:F2} GHz" -f ($cpu.MaxClockSpeed / 1000)

    # RAM
    $ram = Get-CimInstance Win32_ComputerSystem
    $info.RAM = "{0:F1} GB" -f ($ram.TotalPhysicalMemory / 1GB)

    # Computer
    $info.ComputerName = $env:COMPUTERNAME

    return $info
}

# ============================================================================
#  STATUS MANAGEMENT
# ============================================================================

function Get-BenchmarkStatus {
    if (!(Test-Path $StatusFile)) {
        return @{}
    }
    try {
        $content = Get-Content $StatusFile -Raw
        $json = $content | ConvertFrom-Json
        # Convert PSCustomObject to hashtable for compatibility with PowerShell 5.1
        $result = @{}
        foreach ($prop in $json.PSObject.Properties) {
            $result[$prop.Name] = $prop.Value
        }
        return $result
    } catch {
        return @{}
    }
}

function Save-BenchmarkStatus {
    param([hashtable]$Status)
    $Status | ConvertTo-Json -Depth 10 | Set-Content $StatusFile -Encoding UTF8
}

# ============================================================================
#  HISTORY MANAGEMENT (cumulative results)
# ============================================================================

function Get-BenchmarkHistory {
    if (!(Test-Path $HistoryFile)) {
        return @{}
    }
    try {
        $content = Get-Content $HistoryFile -Raw
        $json = $content | ConvertFrom-Json
        $result = @{}
        foreach ($prop in $json.PSObject.Properties) {
            # Convert array of PSCustomObjects to array of hashtables
            $runs = @()
            foreach ($run in $prop.Value) {
                # Determine record type by key name prefix
                if ($prop.Name -like "Python_*" -or $prop.Name -like "Lua_*") {
                    # Python and Lua records have Time only (no Instructions)
                    $runs += @{
                        Time = $run.Time
                        N = $run.N
                        Timestamp = $run.Timestamp
                    }
                } else {
                    # SedaiBasic2 record (has Time and Instructions)
                    $runs += @{
                        Time = $run.Time
                        Instructions = $run.Instructions
                        N = $run.N
                        Timestamp = $run.Timestamp
                    }
                }
            }
            $result[$prop.Name] = $runs
        }
        return $result
    } catch {
        return @{}
    }
}

function Save-BenchmarkHistory {
    param([hashtable]$History)
    $History | ConvertTo-Json -Depth 10 | Set-Content $HistoryFile -Encoding UTF8
}

function Get-HistoryRunCounts {
    # Returns a hashtable with run counts per benchmark and runtime
    # Also returns max runs per benchmark (for realignment)
    # Keys: "benchmarkName" -> @{ Sedai=N; Python1t=N; PythonMt=N; Lua=N; Max=N }

    $history = Get-BenchmarkHistory
    $result = @{}

    # All benchmark names
    $benchNames = @("fannkuch-redux", "n-body", "spectral-norm")

    foreach ($benchName in $benchNames) {
        $counts = @{
            Sedai = 0
            Python1t = 0
            PythonMt = 0
            Lua = 0
            Max = 0
        }

        # SedaiBasic2
        if ($history.ContainsKey($benchName)) {
            $counts.Sedai = $history[$benchName].Count
        }

        # Python single-thread
        $py1tKey = "Python_$benchName-1t"
        if ($history.ContainsKey($py1tKey)) {
            $counts.Python1t = $history[$py1tKey].Count
        }

        # Python multi-thread
        $pyMtKey = "Python_$benchName-mt"
        if ($history.ContainsKey($pyMtKey)) {
            $counts.PythonMt = $history[$pyMtKey].Count
        }

        # Lua
        $luaKey = "Lua_$benchName"
        if ($history.ContainsKey($luaKey)) {
            $counts.Lua = $history[$luaKey].Count
        }

        # Calculate max
        $counts.Max = [Math]::Max($counts.Sedai, [Math]::Max($counts.Python1t, [Math]::Max($counts.PythonMt, $counts.Lua)))

        $result[$benchName] = $counts
    }

    return $result
}

function Get-GlobalMaxRuns {
    # Returns the global max runs across all benchmarks
    $counts = Get-HistoryRunCounts
    $globalMax = 0
    foreach ($benchName in $counts.Keys) {
        if ($counts[$benchName].Max -gt $globalMax) {
            $globalMax = $counts[$benchName].Max
        }
    }
    return $globalMax
}

function Show-RunCountMisalignment {
    param([hashtable]$Counts)

    $hasWarning = $false
    foreach ($benchName in $Counts.Keys) {
        $c = $Counts[$benchName]
        $values = @($c.Sedai, $c.Python1t, $c.PythonMt, $c.Lua) | Where-Object { $_ -gt 0 }
        if ($values.Count -gt 1) {
            $min = ($values | Measure-Object -Minimum).Minimum
            $max = ($values | Measure-Object -Maximum).Maximum
            if ($min -ne $max) {
                if (-not $hasWarning) {
                    Write-Host ""
                    Write-Host "  WARNING: Run count misalignment detected!" -ForegroundColor Yellow
                    $hasWarning = $true
                }
                $maxVal = $c.Max
                Write-Host "    $benchName : Sedai=$($c.Sedai), Python1t=$($c.Python1t), PythonMt=$($c.PythonMt), Lua=$($c.Lua) - using max=$maxVal" -ForegroundColor Yellow
            }
        }
    }
    if ($hasWarning) {
        Write-Host ""
    }
    return $hasWarning
}

function Add-RunToHistory {
    param(
        [string]$Name,
        [int]$N,
        [double]$Time,
        [long]$Instructions
    )

    $history = Get-BenchmarkHistory
    if (-not $history.ContainsKey($Name)) {
        $history[$Name] = @()
    }

    # Add new run
    $history[$Name] += @{
        Time = $Time
        Instructions = $Instructions
        N = $N
        Timestamp = (Get-Date).ToString("o")
    }

    Save-BenchmarkHistory $history
    return $history[$Name].Count
}

function Get-HistoryTimes {
    param(
        [string]$Name,
        [int]$N
    )

    $history = Get-BenchmarkHistory
    if (-not $history.ContainsKey($Name)) {
        return @()
    }

    # Filter by N value and extract times
    $times = @()
    foreach ($run in $history[$Name]) {
        if ($run.N -eq $N) {
            $times += $run.Time
        }
    }
    return $times
}

function Clear-BenchmarkHistory {
    param([string]$Name = $null)

    if ($Name) {
        $history = Get-BenchmarkHistory
        if ($history.ContainsKey($Name)) {
            $history.Remove($Name)
            Save-BenchmarkHistory $history
        }
    } else {
        if (Test-Path $HistoryFile) {
            Remove-Item $HistoryFile -Force
        }
    }
}

# ============================================================================
#  PENDING SESSION MANAGEMENT
#  Runs are stored in pending until ALL 3 benchmarks complete successfully
# ============================================================================

function Get-PendingSession {
    if (!(Test-Path $PendingFile)) {
        return $null
    }
    try {
        $content = Get-Content $PendingFile -Raw
        $json = $content | ConvertFrom-Json
        $result = @{
            QuickMode = $json.QuickMode
            Runs = $json.Runs
            Completed = @{}
            PendingRuns = @{}
            PendingPythonRuns = @{}
            PendingLuaRuns = @{}
        }
        # Convert Completed benchmarks
        if ($json.Completed) {
            foreach ($prop in $json.Completed.PSObject.Properties) {
                $result.Completed[$prop.Name] = $prop.Value
            }
        }
        # Convert PendingRuns (runs waiting to be committed to history)
        if ($json.PendingRuns) {
            foreach ($prop in $json.PendingRuns.PSObject.Properties) {
                $runs = @()
                foreach ($run in $prop.Value) {
                    $runs += @{
                        Time = $run.Time
                        Instructions = $run.Instructions
                        N = $run.N
                        Timestamp = $run.Timestamp
                    }
                }
                $result.PendingRuns[$prop.Name] = $runs
            }
        }
        # Convert PendingPythonRuns (Python runs waiting to be committed to history)
        # Keys are already in format "benchmark-1t" or "benchmark-mt" with Time field
        if ($json.PendingPythonRuns) {
            foreach ($prop in $json.PendingPythonRuns.PSObject.Properties) {
                $runs = @()
                foreach ($run in $prop.Value) {
                    $runs += @{
                        Time = $run.Time
                        N = $run.N
                        Timestamp = $run.Timestamp
                    }
                }
                $result.PendingPythonRuns[$prop.Name] = $runs
            }
        }
        # Convert PendingLuaRuns (Lua runs waiting to be committed to history)
        if ($json.PendingLuaRuns) {
            foreach ($prop in $json.PendingLuaRuns.PSObject.Properties) {
                $runs = @()
                foreach ($run in $prop.Value) {
                    $runs += @{
                        Time = $run.Time
                        N = $run.N
                        Timestamp = $run.Timestamp
                    }
                }
                $result.PendingLuaRuns[$prop.Name] = $runs
            }
        }
        return $result
    } catch {
        return $null
    }
}

function Save-PendingSession {
    param([hashtable]$Session)
    $Session | ConvertTo-Json -Depth 10 | Set-Content $PendingFile -Encoding UTF8
}

function Clear-PendingSession {
    if (Test-Path $PendingFile) {
        Remove-Item $PendingFile -Force
    }
}

function Add-PendingRun {
    param(
        [string]$Name,
        [int]$N,
        [double]$Time,
        [long]$Instructions,
        [bool]$QuickMode,
        [int]$RunCount
    )

    $session = Get-PendingSession
    if (-not $session) {
        $session = @{
            QuickMode = $QuickMode
            Runs = $RunCount
            Completed = @{}
            PendingRuns = @{}
        }
    }

    # Initialize pending runs array for this benchmark if needed
    if (-not $session.PendingRuns.ContainsKey($Name)) {
        $session.PendingRuns[$Name] = @()
    }

    # Add the run to pending
    $session.PendingRuns[$Name] += @{
        Time = $Time
        Instructions = $Instructions
        N = $N
        Timestamp = (Get-Date).ToString("o")
    }

    Save-PendingSession $session
}

function Add-PendingPythonRun {
    param(
        [string]$Name,
        [int]$N,
        [double]$TimeSingle,
        [double]$TimeMulti,
        [bool]$QuickMode,
        [int]$RunCount
    )

    $session = Get-PendingSession
    if (-not $session) {
        $session = @{
            QuickMode = $QuickMode
            Runs = $RunCount
            Completed = @{}
            PendingRuns = @{}
            PendingPythonRuns = @{}
        }
    }

    # Ensure PendingPythonRuns exists
    if (-not $session.PendingPythonRuns) {
        $session.PendingPythonRuns = @{}
    }

    # Save single-thread run if time > 0
    if ($TimeSingle -gt 0) {
        $key1t = "$Name-1t"
        if (-not $session.PendingPythonRuns.ContainsKey($key1t)) {
            $session.PendingPythonRuns[$key1t] = @()
        }
        $session.PendingPythonRuns[$key1t] += @{
            Time = $TimeSingle
            N = $N
            Timestamp = (Get-Date).ToString("o")
        }
    }

    # Save multi-thread run if time > 0
    if ($TimeMulti -gt 0) {
        $keyMt = "$Name-mt"
        if (-not $session.PendingPythonRuns.ContainsKey($keyMt)) {
            $session.PendingPythonRuns[$keyMt] = @()
        }
        $session.PendingPythonRuns[$keyMt] += @{
            Time = $TimeMulti
            N = $N
            Timestamp = (Get-Date).ToString("o")
        }
    }

    Save-PendingSession $session
}

function Add-PendingLuaRun {
    param(
        [string]$Name,
        [int]$N,
        [double]$Time,
        [bool]$QuickMode,
        [int]$RunCount
    )

    $session = Get-PendingSession
    if (-not $session) {
        $session = @{
            QuickMode = $QuickMode
            Runs = $RunCount
            Completed = @{}
            PendingRuns = @{}
            PendingPythonRuns = @{}
            PendingLuaRuns = @{}
        }
    }

    # Ensure PendingLuaRuns exists
    if (-not $session.PendingLuaRuns) {
        $session.PendingLuaRuns = @{}
    }

    # Initialize pending Lua runs array for this benchmark if needed
    if (-not $session.PendingLuaRuns.ContainsKey($Name)) {
        $session.PendingLuaRuns[$Name] = @()
    }

    # Add the Lua run to pending
    $session.PendingLuaRuns[$Name] += @{
        Time = $Time
        N = $N
        Timestamp = (Get-Date).ToString("o")
    }

    Save-PendingSession $session
}

function Set-PendingBenchmarkComplete {
    param([string]$Name)

    $session = Get-PendingSession
    if ($session) {
        $session.Completed[$Name] = $true
        Save-PendingSession $session
    }
}

function Test-SessionComplete {
    param([hashtable]$Session)

    if (-not $Session) { return $false }

    # Check if all 3 benchmarks are marked as completed
    $allComplete = $true
    foreach ($bench in $Benchmarks) {
        if (-not $Session.Completed.ContainsKey($bench.Name) -or -not $Session.Completed[$bench.Name]) {
            $allComplete = $false
            break
        }
    }
    return $allComplete
}

function Commit-PendingToHistory {
    # Move all pending runs to permanent history
    $session = Get-PendingSession
    if (-not $session) { return }

    $history = Get-BenchmarkHistory

    # Commit SedaiBasic2 runs
    foreach ($benchName in $session.PendingRuns.Keys) {
        if (-not $history.ContainsKey($benchName)) {
            $history[$benchName] = @()
        }
        foreach ($run in $session.PendingRuns[$benchName]) {
            $history[$benchName] += $run
        }
    }

    # Commit Python runs
    if ($session.PendingPythonRuns) {
        foreach ($benchName in $session.PendingPythonRuns.Keys) {
            $pythonKey = "Python_$benchName"
            if (-not $history.ContainsKey($pythonKey)) {
                $history[$pythonKey] = @()
            }
            foreach ($run in $session.PendingPythonRuns[$benchName]) {
                $history[$pythonKey] += $run
            }
        }
    }

    # Commit Lua runs
    if ($session.PendingLuaRuns) {
        foreach ($benchName in $session.PendingLuaRuns.Keys) {
            $luaKey = "Lua_$benchName"
            if (-not $history.ContainsKey($luaKey)) {
                $history[$luaKey] = @()
            }
            foreach ($run in $session.PendingLuaRuns[$benchName]) {
                $history[$luaKey] += $run
            }
        }
    }

    Save-BenchmarkHistory $history
    Clear-PendingSession
}

function Set-BenchmarkResult {
    param(
        [string]$Name,
        [int]$N,
        [double]$ExecutionTime,
        [long]$Instructions,
        [double]$Mips,
        [string]$Output,
        [int]$RunCount,
        [double]$MinTime,
        [double]$MaxTime,
        [double]$Median = 0.0,
        [double]$StdDev = 0.0,
        [double]$P90 = 0.0,
        [double]$P95 = 0.0,
        [double]$P99 = 0.0,
        [double]$PythonSingleTime = 0.0,
        [double]$PythonMultiTime = 0.0,
        [double]$LuaTime = 0.0
    )

    $status = Get-BenchmarkStatus
    $status[$Name] = @{
        Completed = $true
        N = $N
        ExecutionTime = $ExecutionTime
        Instructions = $Instructions
        Mips = $Mips
        Output = $Output
        RunCount = $RunCount
        MinTime = $MinTime
        MaxTime = $MaxTime
        Median = $Median
        StdDev = $StdDev
        P90 = $P90
        P95 = $P95
        P99 = $P99
        PythonSingleTime = $PythonSingleTime
        PythonMultiTime = $PythonMultiTime
        LuaTime = $LuaTime
        Timestamp = (Get-Date).ToString("o")
    }
    Save-BenchmarkStatus $status
}

# ============================================================================
#  STATISTICAL FUNCTIONS
# ============================================================================

function Get-Median {
    param([double[]]$Values)

    if ($Values.Count -eq 0) { return 0.0 }
    if ($Values.Count -eq 1) { return $Values[0] }

    $sorted = $Values | Sort-Object
    $mid = [int]($sorted.Count / 2)

    if ($sorted.Count % 2 -eq 0) {
        return ($sorted[$mid - 1] + $sorted[$mid]) / 2.0
    } else {
        return $sorted[$mid]
    }
}

function Get-StdDev {
    param([double[]]$Values)

    if ($Values.Count -le 1) { return 0.0 }

    $mean = ($Values | Measure-Object -Average).Average
    $sumSquares = 0.0
    foreach ($v in $Values) {
        $sumSquares += ($v - $mean) * ($v - $mean)
    }
    return [Math]::Sqrt($sumSquares / ($Values.Count - 1))
}

function Get-Percentile {
    param(
        [double[]]$Values,
        [int]$Percentile
    )

    if ($Values.Count -eq 0) { return 0.0 }
    if ($Values.Count -eq 1) { return $Values[0] }

    $sorted = $Values | Sort-Object
    $index = ($Percentile / 100.0) * ($sorted.Count - 1)
    $lower = [int][Math]::Floor($index)
    $upper = [int][Math]::Ceiling($index)

    if ($lower -eq $upper) {
        return $sorted[$lower]
    }

    $fraction = $index - $lower
    return $sorted[$lower] + ($sorted[$upper] - $sorted[$lower]) * $fraction
}

# ============================================================================
#  BENCHMARK EXECUTION
# ============================================================================

function Get-BenchmarkFile {
    param(
        [hashtable]$Benchmark,
        [bool]$QuickMode
    )

    $sourcePath = Join-Path $ProjectRoot $Benchmark.Source

    if ($QuickMode) {
        # Quick mode: use source file as-is
        return $sourcePath
    } else {
        # Standard mode: create temp file with standard N value
        $tempPath = Join-Path $TempDir "$($Benchmark.Name).bas"
        $content = Get-Content $sourcePath -Raw
        $newValue = $Benchmark.NReplace -f $Benchmark.StandardN
        $content = $content -replace $Benchmark.NPattern, $newValue
        $content | Set-Content $tempPath -Encoding ASCII -NoNewline
        return $tempPath
    }
}

function Run-SingleBenchmark {
    param(
        [string]$TempFile
    )

    # Simple and reliable: run process and capture output via cmd /c redirection
    # This works correctly for long-running processes and allows CTRL+C interruption
    $stdoutFile = Join-Path $TempDir "stdout_$([System.IO.Path]::GetRandomFileName()).txt"
    $stderrFile = Join-Path $TempDir "stderr_$([System.IO.Path]::GetRandomFileName()).txt"

    try {
        # Use cmd /c to run with output redirection - this properly waits and captures output
        $cmdArgs = "`"$SbExe`" `"$TempFile`" --stats > `"$stdoutFile`" 2> `"$stderrFile`""
        $processResult = cmd /c $cmdArgs
        $exitCode = $LASTEXITCODE

        # Read captured output
        $stdout = if (Test-Path $stdoutFile) { Get-Content $stdoutFile -Raw -ErrorAction SilentlyContinue } else { "" }
        $stderr = if (Test-Path $stderrFile) { Get-Content $stderrFile -Raw -ErrorAction SilentlyContinue } else { "" }
        $output = "$stdout`n$stderr"
    }
    finally {
        # Clean up temp files
        if (Test-Path $stdoutFile) { Remove-Item $stdoutFile -Force -ErrorAction SilentlyContinue }
        if (Test-Path $stderrFile) { Remove-Item $stderrFile -Force -ErrorAction SilentlyContinue }
    }

    $result = @{
        Success = ($exitCode -eq 0)
        Output = $output
        ExitCode = $exitCode
        ExecutionTime = 0.0
        Instructions = 0
        Mips = 0.0
    }

    if ($result.Success) {
        # Parse execution time - format is always: "X.XXX ms" or "X.XXX ms (human readable)"
        # The value is always in milliseconds with 3 decimal places
        if ($output -match "Execution time:\s+([\d.]+)\s+ms") {
            $result.ExecutionTime = [double]$Matches[1] / 1000.0  # ms -> seconds
        }

        # Parse instruction count
        if ($output -match "Instructions executed:\s+([\d,]+)") {
            $result.Instructions = [long]($Matches[1] -replace ",", "")
        }

        # Calculate MIPS from instructions and time
        if ($result.ExecutionTime -gt 0 -and $result.Instructions -gt 0) {
            $result.Mips = $result.Instructions / $result.ExecutionTime / 1000000.0
        }
    }

    return $result
}

function Run-SinglePythonBenchmark {
    param(
        [string]$ScriptPath,
        [long]$N
    )

    # Measure Python execution time using Measure-Command
    $stdoutFile = Join-Path $TempDir "py_stdout_$([System.IO.Path]::GetRandomFileName()).txt"
    $stderrFile = Join-Path $TempDir "py_stderr_$([System.IO.Path]::GetRandomFileName()).txt"

    $result = @{
        Success = $false
        Output = ""
        ExitCode = -1
        ExecutionTime = 0.0
        Instructions = 0  # Python doesn't provide this
        Mips = 0.0        # Python doesn't provide this
    }

    try {
        # Use Measure-Command to get accurate timing
        $sw = [System.Diagnostics.Stopwatch]::StartNew()
        $cmdArgs = "`"$PythonExe`" `"$ScriptPath`" $N > `"$stdoutFile`" 2> `"$stderrFile`""
        $processResult = cmd /c $cmdArgs
        $sw.Stop()
        $exitCode = $LASTEXITCODE

        # Read captured output
        $stdout = if (Test-Path $stdoutFile) { Get-Content $stdoutFile -Raw -ErrorAction SilentlyContinue } else { "" }
        $stderr = if (Test-Path $stderrFile) { Get-Content $stderrFile -Raw -ErrorAction SilentlyContinue } else { "" }

        $result.Success = ($exitCode -eq 0)
        $result.Output = "$stdout`n$stderr"
        $result.ExitCode = $exitCode
        $result.ExecutionTime = $sw.Elapsed.TotalSeconds
    }
    finally {
        # Clean up temp files
        if (Test-Path $stdoutFile) { Remove-Item $stdoutFile -Force -ErrorAction SilentlyContinue }
        if (Test-Path $stderrFile) { Remove-Item $stderrFile -Force -ErrorAction SilentlyContinue }
    }

    return $result
}

function Run-SingleLuaBenchmark {
    param(
        [string]$ScriptPath,
        [long]$N
    )

    # Measure Lua execution time using Stopwatch
    $stdoutFile = Join-Path $TempDir "lua_stdout_$([System.IO.Path]::GetRandomFileName()).txt"
    $stderrFile = Join-Path $TempDir "lua_stderr_$([System.IO.Path]::GetRandomFileName()).txt"

    $result = @{
        Success = $false
        Output = ""
        ExitCode = -1
        ExecutionTime = 0.0
        Instructions = 0  # Lua doesn't provide this
        Mips = 0.0        # Lua doesn't provide this
    }

    try {
        # Use Stopwatch to get accurate timing
        $sw = [System.Diagnostics.Stopwatch]::StartNew()
        $cmdArgs = "`"$LuaExe`" `"$ScriptPath`" $N > `"$stdoutFile`" 2> `"$stderrFile`""
        $processResult = cmd /c $cmdArgs
        $sw.Stop()
        $exitCode = $LASTEXITCODE

        # Read captured output
        $stdout = if (Test-Path $stdoutFile) { Get-Content $stdoutFile -Raw -ErrorAction SilentlyContinue } else { "" }
        $stderr = if (Test-Path $stderrFile) { Get-Content $stderrFile -Raw -ErrorAction SilentlyContinue } else { "" }

        $result.Success = ($exitCode -eq 0)
        $result.Output = "$stdout`n$stderr"
        $result.ExitCode = $exitCode
        $result.ExecutionTime = $sw.Elapsed.TotalSeconds
    }
    finally {
        # Clean up temp files
        if (Test-Path $stdoutFile) { Remove-Item $stdoutFile -Force -ErrorAction SilentlyContinue }
        if (Test-Path $stderrFile) { Remove-Item $stderrFile -Force -ErrorAction SilentlyContinue }
    }

    return $result
}

function Run-PythonBenchmark {
    param(
        [hashtable]$PyBenchmark,
        [long]$N,
        [int]$RunCount,
        [bool]$SingleThread = $true
    )

    $scriptName = if ($SingleThread) { $PyBenchmark.PyScript } else { $PyBenchmark.PyScriptMulti }
    $scriptPath = Join-Path $PyTestsDir $scriptName

    if (!(Test-Path $scriptPath)) {
        Show-Status "Python script not found: $scriptName" -Type "Error"
        return $null
    }

    $threadStr = if ($SingleThread) { "single-thread" } else { "multi-thread" }
    Write-Host "       Running Python $threadStr N=$N..." -ForegroundColor DarkGray

    $sessionTimes = @()
    $lastResult = $null

    for ($i = 1; $i -le $RunCount; $i++) {
        if ($RunCount -gt 1) {
            Write-Host "`r       Python run $i of $RunCount...   " -ForegroundColor DarkGray -NoNewline
        }

        $result = Run-SinglePythonBenchmark -ScriptPath $scriptPath -N $N

        if (-not $result.Success) {
            if ($RunCount -gt 1) { Write-Host "" }
            Show-Status "Python benchmark failed" -Type "Warning"
            return $null
        }

        $sessionTimes += $result.ExecutionTime
        $lastResult = $result
    }

    # Clear the progress line
    if ($RunCount -gt 1) {
        Write-Host "`r                              `r" -NoNewline
    }

    # Calculate statistics
    $avgTime = ($sessionTimes | Measure-Object -Average).Average
    $minTime = ($sessionTimes | Measure-Object -Minimum).Minimum
    $maxTime = ($sessionTimes | Measure-Object -Maximum).Maximum

    return @{
        ExecutionTime = $avgTime
        MinTime = $minTime
        MaxTime = $maxTime
        RunCount = $RunCount
        Output = $lastResult.Output
    }
}

function Run-Benchmark {
    param(
        [hashtable]$Benchmark,
        [bool]$QuickMode,
        [int]$RunCount
    )

    $benchFile = Get-BenchmarkFile -Benchmark $Benchmark -QuickMode $QuickMode
    $currentN = Get-BenchmarkN -Benchmark $Benchmark -QuickMode $QuickMode

    # Get existing history count (committed runs only)
    $existingTimes = Get-HistoryTimes -Name $Benchmark.Name -N $currentN
    $historyCount = $existingTimes.Count

    Show-Status "Running $($Benchmark.Name) N=$currentN..." -Type "Run"
    Write-Host "       $($Benchmark.Description)" -ForegroundColor DarkGray
    if ($historyCount -gt 0) {
        Write-Host "       Historical runs: $historyCount committed" -ForegroundColor DarkGray
    }

    $sessionTimes = @()
    $lastResult = $null

    for ($i = 1; $i -le $RunCount; $i++) {
        if ($RunCount -gt 1) {
            Write-Host "`r       Run $i of $RunCount...   " -ForegroundColor DarkGray -NoNewline
        }

        $result = Run-SingleBenchmark $benchFile

        if (-not $result.Success) {
            if ($RunCount -gt 1) { Write-Host "" }
            Show-Status "Benchmark failed with exit code $($result.ExitCode)" -Type "Error"
            return $false
        }

        $sessionTimes += $result.ExecutionTime
        $lastResult = $result

        # Add each run to PENDING (not history yet - will commit only if all 3 benchmarks complete)
        Add-PendingRun -Name $Benchmark.Name -N $currentN -Time $result.ExecutionTime -Instructions $result.Instructions -QuickMode $QuickMode -RunCount $RunCount
    }

    # Clear the progress line
    if ($RunCount -gt 1) {
        Write-Host "`r                              `r" -NoNewline
    }

    # Mark this benchmark as complete in pending session
    Set-PendingBenchmarkComplete -Name $Benchmark.Name

    # For display purposes, use session times only (history times will be added after commit)
    $avgTime = ($sessionTimes | Measure-Object -Average).Average
    $minTime = ($sessionTimes | Measure-Object -Minimum).Minimum
    $maxTime = ($sessionTimes | Measure-Object -Maximum).Maximum
    $medianTime = Get-Median $sessionTimes
    $stdDev = Get-StdDev $sessionTimes
    $p90 = Get-Percentile $sessionTimes 90
    $p95 = Get-Percentile $sessionTimes 95
    $p99 = Get-Percentile $sessionTimes 99

    # Recalculate MIPS based on average time
    $avgMips = if ($avgTime -gt 0) { $lastResult.Instructions / $avgTime / 1000000 } else { 0 }

    # Save result with session statistics (for display) - Python times will be added later
    Set-BenchmarkResult -Name $Benchmark.Name -N $currentN -ExecutionTime $avgTime -Instructions $lastResult.Instructions -Mips $avgMips -Output $lastResult.Output -RunCount $RunCount -MinTime $minTime -MaxTime $maxTime -Median $medianTime -StdDev $stdDev -P90 $p90 -P95 $p95 -P99 $p99

    # Display results
    $timeStr = "{0:F3} ms" -f ($avgTime * 1000)
    if ($RunCount -gt 1) {
        $timeStr += " (median: {0:F3} ms, stddev: {1:F3} ms)" -f ($medianTime * 1000), ($stdDev * 1000)
    }
    $mipsStr = "{0:F2}" -f $avgMips

    # Show pending status
    $pendingStr = " [pending commit]"
    if ($historyCount -gt 0) {
        $pendingStr = " [+$RunCount pending, $historyCount committed]"
    }
    Show-Status "$($Benchmark.Name) completed in $timeStr ($mipsStr MIPS)$pendingStr" -Type "Success"

    return $true
}

# ============================================================================
#  RESULTS GENERATION
# ============================================================================

function Generate-Results {
    param(
        [hashtable]$SystemInfo,
        [bool]$QuickMode,
        [int]$RunCount
    )

    $status = Get-BenchmarkStatus

    $date = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $modeStr = if ($QuickMode) { "Quick (N from source files)" } else { "Standard (Benchmarks Game N values)" }

    # Calculate total runs from status (each benchmark may have different counts)
    $totalRunsInfo = @()
    foreach ($bench in $Script:AllBenchmarks) {
        $result = $status[$bench.Name]
        if ($result -and $result.RunCount) {
            $totalRunsInfo += "$($bench.Name): $($result.RunCount)"
        }
    }
    $runsLine = if ($totalRunsInfo.Count -gt 0) { $totalRunsInfo -join ", " } else { "$RunCount" }

    $md = @"
# SedaiBasic2 Benchmark Results

``````
   ____           _       _ ____            _      ____
  / ___|  ___  __| | __ _(_) __ )  __ _ ___(_) ___|___ \
  \___ \ / _ \/ _`  |/ _`  | |  _ \ / _`  / __| |/ __| __) |
   ___) |  __/ (_| | (_| | | |_) | (_| \__ \ | (__ / __/
  |____/ \___|\__,_|\__,_|_|____/ \__,_|___/_|\___|_____|
``````

**Generated:** $date
**Mode:** $modeStr
**Total runs:** $runsLine

## System Information

| Property | Value |
|----------|-------|
| **OS** | $($SystemInfo.OS) |
| **OS Version** | $($SystemInfo.OSVersion) |
| **Architecture** | $($SystemInfo.Architecture) |
| **CPU** | $($SystemInfo.CPU) |
| **CPU Cores** | $($SystemInfo.CPUCores) |
| **CPU Threads** | $($SystemInfo.CPUThreads) |
| **CPU Max Clock** | $($SystemInfo.CPUMaxClock) |
| **RAM** | $($SystemInfo.RAM) |
| **Interpreter** | SedaiBasic2 (register-based bytecode VM) |

## Benchmark Results

| Benchmark | N | Execution Time | Instructions | Speed (MIPS) |
|-----------|--:|---------------:|-------------:|-------------:|
"@

    foreach ($bench in $Script:AllBenchmarks) {
        $result = $status[$bench.Name]
        if ($result -and $result.Completed) {
            $time = "{0:F3} ms" -f ($result.ExecutionTime * 1000)
            $instr = "{0:N0}" -f $result.Instructions
            $mips = "{0:F2}" -f $result.Mips
            $n = "{0:N0}" -f $result.N
            $md += "`n| $($bench.Name) | $n | $time | $instr | $mips |"
        } else {
            $expectedN = Get-BenchmarkN -Benchmark $bench -QuickMode $QuickMode
            $n = "{0:N0}" -f $expectedN
            $md += "`n| $($bench.Name) | $n | - | - | - |"
        }
    }

    # Check if any benchmark has Python results
    $hasPythonResults = $false
    foreach ($bench in $Script:AllBenchmarks) {
        $result = $status[$bench.Name]
        if ($result -and ($result.PythonSingleTime -gt 0 -or $result.PythonMultiTime -gt 0)) {
            $hasPythonResults = $true
            break
        }
    }

    # Add Python comparison section
    if ($hasPythonResults) {
        $md += @"


## Python Comparison

| Benchmark | SedaiBasic2 | Python (1T) | Speedup (1T) | Python (MT) | Speedup (MT) |
|-----------|------------:|------------:|-------------:|------------:|-------------:|
"@
        foreach ($bench in $Script:AllBenchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed) {
                $sbTime = "{0:F3} ms" -f ($result.ExecutionTime * 1000)
                $pySingle = if ($result.PythonSingleTime -gt 0) { "{0:F3} ms" -f ($result.PythonSingleTime * 1000) } else { "-" }
                $speedupSingle = if ($result.PythonSingleTime -gt 0) { "{0:F2}x" -f ($result.PythonSingleTime / $result.ExecutionTime) } else { "-" }
                $pyMulti = if ($result.PythonMultiTime -gt 0) { "{0:F3} ms" -f ($result.PythonMultiTime * 1000) } else { "-" }
                $speedupMulti = if ($result.PythonMultiTime -gt 0) { "{0:F2}x" -f ($result.PythonMultiTime / $result.ExecutionTime) } else { "-" }
                $md += "`n| $($bench.Name) | $sbTime | $pySingle | $speedupSingle | $pyMulti | $speedupMulti |"
            }
        }
    }

    # Check if any benchmark has Lua results
    $hasLuaResults = $false
    foreach ($bench in $Script:AllBenchmarks) {
        $result = $status[$bench.Name]
        if ($result -and $result.LuaTime -gt 0) {
            $hasLuaResults = $true
            break
        }
    }

    # Add Lua comparison section
    if ($hasLuaResults) {
        $md += @"


## Lua Comparison

| Benchmark | SedaiBasic2 | Lua 5.4 | Speedup |
|-----------|------------:|--------:|--------:|
"@
        foreach ($bench in $Script:AllBenchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed) {
                $sbTime = "{0:F3} ms" -f ($result.ExecutionTime * 1000)
                $luaTime = if ($result.LuaTime -gt 0) { "{0:F3} ms" -f ($result.LuaTime * 1000) } else { "-" }
                $speedup = if ($result.LuaTime -gt 0) { "{0:F2}x" -f ($result.LuaTime / $result.ExecutionTime) } else { "-" }
                $md += "`n| $($bench.Name) | $sbTime | $luaTime | $speedup |"
            }
        }
    }

    # Check if any benchmark has multiple runs
    $hasMultipleRuns = $false
    foreach ($bench in $Script:AllBenchmarks) {
        $result = $status[$bench.Name]
        if ($result -and $result.RunCount -gt 1) {
            $hasMultipleRuns = $true
            break
        }
    }

    # Add timing details if multiple runs
    if ($hasMultipleRuns) {
        $md += @"


### Statistical Analysis (Cumulative Runs)

| Benchmark | Runs | Mean | Median | Std Dev | Min | Max |
|-----------|-----:|-----:|-------:|--------:|----:|----:|
"@
        foreach ($bench in $Script:AllBenchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $runs = $result.RunCount
                $mean = "{0:F3} ms" -f ($result.ExecutionTime * 1000)
                $median = "{0:F3} ms" -f ($result.Median * 1000)
                $stddev = "{0:F3} ms" -f ($result.StdDev * 1000)
                $min = "{0:F3} ms" -f ($result.MinTime * 1000)
                $max = "{0:F3} ms" -f ($result.MaxTime * 1000)
                $md += "`n| $($bench.Name) | $runs | $mean | $median | $stddev | $min | $max |"
            }
        }

        $md += @"


### Percentiles

| Benchmark | P90 | P95 | P99 |
|-----------|----:|----:|----:|
"@
        foreach ($bench in $Script:AllBenchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $p90 = "{0:F3} ms" -f ($result.P90 * 1000)
                $p95 = "{0:F3} ms" -f ($result.P95 * 1000)
                $p99 = "{0:F3} ms" -f ($result.P99 * 1000)
                $md += "`n| $($bench.Name) | $p90 | $p95 | $p99 |"
            }
        }
    }

    $md += @"


## Benchmark Descriptions

| Benchmark | Description |
|-----------|-------------|
| fannkuch-redux | Indexed-access to tiny integer-sequence (pancake flipping) |
| n-body | Double-precision N-body simulation (planetary orbits) |
| spectral-norm | Eigenvalue computation using the power method |

## Notes

- Benchmarks from [The Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- Times measured using SedaiBasic2's built-in ``--stats`` option
- MIPS = Million Instructions Per Second (VM bytecode instructions)
- Results may vary based on system load and thermal throttling
- **Statistical measures** (when using -Runs > 1):
  - **Mean**: Arithmetic average of all runs
  - **Median**: Middle value (more robust to outliers)
  - **Std Dev**: Standard deviation (sample, n-1)
  - **P90/P95/P99**: Percentiles indicating the time below which 90%/95%/99% of runs completed
"@

    $md | Set-Content $ResultsFile -Encoding UTF8
    $outputFileName = [System.IO.Path]::GetFileName($ResultsFile)
    Show-Status "Results saved to $outputFileName" -Type "Success"

    # Display results on screen
    Write-Host ""
    Write-Host "  ============================================================" -ForegroundColor Cyan
    Write-Host "  BENCHMARK RESULTS" -ForegroundColor Yellow
    Write-Host "  ============================================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  System: $($SystemInfo.CPU)" -ForegroundColor White
    Write-Host "  OS: $($SystemInfo.OS) ($($SystemInfo.Architecture))" -ForegroundColor Gray
    Write-Host "  RAM: $($SystemInfo.RAM)" -ForegroundColor Gray
    Write-Host ""

    # Display results table
    Write-Host ("  {0,-18} {1,12} {2,14} {3,16} {4,12}" -f "Benchmark", "N", "Time (ms)", "Instructions", "MIPS") -ForegroundColor Cyan
    Write-Host ("  {0,-18} {1,12} {2,14} {3,16} {4,12}" -f ("-" * 18), ("-" * 12), ("-" * 14), ("-" * 16), ("-" * 12)) -ForegroundColor DarkGray

    foreach ($bench in $Benchmarks) {
        $result = $status[$bench.Name]
        if ($result -and $result.Completed) {
            $time = "{0:F3}" -f ($result.ExecutionTime * 1000)
            $instr = "{0:N0}" -f $result.Instructions
            $mips = "{0:F2}" -f $result.Mips
            $n = "{0:N0}" -f $result.N
            Write-Host ("  {0,-18} {1,12} {2,14} {3,16} {4,12}" -f $bench.Name, $n, $time, $instr, $mips) -ForegroundColor White
        } else {
            $expectedN = Get-BenchmarkN -Benchmark $bench -QuickMode $QuickMode
            $n = "{0:N0}" -f $expectedN
            Write-Host ("  {0,-18} {1,12} {2,14} {3,16} {4,12}" -f $bench.Name, $n, "-", "-", "-") -ForegroundColor DarkGray
        }
    }

    # Display Python comparison if available
    if ($hasPythonResults) {
        Write-Host ""
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host "  PYTHON COMPARISON" -ForegroundColor Yellow
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host ""
        Write-Host ("  {0,-18} {1,12} {2,12} {3,8} {4,12} {5,8}" -f "Benchmark", "SedaiBasic2", "Python(1T)", "Speedup", "Python(MT)", "Speedup") -ForegroundColor Cyan
        Write-Host ("  {0,-18} {1,12} {2,12} {3,8} {4,12} {5,8}" -f ("-" * 18), ("-" * 12), ("-" * 12), ("-" * 8), ("-" * 12), ("-" * 8)) -ForegroundColor DarkGray

        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed) {
                $sbTime = "{0:F3}" -f ($result.ExecutionTime * 1000)
                $pySingle = if ($result.PythonSingleTime -gt 0) { "{0:F3}" -f ($result.PythonSingleTime * 1000) } else { "-" }
                $speedupSingle = if ($result.PythonSingleTime -gt 0) { "{0:F2}x" -f ($result.PythonSingleTime / $result.ExecutionTime) } else { "-" }
                $pyMulti = if ($result.PythonMultiTime -gt 0) { "{0:F3}" -f ($result.PythonMultiTime * 1000) } else { "-" }
                $speedupMulti = if ($result.PythonMultiTime -gt 0) { "{0:F2}x" -f ($result.PythonMultiTime / $result.ExecutionTime) } else { "-" }
                Write-Host ("  {0,-18} {1,12} {2,12} {3,8} {4,12} {5,8}" -f $bench.Name, $sbTime, $pySingle, $speedupSingle, $pyMulti, $speedupMulti) -ForegroundColor White
            }
        }
    }

    # Display Lua comparison if available
    if ($hasLuaResults) {
        Write-Host ""
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host "  LUA COMPARISON" -ForegroundColor Yellow
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host ""
        Write-Host ("  {0,-18} {1,12} {2,12} {3,8}" -f "Benchmark", "SedaiBasic2", "Lua 5.4", "Speedup") -ForegroundColor Cyan
        Write-Host ("  {0,-18} {1,12} {2,12} {3,8}" -f ("-" * 18), ("-" * 12), ("-" * 12), ("-" * 8)) -ForegroundColor DarkGray

        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed) {
                $sbTime = "{0:F3}" -f ($result.ExecutionTime * 1000)
                $luaTime = if ($result.LuaTime -gt 0) { "{0:F3}" -f ($result.LuaTime * 1000) } else { "-" }
                $speedup = if ($result.LuaTime -gt 0) { "{0:F2}x" -f ($result.LuaTime / $result.ExecutionTime) } else { "-" }
                Write-Host ("  {0,-18} {1,12} {2,12} {3,8}" -f $bench.Name, $sbTime, $luaTime, $speedup) -ForegroundColor White
            }
        }
    }

    # Display statistical analysis if any benchmark has multiple runs
    if ($hasMultipleRuns) {
        Write-Host ""
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host "  STATISTICAL ANALYSIS - Cumulative Runs" -ForegroundColor Yellow
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host ""
        Write-Host ("  {0,-18} {1,6} {2,10} {3,10} {4,10} {5,10} {6,10}" -f "Benchmark", "Runs", "Mean(ms)", "Med(ms)", "StdDev", "Min(ms)", "Max(ms)") -ForegroundColor Cyan
        Write-Host ("  {0,-18} {1,6} {2,10} {3,10} {4,10} {5,10} {6,10}" -f ("-" * 18), ("-" * 6), ("-" * 10), ("-" * 10), ("-" * 10), ("-" * 10), ("-" * 10)) -ForegroundColor DarkGray

        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $runs = $result.RunCount
                $mean = "{0:F3}" -f ($result.ExecutionTime * 1000)
                $median = "{0:F3}" -f ($result.Median * 1000)
                $stddev = "{0:F3}" -f ($result.StdDev * 1000)
                $min = "{0:F3}" -f ($result.MinTime * 1000)
                $max = "{0:F3}" -f ($result.MaxTime * 1000)
                Write-Host ("  {0,-18} {1,6} {2,10} {3,10} {4,10} {5,10} {6,10}" -f $bench.Name, $runs, $mean, $median, $stddev, $min, $max) -ForegroundColor White
            }
        }

        Write-Host ""
        Write-Host ("  {0,-18} {1,10} {2,10} {3,10}" -f "Benchmark", "P90(ms)", "P95(ms)", "P99(ms)") -ForegroundColor Cyan
        Write-Host ("  {0,-18} {1,10} {2,10} {3,10}" -f ("-" * 18), ("-" * 10), ("-" * 10), ("-" * 10)) -ForegroundColor DarkGray

        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $p90 = "{0:F3}" -f ($result.P90 * 1000)
                $p95 = "{0:F3}" -f ($result.P95 * 1000)
                $p99 = "{0:F3}" -f ($result.P99 * 1000)
                Write-Host ("  {0,-18} {1,10} {2,10} {3,10}" -f $bench.Name, $p90, $p95, $p99) -ForegroundColor White
            }
        }
    }

    Write-Host ""
    Write-Host "  ============================================================" -ForegroundColor Cyan
}

# ============================================================================
#  CLEANUP
# ============================================================================

function Cleanup-TempDir {
    if (Test-Path $TempDir) {
        # Remove contents but keep the directory
        Get-ChildItem -Path $TempDir -Force | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
        Show-Status "Temporary files cleaned up" -Type "Success"
    }
}

# ============================================================================
#  REPORT GENERATION FROM HISTORY
# ============================================================================

function Get-HistoryStats {
    param(
        [string]$Name,
        [int]$N
    )

    $times = Get-HistoryTimes -Name $Name -N $N
    if ($times.Count -eq 0) {
        return $null
    }

    # Get instructions from history
    $history = Get-BenchmarkHistory
    $instructions = 0
    if ($history.ContainsKey($Name)) {
        foreach ($run in $history[$Name]) {
            if ($run.N -eq $N) {
                $instructions = $run.Instructions
                break
            }
        }
    }

    $avgTime = ($times | Measure-Object -Average).Average
    $minTime = ($times | Measure-Object -Minimum).Minimum
    $maxTime = ($times | Measure-Object -Maximum).Maximum
    $medianTime = Get-Median $times
    $stdDev = Get-StdDev $times
    $p90 = Get-Percentile $times 90
    $p95 = Get-Percentile $times 95
    $p99 = Get-Percentile $times 99
    $mips = if ($avgTime -gt 0 -and $instructions -gt 0) { $instructions / $avgTime / 1000000 } else { 0 }

    return @{
        RunCount = $times.Count
        ExecutionTime = $avgTime
        MinTime = $minTime
        MaxTime = $maxTime
        Median = $medianTime
        StdDev = $stdDev
        P90 = $p90
        P95 = $p95
        P99 = $p99
        Instructions = $instructions
        Mips = $mips
        N = $N
    }
}

function Get-PythonHistoryStats {
    param(
        [string]$Name,
        [int]$N
    )

    $history = Get-BenchmarkHistory

    # Look for separate -1t and -mt keys
    $pythonKey1t = "Python_$Name-1t"
    $pythonKeyMt = "Python_$Name-mt"

    # Filter by N value and extract times from each series
    $singleTimes = @()
    $multiTimes = @()

    if ($history.ContainsKey($pythonKey1t)) {
        foreach ($run in $history[$pythonKey1t]) {
            if ($run.N -eq $N -and $run.Time -gt 0) {
                $singleTimes += $run.Time
            }
        }
    }

    if ($history.ContainsKey($pythonKeyMt)) {
        foreach ($run in $history[$pythonKeyMt]) {
            if ($run.N -eq $N -and $run.Time -gt 0) {
                $multiTimes += $run.Time
            }
        }
    }

    if ($singleTimes.Count -eq 0 -and $multiTimes.Count -eq 0) {
        return $null
    }

    $avgSingle = if ($singleTimes.Count -gt 0) { ($singleTimes | Measure-Object -Average).Average } else { 0.0 }
    $avgMulti = if ($multiTimes.Count -gt 0) { ($multiTimes | Measure-Object -Average).Average } else { 0.0 }

    return @{
        RunCount = [Math]::Max($singleTimes.Count, $multiTimes.Count)
        PythonSingleTime = $avgSingle
        PythonMultiTime = $avgMulti
        N = $N
    }
}

function Get-LuaHistoryStats {
    param(
        [string]$Name,
        [int]$N
    )

    $history = Get-BenchmarkHistory
    $luaKey = "Lua_$Name"

    if (-not $history.ContainsKey($luaKey)) {
        return $null
    }

    # Filter by N value and extract times
    $times = @()
    foreach ($run in $history[$luaKey]) {
        if ($run.N -eq $N) {
            if ($run.Time -gt 0) {
                $times += $run.Time
            }
        }
    }

    if ($times.Count -eq 0) {
        return $null
    }

    $avgTime = ($times | Measure-Object -Average).Average

    return @{
        RunCount = $times.Count
        LuaTime = $avgTime
        N = $N
    }
}

function Generate-ReportFromHistory {
    param(
        [bool]$QuickMode
    )

    # Check if history exists
    $history = Get-BenchmarkHistory
    if ($history.Count -eq 0) {
        Show-Status "No benchmark history found." -Type "Error"
        Show-Status "Run benchmarks first to generate data." -Type "Info"
        return $false
    }

    # Check which benchmarks have data
    $hasData = $false
    foreach ($bench in $Benchmarks) {
        $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $QuickMode
        $times = Get-HistoryTimes -Name $bench.Name -N $currentN
        if ($times.Count -gt 0) {
            $hasData = $true
            break
        }
    }

    if (-not $hasData) {
        $modeStr = if ($QuickMode) { "Quick" } else { "Standard" }
        Show-Status "No benchmark history found for $modeStr mode." -Type "Error"
        Show-Status "Run benchmarks first to generate data." -Type "Info"
        return $false
    }

    # Build status from history
    $status = @{}
    foreach ($bench in $Benchmarks) {
        $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $QuickMode
        $stats = Get-HistoryStats -Name $bench.Name -N $currentN
        if ($stats) {
            # Get Python stats if available
            $pythonStats = Get-PythonHistoryStats -Name $bench.Name -N $currentN
            $pythonSingleTime = if ($pythonStats) { $pythonStats.PythonSingleTime } else { 0.0 }
            $pythonMultiTime = if ($pythonStats) { $pythonStats.PythonMultiTime } else { 0.0 }

            # Get Lua stats if available
            $luaStats = Get-LuaHistoryStats -Name $bench.Name -N $currentN
            $luaTime = if ($luaStats) { $luaStats.LuaTime } else { 0.0 }

            $status[$bench.Name] = @{
                Completed = $true
                N = $stats.N
                ExecutionTime = $stats.ExecutionTime
                Instructions = $stats.Instructions
                Mips = $stats.Mips
                RunCount = $stats.RunCount
                MinTime = $stats.MinTime
                MaxTime = $stats.MaxTime
                Median = $stats.Median
                StdDev = $stats.StdDev
                P90 = $stats.P90
                P95 = $stats.P95
                P99 = $stats.P99
                PythonSingleTime = $pythonSingleTime
                PythonMultiTime = $pythonMultiTime
                LuaTime = $luaTime
            }
        }
    }

    # Save to status file for Generate-Results to use
    Save-BenchmarkStatus $status

    return $true
}

# ============================================================================
#  MAIN
# ============================================================================

function Invoke-Benchmarks {
    # Handle -Help parameter
    if ($Help) {
        Show-Help
        return 0
    }

    # Handle -ClearHistory parameter
    if ($ClearHistory) {
        Show-Banner
        Clear-BenchmarkHistory
        Clear-PendingSession
        if (Test-Path $StatusFile) {
            Remove-Item $StatusFile -Force
        }
        Show-Status "Benchmark history and pending session cleared" -Type "Success"
        Write-Host ""
        return 0
    }

    # Handle -Output parameter: user-specified filename or auto-generate timestamped name
    if ($Output -ne "") {
        if ([System.IO.Path]::IsPathRooted($Output)) {
            $Script:ResultsFile = $Output
        } else {
            $Script:ResultsFile = Join-Path $ResultsDir $Output
        }
    } else {
        # Auto-generate timestamped filename: BENCHMARK_YYYY-MM-DD-SSSSS.md
        $Script:ResultsFile = Get-TimestampedResultsFile
    }

    # Handle -Report parameter (generate report from existing history only)
    if ($Report) {
        Show-Banner
        Write-Host "  Mode: REPORT ONLY - from existing history" -ForegroundColor Yellow
        Write-Host ""

        # Create directories if needed
        if (!(Test-Path $TempDir)) {
            New-Item -ItemType Directory -Path $TempDir -Force | Out-Null
        }
        if (!(Test-Path $HistoryDir)) {
            New-Item -ItemType Directory -Path $HistoryDir -Force | Out-Null
        }
        if (!(Test-Path $ResultsDir)) {
            New-Item -ItemType Directory -Path $ResultsDir -Force | Out-Null
        }

        if (!(Generate-ReportFromHistory -QuickMode $Quick)) {
            return 1
        }

        $systemInfo = Get-SystemInfo
        $status = Get-BenchmarkStatus

        # Determine max run count for display
        $maxRuns = 1
        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.RunCount -gt $maxRuns) {
                $maxRuns = $result.RunCount
            }
        }

        Generate-Results -SystemInfo $systemInfo -QuickMode $Quick -RunCount $maxRuns

        $outputFileName = [System.IO.Path]::GetFileName($ResultsFile)
        Show-Status "Report generated from history: $outputFileName" -Type "Success"
        Write-Host ""
        return 0
    }

    Show-Banner

    # Handle -ResetConfig parameter
    if ($ResetConfig) {
        Reset-Config
        Write-Host "  [OK] Configuration reset" -ForegroundColor Green
        Write-Host ""
    }

    # Load existing configuration
    $configLoaded = Load-Config
    if ($configLoaded) {
        Write-Host "  Configuration loaded from: benchmark.config.json" -ForegroundColor Gray
    }

    # Run interactive setup if no config exists (or ResetConfig was used)
    if (-not $ConfigurationComplete) {
        # First run or config file missing - must configure
        Run-RuntimeSetup

        # Check if user completed the setup
        if (-not $ConfigurationComplete) {
            Write-Host ""
            Write-Host "  [!] Configuration required to continue." -ForegroundColor Red
            Write-Host ""
            return 1
        }
    } else {
        # Config exists, show current config and ask if user wants to change
        Write-Host ""
        Write-Host "  Current runtime configuration:" -ForegroundColor Cyan
        if ($UserConfig.PythonPath -eq "disabled") {
            Write-Host '    Python: [disabled]' -ForegroundColor DarkGray
        } elseif ($UserConfig.PythonPath) {
            Write-Host "    Python: $($UserConfig.PythonPath)" -ForegroundColor Gray
        } else {
            Write-Host '    Python: [Sedai package]' -ForegroundColor Gray
        }
        if ($UserConfig.LuaPath -eq "disabled") {
            Write-Host '    Lua: [disabled]' -ForegroundColor DarkGray
        } elseif ($UserConfig.LuaPath) {
            Write-Host "    Lua: $($UserConfig.LuaPath)" -ForegroundColor Gray
        } else {
            Write-Host '    Lua: [Sedai package]' -ForegroundColor Gray
        }
        Write-Host ""
        Write-Host "  Do you want to modify the configuration? [y/N]: " -NoNewline -ForegroundColor Yellow
        $change = Read-Host
        if ($change -ieq 'y') {
            Run-RuntimeSetup
        }
    }

    # Apply configuration to runtime paths
    Apply-RuntimeConfig

    # Show mode
    if ($Quick) {
        Write-Host '  Mode: QUICK - N values from source files' -ForegroundColor Yellow
    } else {
        Write-Host '  Mode: STANDARD - Benchmarks Game N values' -ForegroundColor Green
    }

    # Determine run count and mode
    $Script:IsUpdateMode = ($Update.Count -gt 0) -or ($Runtime.Count -gt 0)
    $Script:EffectiveRuns = $Runs

    if ($Script:IsUpdateMode) {
        # Update mode: read runs from history
        $runCounts = Get-HistoryRunCounts
        $globalMax = Get-GlobalMaxRuns

        if ($globalMax -gt 0) {
            $Script:EffectiveRuns = $globalMax
            Write-Host "  Runs: $globalMax - from history" -ForegroundColor Cyan
            # Check for misalignment
            Show-RunCountMisalignment -Counts $runCounts | Out-Null
        } else {
            # History empty, use -Runs parameter
            Write-Host "  Runs: $Runs - history empty, using -Runs" -ForegroundColor Cyan
        }
        Write-Host "  Update mode: results will REPLACE history for selected benchmarks/runtimes" -ForegroundColor Yellow
    } else {
        # Full mode with -Runs: clear history and run fresh
        if ($Runs -gt 1) {
            Write-Host "  Runs: $Runs per benchmark - will REPLACE history" -ForegroundColor Cyan
        } else {
            Write-Host "  Runs: 1 per benchmark" -ForegroundColor Cyan
        }
    }
    Write-Host ""

    # Check if sb.exe exists
    if (!(Test-Path $SbExe)) {
        Show-Status "SedaiBasic2 interpreter not found: $SbExe" -Type "Error"
        Show-Status "Please run setup.ps1 first to build the interpreter." -Type "Info"
        return 1
    }

    # Check available runtimes and install if needed
    Write-Host ""
    Write-Host "  ----------------------------------------------------------------" -ForegroundColor DarkGray
    Write-Host "  CHECKING COMPARISON RUNTIMES" -ForegroundColor White
    Write-Host "  ----------------------------------------------------------------" -ForegroundColor DarkGray
    $hasRuntime = $false

    # Check Python
    if ($UserConfig.PythonPath -eq "disabled") {
        Show-Status "Python: disabled by configuration" -Type "Skip"
    } elseif ($UserConfig.PythonPath) {
        # Custom path specified
        if (Test-Path $PythonExe) {
            $pyVersion = & $PythonExe --version 2>&1
            Show-Status "$pyVersion (custom: $PythonExe)" -Type "Success"
            $hasRuntime = $true
        } else {
            Show-Status "Python not found at: $PythonExe" -Type "Error"
            $Script:RunPython = $false
        }
    } else {
        # Use Sedai package - install if not present
        if (!(Test-Path $PythonExe)) {
            Show-Status "Python not found, downloading..." -Type "Warning"
            Write-Host ""
            $pythonInstaller = Join-Path $ProjectRoot "scripts\windows\install-python.ps1"
            if (Test-Path $pythonInstaller) {
                & $pythonInstaller
                Write-Host ""
                if ($LASTEXITCODE -eq 0 -or $LASTEXITCODE -eq 5) {
                    Show-Status "Python installed successfully" -Type "Success"
                } else {
                    Show-Status "Python installation failed (exit code: $LASTEXITCODE)" -Type "Error"
                }
            } else {
                Show-Status "Python installer not found: $pythonInstaller" -Type "Error"
            }
        }

        if (Test-Path $PythonExe) {
            $pyVersion = & $PythonExe --version 2>&1
            Show-Status "$pyVersion available (Sedai package)" -Type "Success"
            $hasRuntime = $true
        }
    }

    # Check Lua
    if ($UserConfig.LuaPath -eq "disabled") {
        Show-Status "Lua: disabled by configuration" -Type "Skip"
    } elseif ($UserConfig.LuaPath) {
        # Custom path specified
        if (Test-Path $LuaExe) {
            $luaVersion = & $LuaExe -v 2>&1
            Show-Status "$luaVersion (custom: $LuaExe)" -Type "Success"
            $hasRuntime = $true
        } else {
            Show-Status "Lua not found at: $LuaExe" -Type "Error"
            $Script:RunLua = $false
        }
    } else {
        # Use Sedai package - install if not present
        if (!(Test-Path $LuaExe)) {
            Show-Status "Lua not found, downloading..." -Type "Warning"
            Write-Host ""
            $luaInstaller = Join-Path $ProjectRoot "scripts\windows\install-lua.ps1"
            if (Test-Path $luaInstaller) {
                & $luaInstaller
                Write-Host ""
                if ($LASTEXITCODE -eq 0 -or $LASTEXITCODE -eq 5) {
                    Show-Status "Lua installed successfully" -Type "Success"
                } else {
                    Show-Status "Lua installation failed (exit code: $LASTEXITCODE)" -Type "Error"
                }
            } else {
                Show-Status "Lua installer not found: $luaInstaller" -Type "Error"
            }
        }

        if (Test-Path $LuaExe) {
            $luaVersion = & $LuaExe -v 2>&1
            Show-Status "$luaVersion available (Sedai package)" -Type "Success"
            $hasRuntime = $true
        }
    }

    if (-not $hasRuntime -and $RunPython -eq $false -and $RunLua -eq $false) {
        Show-Status "All comparison runtimes disabled (benchmarks will run SedaiBasic only)" -Type "Warning"
    } elseif (-not $hasRuntime) {
        Show-Status "No comparison runtime available (benchmarks will run SedaiBasic only)" -Type "Warning"
    }
    Write-Host ""

    # Get system info
    Write-Host "  Collecting system information..." -ForegroundColor Gray
    $systemInfo = Get-SystemInfo
    Write-Host "  CPU: $($systemInfo.CPU)" -ForegroundColor Gray
    Write-Host "  Cores/Threads: $($systemInfo.CPUCores)/$($systemInfo.CPUThreads)" -ForegroundColor Gray
    Write-Host "  RAM: $($systemInfo.RAM)" -ForegroundColor Gray
    Write-Host ""

    # Create temp, data, and results directories
    if (!(Test-Path $TempDir)) {
        New-Item -ItemType Directory -Path $TempDir -Force | Out-Null
    }
    if (!(Test-Path $HistoryDir)) {
        New-Item -ItemType Directory -Path $HistoryDir -Force | Out-Null
    }
    if (!(Test-Path $ResultsDir)) {
        New-Item -ItemType Directory -Path $ResultsDir -Force | Out-Null
    }

    # Clean temp directory at the start of each session
    Get-ChildItem -Path $TempDir -Force -ErrorAction SilentlyContinue | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue

    # Check for pending session (incomplete previous run)
    $pendingSession = $null
    $resumingSession = $false

    $pendingSession = Get-PendingSession

    if ($pendingSession -and -not $Force) {
        # Check if pending session mode matches current mode
        $pendingQuick = $pendingSession.QuickMode
        if ($pendingQuick -eq $Quick) {
            $resumingSession = $true
            Show-Status "Found incomplete session from previous run" -Type "Warning"

            # Count completed benchmarks in pending
            $pendingCompleted = 0
            foreach ($bench in $Benchmarks) {
                if ($pendingSession.Completed.ContainsKey($bench.Name) -and $pendingSession.Completed[$bench.Name]) {
                    $pendingCompleted++
                }
            }
            Show-Status "Resuming: $pendingCompleted of $($Benchmarks.Count) benchmarks completed" -Type "Info"
            Write-Host ""
        } else {
            # Mode mismatch - clear pending and start fresh
            Show-Status "Previous session was in different mode, starting fresh" -Type "Info"
            Clear-PendingSession
            Write-Host ""
        }
    }

    # Clear status and pending if Force flag is set
    if ($Force) {
        if (Test-Path $StatusFile) {
            Remove-Item $StatusFile -Force
        }
        Clear-PendingSession
        Show-Status "Previous session results cleared" -Type "Info"
    }

    # Determine which benchmarks need to run
    $toRun = @()
    if ($resumingSession) {
        # Resume: run only benchmarks not completed in pending session
        foreach ($bench in $Benchmarks) {
            if (-not $pendingSession.Completed.ContainsKey($bench.Name) -or -not $pendingSession.Completed[$bench.Name]) {
                $toRun += $bench
            }
        }

        # If all benchmarks in pending are complete, commit and start fresh
        if ($toRun.Count -eq 0) {
            $session = Get-PendingSession
            if (Test-SessionComplete -Session $session) {
                Commit-PendingToHistory
                Show-Status "Previous session completed! Runs committed to history." -Type "Success"
            }
            Clear-PendingSession
            $resumingSession = $false
            # Now start fresh - run all benchmarks
            $toRun = $Benchmarks
        }
    } else {
        # Fresh start: always run all benchmarks (no status file check)
        # Status file is only used within a session, not to skip entire runs
        if (Test-Path $StatusFile) {
            Remove-Item $StatusFile -Force
        }
        $toRun = $Benchmarks
    }

    # Show warning (skip in quick mode)
    if (-not $Quick) {
        Show-Warning
    }

    Write-Host "  Benchmarks to run: $($toRun.Count) of $($Benchmarks.Count)" -ForegroundColor White
    foreach ($bench in $toRun) {
        $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
        Write-Host "    - $($bench.Name) N=$currentN" -ForegroundColor Gray
    }
    Write-Host ""

    $response = Read-Host "  Press ENTER to continue or 'q' to quit"
    if ($response -eq 'q') {
        Show-Status "Benchmark cancelled by user" -Type "Warning"
        if ($resumingSession) {
            Show-Status "Pending session preserved for later resume" -Type "Info"
        }
        return 0
    }

    # Clear history as needed before running tests
    if ($Script:IsUpdateMode) {
        # Update mode: clear only selected benchmark/runtime combinations
        $history = Get-BenchmarkHistory
        $modified = $false

        foreach ($bench in $toRun) {
            # Clear SedaiBasic2 history for this benchmark if running sedai
            if ($Script:RunSedai -and $history.ContainsKey($bench.Name)) {
                $history.Remove($bench.Name)
                $modified = $true
            }
            # Clear Python history for this benchmark if running python
            if ($Script:RunPython) {
                $py1tKey = "Python_$($bench.Name)-1t"
                $pyMtKey = "Python_$($bench.Name)-mt"
                if ($history.ContainsKey($py1tKey)) {
                    $history.Remove($py1tKey)
                    $modified = $true
                }
                if ($history.ContainsKey($pyMtKey)) {
                    $history.Remove($pyMtKey)
                    $modified = $true
                }
            }
            # Clear Lua history for this benchmark if running lua
            if ($Script:RunLua) {
                $luaKey = "Lua_$($bench.Name)"
                if ($history.ContainsKey($luaKey)) {
                    $history.Remove($luaKey)
                    $modified = $true
                }
            }
        }

        if ($modified) {
            Save-BenchmarkHistory $history
            Show-Status "Cleared history for selected benchmarks/runtimes" -Type "Info"
        }
    } else {
        # Full mode: clear ALL history
        Clear-BenchmarkHistory
        Show-Status "Cleared all benchmark history" -Type "Info"
    }
    Write-Host ""

    $failed = 0

    # Run SedaiBasic2 benchmarks
    if ($Script:RunSedai) {
        Write-Host ""
        Write-Host "  Starting SedaiBasic2 benchmarks..." -ForegroundColor Cyan
        Write-Host ""

        foreach ($bench in $toRun) {
            if (!(Run-Benchmark -Benchmark $bench -QuickMode $Quick -RunCount $Script:EffectiveRuns)) {
                $failed++
            }
            Write-Host ""
        }
    }

    # Run Python benchmarks if available and enabled
    $pythonAvailable = Test-Path $PythonExe
    if ($Script:RunPython -and $pythonAvailable -and $failed -eq 0) {
        Write-Host "  Starting Python benchmarks..." -ForegroundColor Cyan
        Write-Host ""

        $status = Get-BenchmarkStatus

        foreach ($bench in $toRun) {
            $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
            $pyBench = $PythonBenchmarks | Where-Object { $_.Name -eq $bench.Name }

            if ($pyBench) {
                $result = $status[$bench.Name]
                $sbTimeMs = if ($result) { $result.ExecutionTime * 1000 } else { 0 }
                $sbTimeStr = "{0:F3} ms" -f $sbTimeMs

                # Arrays to collect all run times
                $singleTimes = @()
                $multiTimes = @()

                # --- Single-thread Python test ---
                Show-Status "Running Python $($bench.Name) (N=$currentN) single-thread..." -Type "Run"
                Write-Host "       $($pyBench.Description)" -ForegroundColor DarkGray

                # Run each single-thread test individually to save to history
                for ($i = 1; $i -le $Script:EffectiveRuns; $i++) {
                    if ($Script:EffectiveRuns -gt 1) {
                        Write-Host "`r       Run $i of $Script:EffectiveRuns...   " -ForegroundColor DarkGray -NoNewline
                    }
                    $singleResult = Run-SinglePythonBenchmark -ScriptPath (Join-Path $PyTestsDir $pyBench.PyScript) -N $currentN
                    if ($singleResult.Success) {
                        $singleTimes += $singleResult.ExecutionTime
                    }
                }
                if ($Script:EffectiveRuns -gt 1) {
                    Write-Host "`r                              `r" -NoNewline
                }

                $pythonSingleTime = if ($singleTimes.Count -gt 0) { ($singleTimes | Measure-Object -Average).Average } else { 0.0 }

                if ($pythonSingleTime -gt 0 -and $result) {
                    $pySingleStr = "{0:F3} ms" -f ($pythonSingleTime * 1000)
                    $speedup = $pythonSingleTime / $result.ExecutionTime
                    $speedupStr = "{0:F2}x" -f $speedup
                    Show-Status "SedaiBasic2: $sbTimeStr | Python(1T): $pySingleStr | Speedup: $speedupStr" -Type "Success"
                }
                Write-Host ""

                # --- Multi-thread Python test (if available) ---
                $pythonMultiTime = 0.0
                if ($pyBench.PyScriptMulti) {
                    Show-Status "Running Python $($bench.Name) (N=$currentN) multi-thread..." -Type "Run"
                    Write-Host "       $($pyBench.Description)" -ForegroundColor DarkGray

                    # Run each multi-thread test individually to save to history
                    for ($i = 1; $i -le $Script:EffectiveRuns; $i++) {
                        if ($Script:EffectiveRuns -gt 1) {
                            Write-Host "`r       Run $i of $Script:EffectiveRuns...   " -ForegroundColor DarkGray -NoNewline
                        }
                        $multiResult = Run-SinglePythonBenchmark -ScriptPath (Join-Path $PyTestsDir $pyBench.PyScriptMulti) -N $currentN
                        if ($multiResult.Success) {
                            $multiTimes += $multiResult.ExecutionTime
                        }
                    }
                    if ($Script:EffectiveRuns -gt 1) {
                        Write-Host "`r                              `r" -NoNewline
                    }

                    $pythonMultiTime = if ($multiTimes.Count -gt 0) { ($multiTimes | Measure-Object -Average).Average } else { 0.0 }

                    if ($pythonMultiTime -gt 0 -and $result) {
                        $pyMultiStr = "{0:F3} ms" -f ($pythonMultiTime * 1000)
                        $speedup = $pythonMultiTime / $result.ExecutionTime
                        $speedupStr = "{0:F2}x" -f $speedup
                        Show-Status "SedaiBasic2: $sbTimeStr | Python(MT): $pyMultiStr | Speedup: $speedupStr" -Type "Success"
                    }
                    Write-Host ""
                }

                # Save each Python run to pending
                for ($i = 0; $i -lt $Script:EffectiveRuns; $i++) {
                    $timeSingle = if ($i -lt $singleTimes.Count) { $singleTimes[$i] } else { 0.0 }
                    $timeMulti = if ($i -lt $multiTimes.Count) { $multiTimes[$i] } else { 0.0 }
                    Add-PendingPythonRun -Name $bench.Name -N $currentN -TimeSingle $timeSingle -TimeMulti $timeMulti -QuickMode $Quick -RunCount $Script:EffectiveRuns
                }

                # Update status with Python times (averages for display)
                if ($result) {
                    Set-BenchmarkResult -Name $bench.Name -N $result.N -ExecutionTime $result.ExecutionTime -Instructions $result.Instructions -Mips $result.Mips -Output $result.Output -RunCount $result.RunCount -MinTime $result.MinTime -MaxTime $result.MaxTime -Median $result.Median -StdDev $result.StdDev -P90 $result.P90 -P95 $result.P95 -P99 $result.P99 -PythonSingleTime $pythonSingleTime -PythonMultiTime $pythonMultiTime
                }
            }
        }
    }

    # Run Lua benchmarks if available and enabled
    $luaAvailable = Test-Path $LuaExe
    if ($Script:RunLua -and $luaAvailable -and $failed -eq 0) {
        Write-Host "  Starting Lua benchmarks..." -ForegroundColor Cyan
        Write-Host ""

        $status = Get-BenchmarkStatus

        foreach ($bench in $toRun) {
            $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
            $luaBench = $LuaBenchmarks | Where-Object { $_.Name -eq $bench.Name }

            if ($luaBench) {
                $result = $status[$bench.Name]
                $sbTimeMs = if ($result) { $result.ExecutionTime * 1000 } else { 0 }
                $sbTimeStr = "{0:F3} ms" -f $sbTimeMs

                # Array to collect all run times
                $luaTimes = @()

                # --- Lua test ---
                Show-Status "Running Lua $($bench.Name) (N=$currentN)..." -Type "Run"
                Write-Host "       $($luaBench.Description)" -ForegroundColor DarkGray

                # Run each test individually to save to history
                for ($i = 1; $i -le $Script:EffectiveRuns; $i++) {
                    if ($Script:EffectiveRuns -gt 1) {
                        Write-Host "`r       Run $i of $Script:EffectiveRuns...   " -ForegroundColor DarkGray -NoNewline
                    }
                    $luaResult = Run-SingleLuaBenchmark -ScriptPath (Join-Path $LuaTestsDir $luaBench.LuaScript) -N $currentN
                    if ($luaResult.Success) {
                        $luaTimes += $luaResult.ExecutionTime
                    }
                }
                if ($Script:EffectiveRuns -gt 1) {
                    Write-Host "`r                              `r" -NoNewline
                }

                $luaTime = if ($luaTimes.Count -gt 0) { ($luaTimes | Measure-Object -Average).Average } else { 0.0 }

                if ($luaTime -gt 0 -and $result) {
                    $luaTimeStr = "{0:F3} ms" -f ($luaTime * 1000)
                    $speedup = $luaTime / $result.ExecutionTime
                    $speedupStr = "{0:F2}x" -f $speedup
                    Show-Status "SedaiBasic2: $sbTimeStr | Lua: $luaTimeStr | Speedup: $speedupStr" -Type "Success"
                }
                Write-Host ""

                # Save each Lua run to pending
                for ($i = 0; $i -lt $Script:EffectiveRuns; $i++) {
                    $time = if ($i -lt $luaTimes.Count) { $luaTimes[$i] } else { 0.0 }
                    Add-PendingLuaRun -Name $bench.Name -N $currentN -Time $time -QuickMode $Quick -RunCount $Script:EffectiveRuns
                }

                # Update status with Lua time
                if ($result) {
                    Set-BenchmarkResult -Name $bench.Name -N $result.N -ExecutionTime $result.ExecutionTime -Instructions $result.Instructions -Mips $result.Mips -Output $result.Output -RunCount $result.RunCount -MinTime $result.MinTime -MaxTime $result.MaxTime -Median $result.Median -StdDev $result.StdDev -P90 $result.P90 -P95 $result.P95 -P99 $result.P99 -PythonSingleTime $result.PythonSingleTime -PythonMultiTime $result.PythonMultiTime -LuaTime $luaTime
                }
            }
        }
    }

    # Check if ALL benchmarks are now complete
    $session = Get-PendingSession
    if ($failed -eq 0 -and (Test-SessionComplete -Session $session)) {
        Write-Host ""
        Show-Status "All 3 benchmarks completed successfully!" -Type "Success"
        Show-Status "Committing runs to permanent history..." -Type "Info"
        Commit-PendingToHistory

        # Recalculate statistics using full history for final report
        foreach ($bench in $Benchmarks) {
            $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
            $stats = Get-HistoryStats -Name $bench.Name -N $currentN
            if ($stats) {
                # Get Python stats if available
                $pythonStats = Get-PythonHistoryStats -Name $bench.Name -N $currentN
                $pythonSingleTime = if ($pythonStats) { $pythonStats.PythonSingleTime } else { 0.0 }
                $pythonMultiTime = if ($pythonStats) { $pythonStats.PythonMultiTime } else { 0.0 }

                # Get Lua stats if available
                $luaStats = Get-LuaHistoryStats -Name $bench.Name -N $currentN
                $luaTime = if ($luaStats) { $luaStats.LuaTime } else { 0.0 }

                Set-BenchmarkResult -Name $bench.Name -N $stats.N -ExecutionTime $stats.ExecutionTime -Instructions $stats.Instructions -Mips $stats.Mips -Output "" -RunCount $stats.RunCount -MinTime $stats.MinTime -MaxTime $stats.MaxTime -Median $stats.Median -StdDev $stats.StdDev -P90 $stats.P90 -P95 $stats.P95 -P99 $stats.P99 -PythonSingleTime $pythonSingleTime -PythonMultiTime $pythonMultiTime -LuaTime $luaTime
            }
        }

        $totalRuns = 0
        foreach ($bench in $Benchmarks) {
            $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
            $times = Get-HistoryTimes -Name $bench.Name -N $currentN
            if ($times.Count -gt $totalRuns) { $totalRuns = $times.Count }
        }
        Show-Status "History now contains $totalRuns runs per benchmark" -Type "Success"
    } else {
        if ($failed -gt 0) {
            Show-Status "$failed benchmarks failed" -Type "Warning"
            Show-Status "Pending runs NOT committed to history" -Type "Warning"
        } else {
            Show-Status "Session incomplete - runs pending until all 3 benchmarks complete" -Type "Warning"
        }
    }

    # Generate results
    Write-Host ""
    Generate-Results -SystemInfo $systemInfo -QuickMode $Quick -RunCount $Script:EffectiveRuns

    # Show report saved message and clean up after successful session
    if ($failed -eq 0 -and (Test-SessionComplete -Session $session)) {
        Write-Host ""
        $outputFileName = [System.IO.Path]::GetFileName($ResultsFile)
        Write-Host "  Full report saved to: $outputFileName" -ForegroundColor Cyan
        Write-Host ""

        # Clean up status file after successful session - next run should start fresh
        if (Test-Path $StatusFile) {
            Remove-Item $StatusFile -Force
        }
    }

    return $(if ($failed -gt 0) { 1 } else { 0 })
}

# Entry point
$exitCode = Invoke-Benchmarks
exit $exitCode
