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
    Number of runs per benchmark in this session (default: 1)
    Results are accumulated with previous runs for statistics.

.PARAMETER ClearHistory
    Clear all accumulated benchmark history and start fresh

.PARAMETER Report
    Generate markdown report from existing history without running benchmarks.
    Exits with error if no history data exists.

.PARAMETER Output
    Custom output filename for the results (default: BENCHMARKS.md)

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
#>

param(
    [switch]$Help,
    [switch]$Force,
    [switch]$Quick,
    [switch]$ClearHistory,
    [switch]$Report,
    [string]$Output = "",
    [int]$Runs = 1
)

# ============================================================================
#  CONFIGURATION
# ============================================================================

$Script:ProjectRoot = $PSScriptRoot
$Script:TempDir = Join-Path $ProjectRoot ".benchmark_temp"
$Script:DataDir = Join-Path $ProjectRoot ".benchmark_data"
$Script:StatusFile = Join-Path $TempDir "status.json"
$Script:HistoryFile = Join-Path $DataDir "history.json"       # Permanent - cumulative data
$Script:PendingFile = Join-Path $DataDir "pending.json"       # Permanent - incomplete session
$Script:DefaultResultsFile = "BENCHMARKS.md"
$Script:ResultsFile = Join-Path $ProjectRoot $DefaultResultsFile
$Script:SbExe = Join-Path $ProjectRoot "bin\x86_64-win64\sb.exe"

# Benchmark configurations
# StandardN = values from The Computer Language Benchmarks Game
# NPattern = regex to extract N from source file (for Quick mode)
$Script:Benchmarks = @(
    @{
        Name = "fannkuch-redux"
        Source = "bas\fannkuch-redux.bas"
        NPattern = "N%\s*=\s*(\d+)"
        NReplace = "N% = {0}"
        StandardN = 12
        Description = "Indexed-access to tiny integer-sequence"
    },
    @{
        Name = "n-body"
        Source = "bas\n-body.bas"
        NPattern = "N%\s*=\s*(\d+)"
        NReplace = "N%={0}"
        StandardN = 50000000
        Description = "Double-precision N-body simulation"
    },
    @{
        Name = "spectral-norm"
        Source = "bas\spectral-norm.bas"
        NPattern = "N%\s*=\s*(\d+)"
        NReplace = "N% = {0}"
        StandardN = 5500
        Description = "Eigenvalue using the power method"
    }
)

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
    Write-Host "    -Quick          Use N values from source files (fast test mode)"
    Write-Host "    -Runs <n>       Number of runs per benchmark in this session (default: 1)"
    Write-Host "    -ClearHistory   Clear accumulated benchmark history and start fresh"
    Write-Host "    -Report         Generate report from existing history (no benchmark run)"
    Write-Host "    -Output <file>  Custom output filename (default: BENCHMARKS.md)"
    Write-Host ""
    Write-Host "CUMULATIVE STATISTICS:" -ForegroundColor Yellow
    Write-Host "    Each run is saved to a history file. Statistics (mean, median, stddev,"
    Write-Host "    percentiles) are calculated using ALL accumulated runs over time."
    Write-Host "    Run the benchmark multiple times to build reliable statistics."
    Write-Host "    Runs are only added to history when ALL 3 benchmarks complete successfully."
    Write-Host ""
    Write-Host "SESSION RESUME:" -ForegroundColor Yellow
    Write-Host "    If a session was interrupted, the next run will automatically resume"
    Write-Host "    from where it left off, running only the incomplete benchmarks."
    Write-Host ""
    Write-Host "BENCHMARKS:" -ForegroundColor Yellow
    Write-Host "    fannkuch-redux  Indexed-access to tiny integer-sequence (N=12)"
    Write-Host "    n-body          Double-precision N-body simulation (N=50,000,000)"
    Write-Host "    spectral-norm   Eigenvalue using the power method (N=5,500)"
    Write-Host ""
    Write-Host "    Standard N values from The Computer Language Benchmarks Game."
    Write-Host "    Use -Quick to run with N values from source files instead."
    Write-Host ""
    Write-Host "EXAMPLES:" -ForegroundColor Yellow
    Write-Host "    .\benchmark.ps1                # Run once, accumulate results"
    Write-Host "    .\benchmark.ps1 -Runs 3        # Run 3 times, accumulate results"
    Write-Host "    .\benchmark.ps1 -Quick         # Run with N from source files"
    Write-Host "    .\benchmark.ps1 -Force         # Re-run all, ignore cached session"
    Write-Host "    .\benchmark.ps1 -ClearHistory  # Reset all accumulated data"
    Write-Host "    .\benchmark.ps1 -Report        # Generate report from history only"
    Write-Host "    .\benchmark.ps1 -Output out.md # Use custom output filename"
    Write-Host ""
    Write-Host "OUTPUT:" -ForegroundColor Yellow
    Write-Host "    Results are saved to BENCHMARKS.md (or custom file) and displayed on screen."
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
    Write-Host "    - Ensure your computer is plugged in (not on battery)" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  Estimated total time: 10-30 minutes (depending on CPU)" -ForegroundColor White
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
                $runs += @{
                    Time = $run.Time
                    Instructions = $run.Instructions
                    N = $run.N
                    Timestamp = $run.Timestamp
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

    foreach ($benchName in $session.PendingRuns.Keys) {
        if (-not $history.ContainsKey($benchName)) {
            $history[$benchName] = @()
        }
        foreach ($run in $session.PendingRuns[$benchName]) {
            $history[$benchName] += $run
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
        [double]$P99 = 0.0
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

    Show-Status "Running $($Benchmark.Name) (N=$currentN)..." -Type "Run"
    Write-Host "       $($Benchmark.Description)" -ForegroundColor DarkGray
    if ($historyCount -gt 0) {
        Write-Host "       Historical runs: $historyCount (committed)" -ForegroundColor DarkGray
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
        # Skip pending/history in Quick mode - those are just test runs
        if (-not $QuickMode) {
            Add-PendingRun -Name $Benchmark.Name -N $currentN -Time $result.ExecutionTime -Instructions $result.Instructions -QuickMode $QuickMode -RunCount $RunCount
        }
    }

    # Clear the progress line
    if ($RunCount -gt 1) {
        Write-Host "`r                              `r" -NoNewline
    }

    # Mark this benchmark as complete in pending session (skip in Quick mode)
    if (-not $QuickMode) {
        Set-PendingBenchmarkComplete -Name $Benchmark.Name
    }

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

    # Save result with session statistics (for display)
    Set-BenchmarkResult -Name $Benchmark.Name -N $currentN -ExecutionTime $avgTime -Instructions $lastResult.Instructions -Mips $avgMips -Output $lastResult.Output -RunCount $RunCount -MinTime $minTime -MaxTime $maxTime -Median $medianTime -StdDev $stdDev -P90 $p90 -P95 $p95 -P99 $p99

    $timeStr = "{0:F3} ms" -f ($avgTime * 1000)
    if ($RunCount -gt 1) {
        $timeStr += " (median: {0:F3} ms, stddev: {1:F3} ms)" -f ($medianTime * 1000), ($stdDev * 1000)
    }
    $mipsStr = "{0:F2}" -f $avgMips

    # Show pending status only in Standard mode (Quick mode doesn't save to history)
    $pendingStr = ""
    if (-not $QuickMode) {
        $pendingStr = " [pending commit]"
        if ($historyCount -gt 0) {
            $pendingStr = " [+$RunCount pending, $historyCount committed]"
        }
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
    foreach ($bench in $Benchmarks) {
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

    foreach ($bench in $Benchmarks) {
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

    # Check if any benchmark has multiple runs
    $hasMultipleRuns = $false
    foreach ($bench in $Benchmarks) {
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
        foreach ($bench in $Benchmarks) {
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
        foreach ($bench in $Benchmarks) {
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

    # Display statistical analysis if any benchmark has multiple runs
    if ($hasMultipleRuns) {
        Write-Host ""
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host "  STATISTICAL ANALYSIS (Cumulative Runs)" -ForegroundColor Yellow
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

    # Handle -Output parameter
    if ($Output -ne "") {
        if ([System.IO.Path]::IsPathRooted($Output)) {
            $Script:ResultsFile = $Output
        } else {
            $Script:ResultsFile = Join-Path $ProjectRoot $Output
        }
    }

    # Handle -Report parameter (generate report from existing history only)
    if ($Report) {
        Show-Banner
        Write-Host "  Mode: REPORT ONLY (from existing history)" -ForegroundColor Yellow
        Write-Host ""

        # Create directories if needed
        if (!(Test-Path $TempDir)) {
            New-Item -ItemType Directory -Path $TempDir -Force | Out-Null
        }
        if (!(Test-Path $DataDir)) {
            New-Item -ItemType Directory -Path $DataDir -Force | Out-Null
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

    # Show mode
    if ($Quick) {
        Write-Host "  Mode: QUICK (N values from source files)" -ForegroundColor Yellow
    } else {
        Write-Host "  Mode: STANDARD (Benchmarks Game N values)" -ForegroundColor Green
    }
    if ($Runs -gt 1) {
        Write-Host "  Runs: $Runs per benchmark (cumulative statistics)" -ForegroundColor Cyan
    }
    Write-Host ""

    # Check if sb.exe exists
    if (!(Test-Path $SbExe)) {
        Show-Status "SedaiBasic2 interpreter not found: $SbExe" -Type "Error"
        Show-Status "Please run setup.ps1 first to build the interpreter." -Type "Info"
        return 1
    }

    # Get system info
    Write-Host "  Collecting system information..." -ForegroundColor Gray
    $systemInfo = Get-SystemInfo
    Write-Host "  CPU: $($systemInfo.CPU)" -ForegroundColor Gray
    Write-Host "  Cores/Threads: $($systemInfo.CPUCores)/$($systemInfo.CPUThreads)" -ForegroundColor Gray
    Write-Host "  RAM: $($systemInfo.RAM)" -ForegroundColor Gray
    Write-Host ""

    # Create temp and data directories
    if (!(Test-Path $TempDir)) {
        New-Item -ItemType Directory -Path $TempDir -Force | Out-Null
    }
    if (!(Test-Path $DataDir)) {
        New-Item -ItemType Directory -Path $DataDir -Force | Out-Null
    }

    # Check for pending session (incomplete previous run) - only in Standard mode
    $pendingSession = $null
    $resumingSession = $false

    if (-not $Quick) {
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
    }

    # Clear status and pending if Force flag is set
    if ($Force) {
        if (Test-Path $StatusFile) {
            Remove-Item $StatusFile -Force
        }
        Clear-PendingSession
        Show-Status "Previous session results cleared" -Type "Info"
    }

    # Load existing status
    $status = Get-BenchmarkStatus

    # Determine which benchmarks need to run
    $toRun = @()
    if ($Quick) {
        # Quick mode: always run all benchmarks (no history tracking)
        $toRun = $Benchmarks
    } elseif ($resumingSession) {
        # Resume: run only benchmarks not completed in pending session
        foreach ($bench in $Benchmarks) {
            if (-not $pendingSession.Completed.ContainsKey($bench.Name) -or -not $pendingSession.Completed[$bench.Name]) {
                $toRun += $bench
            }
        }
    } else {
        # Fresh start: check status file
        # Also verify N matches current mode (status might be from Quick mode)
        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            $expectedN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
            if (!$result -or !$result.Completed -or $result.N -ne $expectedN) {
                $toRun += $bench
            }
        }
    }

    if ($toRun.Count -eq 0) {
        if ($resumingSession) {
            # All benchmarks in pending session are complete - commit to history
            $session = Get-PendingSession
            if (Test-SessionComplete -Session $session) {
                Commit-PendingToHistory
                Show-Status "All benchmarks completed! Runs committed to history." -Type "Success"
            }
        } else {
            Show-Status "All benchmarks already completed!" -Type "Success"
            Show-Status "Use -Force to re-run all benchmarks" -Type "Info"
        }
        Generate-Results -SystemInfo $systemInfo -QuickMode $Quick -RunCount $Runs
        return 0
    }

    # Show warning (skip in quick mode)
    if (-not $Quick) {
        Show-Warning
    }

    Write-Host "  Benchmarks to run: $($toRun.Count) of $($Benchmarks.Count)" -ForegroundColor White
    foreach ($bench in $toRun) {
        $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
        Write-Host "    - $($bench.Name) (N=$currentN)" -ForegroundColor Gray
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

    Write-Host ""
    Write-Host "  Starting benchmarks..." -ForegroundColor Cyan
    Write-Host ""

    # Run benchmarks
    $failed = 0
    foreach ($bench in $toRun) {
        if (!(Run-Benchmark -Benchmark $bench -QuickMode $Quick -RunCount $Runs)) {
            $failed++
        }
        Write-Host ""
    }

    # Check if ALL benchmarks are now complete
    if ($Quick) {
        # Quick mode: no history tracking, just show completion message
        if ($failed -eq 0) {
            Write-Host ""
            Show-Status "All benchmarks completed successfully!" -Type "Success"
            Show-Status "Quick mode: results NOT saved to history" -Type "Info"
        } else {
            Show-Status "$failed benchmark(s) failed" -Type "Warning"
        }
    } else {
        # Standard mode: check pending session and commit to history
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
                    Set-BenchmarkResult -Name $bench.Name -N $stats.N -ExecutionTime $stats.ExecutionTime -Instructions $stats.Instructions -Mips $stats.Mips -Output "" -RunCount $stats.RunCount -MinTime $stats.MinTime -MaxTime $stats.MaxTime -Median $stats.Median -StdDev $stats.StdDev -P90 $stats.P90 -P95 $stats.P95 -P99 $stats.P99
                }
            }

            $totalRuns = 0
            $history = Get-BenchmarkHistory
            foreach ($bench in $Benchmarks) {
                $currentN = Get-BenchmarkN -Benchmark $bench -QuickMode $Quick
                $times = Get-HistoryTimes -Name $bench.Name -N $currentN
                if ($times.Count -gt $totalRuns) { $totalRuns = $times.Count }
            }
            Show-Status "History now contains $totalRuns run(s) per benchmark" -Type "Success"
        } else {
            if ($failed -gt 0) {
                Show-Status "$failed benchmark(s) failed" -Type "Warning"
                Show-Status "Pending runs NOT committed to history" -Type "Warning"
            } else {
                Show-Status "Session incomplete - runs pending until all 3 benchmarks complete" -Type "Warning"
            }
        }
    }

    # Generate results
    Write-Host ""
    Generate-Results -SystemInfo $systemInfo -QuickMode $Quick -RunCount $Runs

    if ($Quick) {
        # Quick mode: just show report saved message
        if ($failed -eq 0) {
            Write-Host ""
            $outputFileName = [System.IO.Path]::GetFileName($ResultsFile)
            Write-Host "  Report saved to: $outputFileName" -ForegroundColor Cyan
            Write-Host ""
        }
    } else {
        # Standard mode: clean up status file after successful session
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
    }

    return $(if ($failed -gt 0) { 1 } else { 0 })
}

# Entry point
$exitCode = Invoke-Benchmarks
exit $exitCode
