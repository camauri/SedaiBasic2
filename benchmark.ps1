<#
.SYNOPSIS
    SedaiBasic2 Benchmark Script

.DESCRIPTION
    Runs standardized benchmarks from The Computer Language Benchmarks Game:
    - fannkuch-redux
    - n-body
    - spectral-norm

    Results are saved to BENCHMARKS.md

    Copyright (c) 2025 Maurizio Cammalleri
    Released under GNU GPL v3

.PARAMETER Force
    Force re-run of all benchmarks, ignoring previous results

.PARAMETER Quick
    Quick test mode: use N values from source files instead of standard benchmark values

.PARAMETER Runs
    Number of runs per benchmark for averaging (default: 1)

.EXAMPLE
    .\benchmark.ps1

.EXAMPLE
    .\benchmark.ps1 -Quick

.EXAMPLE
    .\benchmark.ps1 -Runs 3
#>

param(
    [switch]$Help,
    [switch]$Force,
    [switch]$Quick,
    [int]$Runs = 1
)

# ============================================================================
#  CONFIGURATION
# ============================================================================

$Script:ProjectRoot = $PSScriptRoot
$Script:TempDir = Join-Path $ProjectRoot ".benchmark_temp"
$Script:StatusFile = Join-Path $TempDir "status.json"
$Script:ResultsFile = Join-Path $ProjectRoot "BENCHMARKS.md"
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
    Write-Host "    -Runs <n>       Number of runs per benchmark for averaging (default: 1)"
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
    Write-Host "    .\benchmark.ps1              # Run with standard N values"
    Write-Host "    .\benchmark.ps1 -Quick       # Run with N from source files"
    Write-Host "    .\benchmark.ps1 -Runs 3      # Run each benchmark 3 times"
    Write-Host "    .\benchmark.ps1 -Force       # Re-run all, ignore cached results"
    Write-Host ""
    Write-Host "OUTPUT:" -ForegroundColor Yellow
    Write-Host "    Results are saved to BENCHMARKS.md and displayed on screen."
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

    # Run with --stats to get execution statistics
    $output = & $SbExe $TempFile --stats 2>&1 | Out-String
    $exitCode = $LASTEXITCODE

    $result = @{
        Success = ($exitCode -eq 0)
        Output = $output
        ExitCode = $exitCode
        ExecutionTime = 0.0
        Instructions = 0
        Mips = 0.0
    }

    if ($result.Success) {
        # Parse execution time - format: "Execution time: <value> <unit>"
        # Units: s = seconds, ms = milliseconds, ns = nanoseconds, anything else = microseconds
        if ($output -match "Execution time:\s+([\d.]+)\s*(\S+)") {
            $timeValue = [double]$Matches[1]
            $timeUnit = $Matches[2]

            switch -Regex ($timeUnit) {
                "^s$"   { $result.ExecutionTime = $timeValue }                      # seconds
                "^ms$"  { $result.ExecutionTime = $timeValue / 1000.0 }             # milliseconds
                "^ns$"  { $result.ExecutionTime = $timeValue / 1000000000.0 }       # nanoseconds
                default { $result.ExecutionTime = $timeValue / 1000000.0 }          # microseconds (µs, μs, etc.)
            }
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

    Show-Status "Running $($Benchmark.Name) (N=$currentN)..." -Type "Run"
    Write-Host "       $($Benchmark.Description)" -ForegroundColor DarkGray

    $times = @()
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

        $times += $result.ExecutionTime
        $lastResult = $result
    }

    # Clear the progress line
    if ($RunCount -gt 1) {
        Write-Host "`r                              `r" -NoNewline
    }

    # Calculate statistics
    $avgTime = ($times | Measure-Object -Average).Average
    $minTime = ($times | Measure-Object -Minimum).Minimum
    $maxTime = ($times | Measure-Object -Maximum).Maximum
    $medianTime = Get-Median $times
    $stdDev = Get-StdDev $times
    $p90 = Get-Percentile $times 90
    $p95 = Get-Percentile $times 95
    $p99 = Get-Percentile $times 99

    # Recalculate MIPS based on average time
    $avgMips = if ($avgTime -gt 0) { $lastResult.Instructions / $avgTime / 1000000 } else { 0 }

    # Save result
    Set-BenchmarkResult -Name $Benchmark.Name -N $currentN -ExecutionTime $avgTime -Instructions $lastResult.Instructions -Mips $avgMips -Output $lastResult.Output -RunCount $RunCount -MinTime $minTime -MaxTime $maxTime -Median $medianTime -StdDev $stdDev -P90 $p90 -P95 $p95 -P99 $p99

    $timeStr = "{0:F3}s" -f $avgTime
    if ($RunCount -gt 1) {
        $timeStr += " (median: {0:F3}s, stddev: {1:F4}s)" -f $medianTime, $stdDev
    }
    $mipsStr = "{0:F2}" -f $avgMips
    Show-Status "$($Benchmark.Name) completed in $timeStr ($mipsStr MIPS)" -Type "Success"

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
**Runs per benchmark:** $RunCount

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
            $time = "{0:F3} s" -f $result.ExecutionTime
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

    # Add timing details if multiple runs
    if ($RunCount -gt 1) {
        $md += @"


### Statistical Analysis (Multiple Runs)

| Benchmark | Mean | Median | Std Dev | Min | Max |
|-----------|-----:|-------:|--------:|----:|----:|
"@
        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $mean = "{0:F3} s" -f $result.ExecutionTime
                $median = "{0:F3} s" -f $result.Median
                $stddev = "{0:F4} s" -f $result.StdDev
                $min = "{0:F3} s" -f $result.MinTime
                $max = "{0:F3} s" -f $result.MaxTime
                $md += "`n| $($bench.Name) | $mean | $median | $stddev | $min | $max |"
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
                $p90 = "{0:F3} s" -f $result.P90
                $p95 = "{0:F3} s" -f $result.P95
                $p99 = "{0:F3} s" -f $result.P99
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
    Show-Status "Results saved to BENCHMARKS.md" -Type "Success"

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
    Write-Host ("  {0,-18} {1,12} {2,14} {3,16} {4,12}" -f "Benchmark", "N", "Time (s)", "Instructions", "MIPS") -ForegroundColor Cyan
    Write-Host ("  {0,-18} {1,12} {2,14} {3,16} {4,12}" -f ("-" * 18), ("-" * 12), ("-" * 14), ("-" * 16), ("-" * 12)) -ForegroundColor DarkGray

    foreach ($bench in $Benchmarks) {
        $result = $status[$bench.Name]
        if ($result -and $result.Completed) {
            $time = "{0:F3}" -f $result.ExecutionTime
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

    # Display statistical analysis if multiple runs
    if ($RunCount -gt 1) {
        Write-Host ""
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host "  STATISTICAL ANALYSIS" -ForegroundColor Yellow
        Write-Host "  ============================================================" -ForegroundColor Cyan
        Write-Host ""
        Write-Host ("  {0,-18} {1,10} {2,10} {3,10} {4,10} {5,10}" -f "Benchmark", "Mean", "Median", "Std Dev", "Min", "Max") -ForegroundColor Cyan
        Write-Host ("  {0,-18} {1,10} {2,10} {3,10} {4,10} {5,10}" -f ("-" * 18), ("-" * 10), ("-" * 10), ("-" * 10), ("-" * 10), ("-" * 10)) -ForegroundColor DarkGray

        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $mean = "{0:F3}s" -f $result.ExecutionTime
                $median = "{0:F3}s" -f $result.Median
                $stddev = "{0:F4}s" -f $result.StdDev
                $min = "{0:F3}s" -f $result.MinTime
                $max = "{0:F3}s" -f $result.MaxTime
                Write-Host ("  {0,-18} {1,10} {2,10} {3,10} {4,10} {5,10}" -f $bench.Name, $mean, $median, $stddev, $min, $max) -ForegroundColor White
            }
        }

        Write-Host ""
        Write-Host ("  {0,-18} {1,10} {2,10} {3,10}" -f "Benchmark", "P90", "P95", "P99") -ForegroundColor Cyan
        Write-Host ("  {0,-18} {1,10} {2,10} {3,10}" -f ("-" * 18), ("-" * 10), ("-" * 10), ("-" * 10)) -ForegroundColor DarkGray

        foreach ($bench in $Benchmarks) {
            $result = $status[$bench.Name]
            if ($result -and $result.Completed -and $result.RunCount -gt 1) {
                $p90 = "{0:F3}s" -f $result.P90
                $p95 = "{0:F3}s" -f $result.P95
                $p99 = "{0:F3}s" -f $result.P99
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
        Remove-Item -Path $TempDir -Recurse -Force -ErrorAction SilentlyContinue
        Show-Status "Temporary files cleaned up" -Type "Success"
    }
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

    Show-Banner

    # Show mode
    if ($Quick) {
        Write-Host "  Mode: QUICK (N values from source files)" -ForegroundColor Yellow
    } else {
        Write-Host "  Mode: STANDARD (Benchmarks Game N values)" -ForegroundColor Green
    }
    if ($Runs -gt 1) {
        Write-Host "  Runs: $Runs per benchmark (results will be averaged)" -ForegroundColor Cyan
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

    # Create temp directory
    if (!(Test-Path $TempDir)) {
        New-Item -ItemType Directory -Path $TempDir -Force | Out-Null
    }

    # Clear status if Force flag is set
    if ($Force -and (Test-Path $StatusFile)) {
        Remove-Item $StatusFile -Force
        Show-Status "Previous results cleared" -Type "Info"
    }

    # Load existing status
    $status = Get-BenchmarkStatus

    # Check which benchmarks need to run
    $toRun = @()
    foreach ($bench in $Benchmarks) {
        $result = $status[$bench.Name]
        if (!$result -or !$result.Completed) {
            $toRun += $bench
        }
    }

    if ($toRun.Count -eq 0) {
        Show-Status "All benchmarks already completed!" -Type "Success"
        Show-Status "Use -Force to re-run all benchmarks" -Type "Info"
        Generate-Results -SystemInfo $systemInfo -QuickMode $Quick -RunCount $Runs
        Cleanup-TempDir
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

    # Generate results
    Write-Host ""
    Generate-Results -SystemInfo $systemInfo -QuickMode $Quick -RunCount $Runs

    # Cleanup
    Cleanup-TempDir

    if ($failed -gt 0) {
        Show-Status "$failed benchmark(s) failed" -Type "Warning"
        return 1
    }

    Write-Host ""
    Write-Host "  All benchmarks completed successfully!" -ForegroundColor Green
    Write-Host "  Full report saved to: BENCHMARKS.md" -ForegroundColor Cyan
    Write-Host ""

    return 0
}

# Entry point
$exitCode = Invoke-Benchmarks
exit $exitCode
