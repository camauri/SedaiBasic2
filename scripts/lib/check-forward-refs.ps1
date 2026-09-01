<#
.SYNOPSIS
    Refuse a call to one of our own functions made BEFORE the line that defines it.

.DESCRIPTION
    ⛔ POWERSHELL DOES NOT HOIST FUNCTIONS. A `function` statement takes effect when execution
    REACHES it, not when the file is parsed. So a top-level call placed above the definition parses
    cleanly, passes every syntax check, and then dies at run time with:

        Find-CCompiler : The term 'Find-CCompiler' is not recognized as the name of a cmdlet,
        function, script file, or operable program.

    That is the bug reported from a clean Windows checkout on 2 Sep 2026: build.ps1 called
    Find-CCompiler and Build-HotDisp at line 1019/1021 while both were defined at 1093/1114, so
    EVERY Windows build failed and setup.ps1 ended in SETUP FAILED. It had never been seen here
    because the day-to-day build is build.sh on Linux - a build branch without a net drifts.

    ⭐ THE TRANSITIVE CASE COUNTS TOO, and it is the one a reader misses. A function defined early
    may call one defined late; that is fine only for as long as nothing INVOKES it at top level in
    between. So the check walks the call graph from every top-level invocation and demands that
    everything reachable from it is already defined at that line.

    ⚠️ A call from inside a function body to a function defined later is NOT reported on its own -
    it is legal, and common, whenever the enclosing function is invoked after both definitions.
    Reporting it would flag three innocent sites in benchmarks/benchmark.ps1.

.EXAMPLE
    pwsh -NoProfile -File scripts/lib/check-forward-refs.ps1
    # exit 0: nothing to report.  exit 1: one line per offending call site.
#>

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$bad  = New-Object System.Collections.Generic.List[string]
$seen = 0

# ⛔ The net finds its own files. An earlier throwaway version took them as a parameter and the
# caller's shell expansion handed it ONE of twelve - it reported three harmless sites in the first
# file and stayed silent on the defect it was written for. A net that can be under-fed is blind.
foreach ($file in Get-ChildItem -Path $root -Filter *.ps1 -Recurse -File |
                  Where-Object { $_.FullName -notmatch '[\\/](_archive|distrib|distrib\.bak|distrib\.old|job)[\\/]' }) {

    $errs = $null
    $ast = [System.Management.Automation.Language.Parser]::ParseFile($file.FullName, [ref]$null, [ref]$errs)
    if ($errs) { $bad.Add("$($file.Name): does not parse ($($errs.Count) errors)"); continue }
    $seen++

    $funcs = $ast.FindAll({ param($n) $n -is [System.Management.Automation.Language.FunctionDefinitionAst] }, $true)
    if ($funcs.Count -eq 0) { continue }

    $defLine = @{}
    foreach ($fn in $funcs) { if (-not $defLine.ContainsKey($fn.Name)) { $defLine[$fn.Name] = $fn.Extent.StartLineNumber } }

    # The innermost function body enclosing a line, or $null when the line is top-level code.
    function Get-Enclosing([int]$line, $funcList) {
        $best = $null
        foreach ($fn in $funcList) {
            if ($line -ge $fn.Extent.StartLineNumber -and $line -le $fn.Extent.EndLineNumber) {
                if (-not $best -or $fn.Extent.StartLineNumber -gt $best.Extent.StartLineNumber) { $best = $fn }
            }
        }
        return $best
    }

    # Split every call site into "top level" and "inside function F".
    $topCalls = New-Object System.Collections.Generic.List[object]
    $callees  = @{}                       # function name -> names it calls
    foreach ($c in $ast.FindAll({ param($n) $n -is [System.Management.Automation.Language.CommandAst] }, $true)) {
        $name = $c.GetCommandName()
        if (-not $name -or -not $defLine.ContainsKey($name)) { continue }
        $line = $c.Extent.StartLineNumber
        $encl = Get-Enclosing $line $funcs
        if ($encl) {
            if (-not $callees.ContainsKey($encl.Name)) { $callees[$encl.Name] = New-Object System.Collections.Generic.List[string] }
            $callees[$encl.Name].Add($name)
        } else {
            $topCalls.Add([PSCustomObject]@{ Name = $name; Line = $line })
        }
    }

    # From each top-level invocation, everything reachable must already exist at that line.
    foreach ($call in $topCalls) {
        $stack   = New-Object System.Collections.Generic.Stack[string]
        $visited = @{}
        $stack.Push($call.Name)
        while ($stack.Count -gt 0) {
            $f = $stack.Pop()
            if ($visited.ContainsKey($f)) { continue }
            $visited[$f] = $true
            if ($defLine[$f] -gt $call.Line) {
                $how = if ($f -eq $call.Name) { "calls $f" } else { "calls $($call.Name), which reaches $f" }
                $bad.Add("$($file.Name):$($call.Line): $how - defined at line $($defLine[$f]), BELOW this call")
            }
            if ($callees.ContainsKey($f)) { foreach ($g in $callees[$f]) { if (-not $visited.ContainsKey($g)) { $stack.Push($g) } } }
        }
    }
}

if ($bad.Count -eq 0) {
    Write-Host "FWDREF: OK - $seen PowerShell scripts, no function called above its definition"
    exit 0
}
Write-Host "FWDREF: $($bad.Count) forward reference(s) - these die at run time, not at parse time:"
$bad | Sort-Object -Unique | ForEach-Object { Write-Host "  $_" }
exit 1
