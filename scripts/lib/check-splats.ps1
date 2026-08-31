<#
.SYNOPSIS
    Refuse an ARRAY splat into one of our own PowerShell scripts.

.DESCRIPTION
    ⛔ SPLATTING AN ARRAY PASSES ITS ELEMENTS AS POSITIONAL ARGUMENTS. A hashtable splat binds them by
    NAME. The two spellings look alike and behave differently, and the difference is invisible until a
    script has a positional parameter that REFUSES the value:

        $buildArgs = @('-Clean', '-NoBanner')      # WRONG for a .ps1 with named parameters
        & $buildScript @buildArgs
        #  -> "-Clean" is bound to $Target, whose ValidateSet does not contain it, and setup dies.

    That is the bug reported from a clean Windows checkout in setup.ps1, at its last step [7/7]: five
    installer calls in the same file had always splatted a hashtable and the sixth had not.

    ⚠️ AN ARRAY SPLAT INTO A NATIVE PROGRAM IS FINE AND STAYS FINE - gcc takes positional arguments,
    and build.ps1 splats one into it on purpose. Only a call to one of OUR OWN .ps1 files is checked,
    because only there do named parameters exist to be missed.

.EXAMPLE
    pwsh -NoProfile -File scripts/lib/check-splats.ps1
    # exit 0: nothing to report.  exit 1: one line per offending call site.
#>

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent (Split-Path -Parent $PSScriptRoot)
$bad  = New-Object System.Collections.Generic.List[string]

foreach ($file in Get-ChildItem -Path $root -Filter *.ps1 -Recurse -File |
                  Where-Object { $_.FullName -notmatch '[\\/](_archive|distrib|distrib\.bak|distrib\.old|job)[\\/]' }) {

    $errs = $null
    $ast = [System.Management.Automation.Language.Parser]::ParseFile($file.FullName, [ref]$null, [ref]$errs)
    if ($errs) { $bad.Add("$($file.Name): does not parse ($($errs.Count) errors)"); continue }

    # Every variable ever assigned an ARRAY literal, by name.
    $arrayVars = @{}
    foreach ($a in $ast.FindAll({ param($n) $n -is [System.Management.Automation.Language.AssignmentStatementAst] }, $true)) {
        if ($a.Left -is [System.Management.Automation.Language.VariableExpressionAst] -and
            $a.Right.Extent.Text -match '^\s*@\(') {
            $arrayVars[$a.Left.VariablePath.UserPath] = $true
        }
    }
    if ($arrayVars.Count -eq 0) { continue }

    # ...and every call that SPLATS one of them into a .ps1 rather than into a native program.
    foreach ($c in $ast.FindAll({ param($n) $n -is [System.Management.Automation.Language.CommandAst] }, $true)) {
        $target = $c.CommandElements[0].Extent.Text
        $callsOwnScript = ($target -match '\.ps1') -or ($target -match 'Script\s*$') -or ($target -match 'Script["'']?\s*$')
        if (-not $callsOwnScript) { continue }
        foreach ($e in $c.CommandElements) {
            if ($e -is [System.Management.Automation.Language.VariableExpressionAst] -and $e.Splatted -and
                $arrayVars.ContainsKey($e.VariablePath.UserPath)) {
                $bad.Add("$($file.Name):$($e.Extent.StartLineNumber): array splat '@$($e.VariablePath.UserPath)' into $target - use a hashtable")
            }
        }
    }
}

if ($bad.Count -gt 0) {
    Write-Host "check-splats: $($bad.Count) array splat(s) into one of our own scripts" -ForegroundColor Red
    $bad | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
    exit 1
}
Write-Host "check-splats: OK - no array splat into a .ps1" -ForegroundColor Green
exit 0
