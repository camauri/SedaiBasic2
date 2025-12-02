# Download utilities for SedaiBasic2 scripts
# Common functions for downloading files with progress

function Get-FileWithProgress {
    <#
    .SYNOPSIS
        Downloads a file with progress bar display
    .PARAMETER Url
        The URL to download from
    .PARAMETER OutFile
        The destination file path
    .PARAMETER Quiet
        Suppress progress output
    .OUTPUTS
        Hashtable with Status (0=success, 1=error) and Message
    #>
    param(
        [Parameter(Mandatory=$true)]
        [string]$Url,

        [Parameter(Mandatory=$true)]
        [string]$OutFile,

        [switch]$Quiet
    )

    $result = @{
        Status = 0
        Message = ""
        BytesDownloaded = 0
    }

    try {
        # Create directory if needed
        $outDir = Split-Path -Parent $OutFile
        if ($outDir -and !(Test-Path $outDir)) {
            New-Item -ItemType Directory -Path $outDir -Force | Out-Null
        }

        if (!$Quiet) {
            Write-Host "Starting download..." -ForegroundColor Cyan
        }

        # Configure progress preference
        $oldProgressPreference = $ProgressPreference
        if ($Quiet) {
            $ProgressPreference = 'SilentlyContinue'
        } else {
            $ProgressPreference = 'Continue'
        }

        # Use Invoke-WebRequest with built-in progress
        # For large files, use streaming to avoid memory issues
        $webRequest = [System.Net.HttpWebRequest]::Create($Url)
        $webRequest.Method = "GET"
        $webRequest.AllowAutoRedirect = $true
        $webRequest.UserAgent = "SedaiBasic2-Installer/1.0"

        $response = $webRequest.GetResponse()
        $totalBytes = $response.ContentLength
        $responseStream = $response.GetResponseStream()

        $fileStream = [System.IO.File]::Create($OutFile)
        $buffer = New-Object byte[] 65536  # 64KB buffer
        $bytesRead = 0
        $totalBytesRead = 0
        $lastPercentReported = -1
        $startTime = Get-Date

        if (!$Quiet -and $totalBytes -gt 0) {
            $totalMB = [math]::Round($totalBytes / 1MB, 2)
            Write-Host "File size: $totalMB MB" -ForegroundColor Gray
        }

        while (($bytesRead = $responseStream.Read($buffer, 0, $buffer.Length)) -gt 0) {
            $fileStream.Write($buffer, 0, $bytesRead)
            $totalBytesRead += $bytesRead

            if (!$Quiet -and $totalBytes -gt 0) {
                $percent = [math]::Floor(($totalBytesRead / $totalBytes) * 100)
                if ($percent -ne $lastPercentReported) {
                    $downloadedMB = [math]::Round($totalBytesRead / 1MB, 2)
                    $totalMB = [math]::Round($totalBytes / 1MB, 2)
                    $elapsed = (Get-Date) - $startTime
                    if ($elapsed.TotalSeconds -gt 0) {
                        $speedMBps = [math]::Round(($totalBytesRead / 1MB) / $elapsed.TotalSeconds, 2)
                        Write-Host "`r  Progress: $percent% ($downloadedMB / $totalMB MB) - $speedMBps MB/s   " -NoNewline -ForegroundColor Yellow
                    } else {
                        Write-Host "`r  Progress: $percent% ($downloadedMB / $totalMB MB)   " -NoNewline -ForegroundColor Yellow
                    }
                    $lastPercentReported = $percent
                }
            }
        }

        $fileStream.Close()
        $responseStream.Close()
        $response.Close()

        $ProgressPreference = $oldProgressPreference

        if (!$Quiet) {
            Write-Host ""  # New line after progress
        }

        # Verify file exists and has content
        if (!(Test-Path $OutFile)) {
            throw "Download completed but file not found"
        }

        $fileInfo = Get-Item $OutFile
        if ($fileInfo.Length -eq 0) {
            throw "Downloaded file is empty"
        }

        $result.BytesDownloaded = $fileInfo.Length
        $result.Message = "Download completed successfully"

    } catch {
        $result.Status = 1
        $result.Message = "Download failed: $($_.Exception.Message)"

        # Cleanup partial file
        if (Test-Path $OutFile) {
            Remove-Item $OutFile -Force -ErrorAction SilentlyContinue
        }
    }

    return $result
}

function Test-FileHash {
    <#
    .SYNOPSIS
        Verifies file SHA256 hash
    .PARAMETER FilePath
        Path to the file
    .PARAMETER ExpectedHash
        Expected SHA256 hash
    .OUTPUTS
        Hashtable with Status (0=match, 3=mismatch) and Message
    #>
    param(
        [Parameter(Mandatory=$true)]
        [string]$FilePath,

        [Parameter(Mandatory=$true)]
        [string]$ExpectedHash
    )

    $result = @{
        Status = 0
        Message = ""
        ActualHash = ""
    }

    try {
        if (!(Test-Path $FilePath)) {
            $result.Status = 3
            $result.Message = "File not found: $FilePath"
            return $result
        }

        $actualHash = (Get-FileHash -Path $FilePath -Algorithm SHA256).Hash.ToLower()
        $result.ActualHash = $actualHash

        if ($actualHash -eq $ExpectedHash.ToLower()) {
            $result.Message = "Hash verification passed"
        } else {
            $result.Status = 3
            $result.Message = "Hash mismatch. Expected: $ExpectedHash, Got: $actualHash"
        }

    } catch {
        $result.Status = 3
        $result.Message = "Hash verification failed: $($_.Exception.Message)"
    }

    return $result
}

function Expand-ArchiveWithProgress {
    <#
    .SYNOPSIS
        Extracts a zip archive with progress indication
    .PARAMETER Path
        Path to the zip file
    .PARAMETER DestinationPath
        Extraction destination
    .PARAMETER Quiet
        Suppress progress output
    .OUTPUTS
        Hashtable with Status (0=success, 2=error) and Message
    #>
    param(
        [Parameter(Mandatory=$true)]
        [string]$Path,

        [Parameter(Mandatory=$true)]
        [string]$DestinationPath,

        [switch]$Quiet
    )

    $result = @{
        Status = 0
        Message = ""
    }

    try {
        if (!(Test-Path $Path)) {
            throw "Archive not found: $Path"
        }

        # Create destination if needed
        if (!(Test-Path $DestinationPath)) {
            New-Item -ItemType Directory -Path $DestinationPath -Force | Out-Null
        }

        if (!$Quiet) {
            Write-Host "Extracting archive..." -ForegroundColor Cyan
        }

        # Use .NET for extraction with progress
        Add-Type -AssemblyName System.IO.Compression.FileSystem

        $zip = [System.IO.Compression.ZipFile]::OpenRead($Path)
        $totalEntries = $zip.Entries.Count
        $currentEntry = 0
        $lastPercent = -1

        foreach ($entry in $zip.Entries) {
            $currentEntry++
            $percent = [math]::Floor(($currentEntry / $totalEntries) * 100)

            if (!$Quiet -and $percent -ne $lastPercent -and ($percent % 10 -eq 0 -or $percent -eq 100)) {
                Write-Host "`r  Extracting: $percent% ($currentEntry / $totalEntries files)   " -NoNewline -ForegroundColor Yellow
                $lastPercent = $percent
            }

            $entryPath = Join-Path $DestinationPath $entry.FullName
            $entryDir = Split-Path -Parent $entryPath

            if ($entry.FullName.EndsWith('/')) {
                # Directory entry
                if (!(Test-Path $entryPath)) {
                    New-Item -ItemType Directory -Path $entryPath -Force | Out-Null
                }
            } else {
                # File entry
                if ($entryDir -and !(Test-Path $entryDir)) {
                    New-Item -ItemType Directory -Path $entryDir -Force | Out-Null
                }
                [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, $entryPath, $true)
            }
        }

        $zip.Dispose()

        if (!$Quiet) {
            Write-Host ""  # New line after progress
        }

        $result.Message = "Extraction completed successfully ($totalEntries files)"

    } catch {
        $result.Status = 2
        $result.Message = "Extraction failed: $($_.Exception.Message)"
    }

    return $result
}

function Test-DiskSpace {
    <#
    .SYNOPSIS
        Checks if there's enough disk space
    .PARAMETER Path
        Path to check (uses its drive)
    .PARAMETER RequiredBytes
        Required space in bytes
    .OUTPUTS
        Hashtable with Status (0=ok, 4=insufficient) and Message
    #>
    param(
        [Parameter(Mandatory=$true)]
        [string]$Path,

        [Parameter(Mandatory=$true)]
        [long]$RequiredBytes
    )

    $result = @{
        Status = 0
        Message = ""
        AvailableBytes = 0
    }

    try {
        $drive = (Get-Item $Path -ErrorAction SilentlyContinue).PSDrive
        if (!$drive) {
            # Path doesn't exist yet, get drive from root
            $driveLetter = (Split-Path -Qualifier $Path)
            $drive = Get-PSDrive -Name $driveLetter.TrimEnd(':')
        }

        $availableBytes = $drive.Free
        $result.AvailableBytes = $availableBytes

        if ($availableBytes -lt $RequiredBytes) {
            $requiredMB = [math]::Round($RequiredBytes / 1MB, 2)
            $availableMB = [math]::Round($availableBytes / 1MB, 2)
            $result.Status = 4
            $result.Message = "Insufficient disk space. Required: $requiredMB MB, Available: $availableMB MB"
        } else {
            $result.Message = "Disk space check passed"
        }

    } catch {
        # If we can't check, assume it's ok
        $result.Message = "Could not verify disk space (assuming sufficient)"
    }

    return $result
}

function Test-InternetConnection {
    <#
    .SYNOPSIS
        Tests internet connectivity
    .PARAMETER TestUrl
        URL to test (default: github.com)
    .OUTPUTS
        Hashtable with Status (0=connected, 1=error) and Message
    #>
    param(
        [string]$TestUrl = "https://github.com"
    )

    $result = @{
        Status = 0
        Message = ""
    }

    try {
        $request = [System.Net.WebRequest]::Create($TestUrl)
        $request.Timeout = 10000
        $request.Method = "HEAD"
        $response = $request.GetResponse()
        $response.Close()
        $result.Message = "Internet connection available"
    } catch {
        $result.Status = 1
        $result.Message = "No internet connection: $($_.Exception.Message)"
    }

    return $result
}
