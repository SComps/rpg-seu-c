$path = "c:\Users\Scott\.gemini\antigravity\scratch\rpg-seu-c\MVS-SEU\MVS--639091383024782421--374.dst"
$bytes = [System.IO.File]::ReadAllBytes($path)
$enc = [System.Text.Encoding]::GetEncoding(37)
$text = $enc.GetString($bytes)
# Split into pages/lines based on common printer control chars
$pages = $text.Split([char]12)  # form feed
$lines = $text -split "[\r\n\x15\x25]"  # try CR/LF and some EBCDIC newline codes
Write-Host "Decoded pages: $($pages.Length)"
Write-Host "Decoded lines: $($lines.Length)"
Write-Host "First 20 lines (raw):"
$lines | Select-Object -First 20 | ForEach-Object { Write-Host $_ }
Write-Host "Searching for SEU/DBG text in full decoded stream..."
if ($text -match 'SEU|DBG') {
    Write-Host "Found SEU/DBG in decoded text. Showing context around first match:" -ForegroundColor Green
    $idx = $text.IndexOf('SEU')
    if ($idx -lt 0) { $idx = $text.IndexOf('DBG') }
    if ($idx -ge 0) {
        $start = [Math]::Max(0, $idx - 80)
        $end = [Math]::Min($text.Length - 1, $idx + 200)
        Write-Host $text.Substring($start, $end - $start)
    }
} else {
    Write-Host "No SEU/DBG string found in decoded stream." -ForegroundColor Yellow
}
Write-Host "Searching for SEU/DBG lines based on regexp..."
$lines | Select-String -Pattern 'SEU|DBG' | Select-Object -First 60 | ForEach-Object { Write-Host $_.Line }
