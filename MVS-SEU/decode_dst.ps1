$path = "$PSScriptRoot\MVS--639091383024782421--374.dst"
$bytes = [System.IO.File]::ReadAllBytes($path)
$enc = [System.Text.Encoding]::GetEncoding(37)  # EBCDIC US/Canada
$text = $enc.GetString($bytes)
# Split at form feed (FF, 0x0C) to get first page, then split lines by CR
$lines = $text.Split([char]12)[0].Split([char]13)
$lines | Select-Object -First 60 | ForEach-Object { $_ }
