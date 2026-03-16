$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$asmPath = Join-Path $scriptDir "seu.asm"
$jclPath = Join-Path $scriptDir "autobuild.jcl"

$asmContent = Get-Content -Path $asmPath -Raw

$jclTop = @'
//SEU     JOB (TSO),'COMPILE SEU',CLASS=A,MSGCLASS=A,NOTIFY=HERC01,
//        USER=HERC01,PASSWORD=CUL8TR
//*
//UPLOAD   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=A
//SYSUT2   DD  DSN=HERC01.SOURCE.ASM(SEU),DISP=SHR
//SYSIN    DD  DUMMY
//SYSUT1   DD  DATA,DLM='$$'
'@

$jclMiddle = "`r`n" + $asmContent + "`r`n" + '$$' + "`r`n"

$jclBottom = @'
//*
//* COMPILER JCL FOR IFOX00 ON MVS 3.8J
//*
//ASM      EXEC PGM=IFOX00,PARM='OBJ,NODECK,LIST'
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSPRINT DD  SYSOUT=A
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(200,200)),
//             DISP=(MOD,PASS)
//SYSIN    DD  DSN=HERC01.SOURCE.ASM(SEU),DISP=SHR
//*
//LINK     EXEC PGM=IEWL,PARM='LIST,XREF,LET,MAP'
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,DELETE)
//SYSLMOD  DD  DSN=HERC01.TEST.LOADLIB(SEU),DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSPRINT DD  SYSOUT=A
'@

$combined = $jclTop + $jclMiddle + $jclBottom

# Write out ASCII file
[System.IO.File]::WriteAllText($jclPath, $combined, [System.Text.Encoding]::ASCII)

Write-Host "Sending autobuild.jcl (Upload + Compile + Link) to IP 192.168.12.203..."

$client = New-Object System.Net.Sockets.TcpClient('192.168.12.203', 3505)
$stream = $client.GetStream()
$bytes = [System.IO.File]::ReadAllBytes($jclPath)
$stream.Write($bytes, 0, $bytes.Length)
$stream.Flush()
Start-Sleep -Milliseconds 500
$client.Close()

Write-Host "Build job successfully submitted to MVS spool!"
