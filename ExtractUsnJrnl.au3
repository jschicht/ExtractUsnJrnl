#Region ;**** Directives created by AutoIt3Wrapper_GUI ****
#AutoIt3Wrapper_Icon=C:\Program Files (x86)\AutoIt3\Icons\au3.ico
#AutoIt3Wrapper_Outfile=ExtractUsnJrnl.exe
#AutoIt3Wrapper_Outfile_x64=ExtractUsnJrnl64.exe
#AutoIt3Wrapper_UseUpx=y
#AutoIt3Wrapper_Compile_Both=y
#AutoIt3Wrapper_UseX64=y
#AutoIt3Wrapper_Change2CUI=y
#AutoIt3Wrapper_Res_Comment=Quickly extract $UsnJrnl from an NTFS volume
#AutoIt3Wrapper_Res_Description=Quickly extract $UsnJrnl from an NTFS volume
#AutoIt3Wrapper_Res_Fileversion=1.0.0.5
#AutoIt3Wrapper_Res_requestedExecutionLevel=asInvoker
#AutoIt3Wrapper_AU3Check_Parameters=-w 3 -w 5
#AutoIt3Wrapper_Run_Au3Stripper=y
#Au3Stripper_Parameters=/sf /sv /rm /pe
#EndRegion ;**** Directives created by AutoIt3Wrapper_GUI ****

#Include <WinAPIEx.au3>
#include <Array.au3>
#Include <String.au3>
#Include <FileConstants.au3>
#Include <APIConstants.au3>

Global Const $RecordSignature = '46494C45' ; FILE signature
Global $IsPhysicalDrive=False,$IsImage=False
Global $GlobUsnJrnlFileSize, $GlobUsnJrnlSparseBytes, $LogicalClusterNumberforthefileMFT, $NonResidentFlag, $InitState = False, $IsCompressed, $Data_Clusters, $ClustersPerFileRecordSegment,$OutputName="$UsnJrnl_$J.bin"
Global $RUN_VCN[1],$RUN_Clusters[1],$MFT_RUN_Clusters[1],$MFT_RUN_VCN[1],$DataQ[1],$sBuffer,$AttrQ[1],$NameQ[5],$AttributesArr[18][4],$Entries,$TargetDrive,$OutPutPath=@ScriptDir,$TargetImageFile,$ImageOffset=0
Global $SectorsPerCluster,$BytesPerSector,$DATA_Name,$_COMMON_KERNEL32DLL=DllOpen("kernel32.dll"),$INDX_Record_Size=4096,$HEADER_MFTREcordNumber,$FN_ParentReferenceNo,$RawTestOffsetArray
Global $IndxEntryNumberArr[1],$IndxMFTReferenceArr[1],$IndxMFTRefSeqNoArr[1],$IndxMFTReferenceOfParentArr[1],$IndxMFTParentRefSeqNoArr[1],$IndxCTimeArr[1],$IndxATimeArr[1],$IndxMTimeArr[1],$IndxRTimeArr[1],$IndxFileNameArr[1]
Global $DATA_RealSize,$FN_FileName,$DataRun,$MFT_Record_Size,$MFT_Offset,$SkipFixups=0,$IsRawShadowCopy=0,$DoIndxOffsetArray=0,$IsFirstRun=1,$IsCurrentIndxOfParent=0,$ResidentIndx
Global $InfoArrShadowMainTarget[3],$InfoArrShadowParent[3],$IndxFileNameFromParentArr[1],$IndxMFTReferenceFromParentArr[1],$IndxMFTReferenceOfParentFromParentArr[1],$IndxCTimeFromParentArr[1],$IndxATimeFromParentArr[1],$IndxMTimeFromParentArr[1],$IndxRTimeFromParentArr[1]
Global Const $STANDARD_INFORMATION = '10000000'
Global Const $ATTRIBUTE_LIST = '20000000'
Global Const $FILE_NAME = '30000000'
Global Const $OBJECT_ID = '40000000'
Global Const $SECURITY_DESCRIPTOR = '50000000'
Global Const $VOLUME_NAME = '60000000'
Global Const $VOLUME_INFORMATION = '70000000'
Global Const $DATA = '80000000'
Global Const $INDEX_ROOT = '90000000'
Global Const $INDEX_ALLOCATION = 'A0000000'
Global Const $BITMAP = 'B0000000'
Global Const $REPARSE_POINT = 'C0000000'
Global Const $EA_INFORMATION = 'D0000000'
Global Const $EA = 'E0000000'
Global Const $PROPERTY_SET = 'F0000000'
Global Const $LOGGED_UTILITY_STREAM = '00010000'
Global Const $ATTRIBUTE_END_MARKER = 'FFFFFFFF'
Global $DateTimeFormat = 6 ; YYYY-MM-DD HH:MM:SS:MSMSMS:NSNSNSNS = 2007-08-18 08:15:37:733:1234
Global $tDelta = _WinTime_GetUTCToLocalFileTimeDelta(), $TimestampPrecision
Global $VolumesArray[1][3]
$VolumesArray[0][0] = "Type"
$VolumesArray[0][1] = "ByteOffset"
$VolumesArray[0][2] = "Sectors"
$DoRead=1

ConsoleWrite("ExtractUsnJrnl v1.0.0.5" & @CRLF & @CRLF)

_GetInputParams()
;_ArrayDisplay($VolumesArray,"$VolumesArray")

$TargetFileName = "x:\$Extend\$UsnJrnl"
$IndexNumber=""

$begin = TimerInit()

_ReadBootSector($TargetDrive)
If @error Then
	ConsoleWrite("Error: Filesystem not NTFS" & @CRLF)
	Exit
EndIf

$BytesPerCluster = $SectorsPerCluster*$BytesPerSector
$MFTEntry = _FindMFT($TargetDrive,0)

$MFT = _DecodeMFTRecord0($MFTEntry, 0)        ;produces DataQ for $MFT, record 0
If $MFT = "" Then
	ConsoleWrite("Error: Parsing the MFT record 0" & @CRLF)
	Exit
EndIf
_GetRunsFromAttributeListMFT0()

$MFTSize = $DATA_RealSize
$MFT_RUN_VCN = $RUN_VCN
$MFT_RUN_Clusters = $RUN_Clusters

If Not _Prep($TargetDrive,$IndexNumber,$TargetFileName) Then
	ConsoleWrite("Error initializing structs and arrays" & @crlf)
	Exit
EndIf

$RetRec = _FindFileMFTRecord($TargetDrive,$InfoArrShadowMainTarget[0])
$NewRecord = $RetRec[1]
If _DecodeMFTRecord($TargetDrive,$NewRecord,1) < 1 Then
	ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
	Exit
EndIf

$RetRec = _FindFileMFTRecord($TargetDrive,$InfoArrShadowMainTarget[0])
$NewRecord = $RetRec[1]
If _DecodeMFTRecord($TargetDrive,$NewRecord,3) < 1 Then
	ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
	Exit
EndIf
;_ArrayDisplay($RawTestOffsetArray,"$RawTestOffsetArray")

If Ubound($RawTestOffsetArray) > 1 Then
	Dim $nBytes

	$OutUsnJrnlFile = $OutPutPath & "\" & $OutputName
	$hVol = _WinAPI_CreateFile($TargetDrive,2,2,7)
	If $hVol = 0 Then
		ConsoleWrite("Error: Creating handle on " & $TargetDrive & @CRLF)
		Exit
	EndIf
	If FileExists($OutUsnJrnlFile) Then
		$CurrentTimestamp = @YEAR & @MON & @MDAY & @HOUR & @MIN & @SEC
		FileMove($OutUsnJrnlFile,$OutUsnJrnlFile&".renamed_"&$CurrentTimestamp)
		ConsoleWrite("Previous output file renamed to prevent overwrite: " & $OutUsnJrnlFile&".renamed_"&$CurrentTimestamp & @CRLF)
	EndIf
	$hOutFile = _WinAPI_CreateFile("\\.\" & $OutUsnJrnlFile, 1, 6, 6)
	If $hOutFile = 0 Then
		ConsoleWrite("Error in function CreateFile on " & $OutUsnJrnlFile & " : " & _WinAPI_GetLastErrorMessage() & @CRLF)
		Exit
	EndIf
	For $i = 1 To UBound($RawTestOffsetArray)-1
		If $RawTestOffsetArray[$i][0] = 0 Then ContinueLoop
		ConsoleWrite(@CRLF & "Trying volume offset 0x" & Hex(Int($RawTestOffsetArray[$i][0])) & @CRLF)
		_WinAPI_SetFilePointerEx($hVol, $RawTestOffsetArray[$i][0], $FILE_BEGIN)
		$End=0
		If Mod($RawTestOffsetArray[$i][2],512) Then
;			ConsoleWrite("Corrected chunk size" & @crlf)
			Do
				$End += 1
			until  Mod($RawTestOffsetArray[$i][2]+$End,512)=1
			$End -= 1
		EndIf
		$tBuffer = DllStructCreate("byte[" & $RawTestOffsetArray[$i][2]+$End & "]")
		If Not _WinAPI_ReadFile($hVol, DllStructGetPtr($tBuffer), $RawTestOffsetArray[$i][2]+$End, $nBytes) Then
			ConsoleWrite("Error in function ReadFile: " & _WinAPI_GetLastErrorMessage() & @CRLF)
		EndIf
		If Not _WinAPI_WriteFile($hOutFile, DllStructGetPtr($tBuffer), $RawTestOffsetArray[$i][2], $nBytes) Then
			ConsoleWrite("Error in function WriteFile: " & _WinAPI_GetLastErrorMessage() & @CRLF)
		EndIf
	Next
	_WinAPI_CloseHandle($hVol)
	_WinAPI_CloseHandle($hOutFile)
EndIf

ConsoleWrite("Extract took " & _WinAPI_StrFromTimeInterval(TimerDiff($begin)) & @CRLF)


Func _DecodeDataQEntry($attr)		;processes data attribute
   $NonResidentFlag = StringMid($attr,17,2)
   $NameLength = Dec(StringMid($attr,19,2))
   $NameOffset = Dec(_SwapEndian(StringMid($attr,21,4)))
   If $NameLength > 0 Then		;must be ADS
	  $ADS_Name = _UnicodeHexToStr(StringMid($attr,$NameOffset*2 + 1,$NameLength*4))
	  $ADS_Name = $FN_FileName & "[ADS_" & $ADS_Name & "]"
   Else
	  $ADS_Name = $FN_FileName		;need to preserve $FN_FileName
   EndIf
   $Flags = StringMid($attr,25,4)
   If BitAND($Flags,"0100") Then $IsCompressed = 1
   ;If BitAND($Flags,"0080") Then $IsSparse = 1
   If $NonResidentFlag = '01' Then
	  $DATA_Clusters = Dec(_SwapEndian(StringMid($attr,49,16)),2) - Dec(_SwapEndian(StringMid($attr,33,16)),2) + 1
	  $DATA_RealSize = Dec(_SwapEndian(StringMid($attr,97,16)),2)
	  ;$DATA_InitSize = Dec(_SwapEndian(StringMid($attr,113,16)),2)
	  $Offset = Dec(_SwapEndian(StringMid($attr,65,4)))
	  $DataRun = StringMid($attr,$Offset*2+1,(StringLen($attr)-$Offset)*2)
   ElseIf $NonResidentFlag = '00' Then
	  $DATA_LengthOfAttribute = Dec(_SwapEndian(StringMid($attr,33,8)),2)
	  $Offset = Dec(_SwapEndian(StringMid($attr,41,4)))
	  $DataRun = StringMid($attr,$Offset*2+1,$DATA_LengthOfAttribute*2)
   EndIf
EndFunc

Func _SwapEndian($iHex)
	Return StringMid(Binary(Dec($iHex,2)),3, StringLen($iHex))
EndFunc

Func _ExtractDataRuns()
	$r=UBound($RUN_Clusters)
	$i=1
	$RUN_VCN[0] = 0
	$BaseVCN = $RUN_VCN[0]
	If $DataRun = "" Then $DataRun = "00"
	Do
		$RunListID = StringMid($DataRun,$i,2)
		If $RunListID = "00" Then ExitLoop
		$i += 2
		$RunListClustersLength = Dec(StringMid($RunListID,2,1))
		$RunListVCNLength = Dec(StringMid($RunListID,1,1))
		$RunListClusters = Dec(_SwapEndian(StringMid($DataRun,$i,$RunListClustersLength*2)),2)
		$i += $RunListClustersLength*2
		$RunListVCN = _SwapEndian(StringMid($DataRun, $i, $RunListVCNLength*2))
		;next line handles positive or negative move
		$BaseVCN += Dec($RunListVCN,2)-(($r>1) And (Dec(StringMid($RunListVCN,1,1))>7))*Dec(StringMid("10000000000000000",1,$RunListVCNLength*2+1),2)
		If $RunListVCN <> "" Then
			$RunListVCN = $BaseVCN
		Else
			$RunListVCN = 0			;$RUN_VCN[$r-1]		;0
		EndIf
		If (($RunListVCN=0) And ($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
		 ;may be sparse section at end of Compression Signature
			_ArrayAdd($RUN_Clusters,Mod($RunListClusters,16))
			_ArrayAdd($RUN_VCN,$RunListVCN)
			$RunListClusters -= Mod($RunListClusters,16)
			$r += 1
		ElseIf (($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
		 ;may be compressed data section at start of Compression Signature
			_ArrayAdd($RUN_Clusters,$RunListClusters-Mod($RunListClusters,16))
			_ArrayAdd($RUN_VCN,$RunListVCN)
			$RunListVCN += $RUN_Clusters[$r]
			$RunListClusters = Mod($RunListClusters,16)
			$r += 1
		EndIf
	  ;just normal or sparse data
		_ArrayAdd($RUN_Clusters,$RunListClusters)
		_ArrayAdd($RUN_VCN,$RunListVCN)
		$r += 1
		$i += $RunListVCNLength*2
	Until $i > StringLen($DataRun)
EndFunc

Func _FindMFT($TargetDevice,$TargetFile)
	Local $nBytes;, $MFT_Record_Size=1024
	$tBuffer = DllStructCreate("byte[" & $MFT_Record_Size & "]")
	$hFile = _WinAPI_CreateFile($TargetDevice, 2, 2, 7)
	If $hFile = 0 Then
		ConsoleWrite("Error CreateFile in function _FindMFT(): " & _WinAPI_GetLastErrorMessage() & " for " & $TargetDevice & @CRLF)
		Return SetError(1,0,0)
	EndIf
	_WinAPI_SetFilePointerEx($hFile, $ImageOffset+$MFT_Offset, $FILE_BEGIN)
	_WinAPI_ReadFile($hFile, DllStructGetPtr($tBuffer), $MFT_Record_Size, $nBytes)
	_WinAPI_CloseHandle($hFile)
	$record = DllStructGetData($tBuffer, 1)
	If NOT StringMid($record,1,8) = '46494C45' Then
		ConsoleWrite("MFT record signature not found. "& @crlf)
		Return ""
	EndIf
	If StringMid($record,47,4) = "0100" AND Dec(_SwapEndian(StringMid($record,91,8))) = $TargetFile Then
;		ConsoleWrite("MFT record found" & @CRLF)
		Return $record		;returns record for MFT
	EndIf
	ConsoleWrite("MFT record not found" & @CRLF)
	Return ""
EndFunc

Func _ReadBootSector($TargetDevice)
	Local $nbytes
	$tBuffer=DllStructCreate("byte[512]")
	$hFile = _WinAPI_CreateFile($TargetDevice,2,2,7)
	If $hFile = 0 then
		ConsoleWrite("Error CreateFile in function _ReadBootSector(): " & _WinAPI_GetLastErrorMessage() & " for: " & $TargetDevice & @crlf)
		Return SetError(1,0,0)
	EndIf
	_WinAPI_SetFilePointerEx($hFile, $ImageOffset, $FILE_BEGIN)
	$read = _WinAPI_ReadFile($hFile, DllStructGetPtr($tBuffer), 512, $nBytes)
	If $read = 0 then
		ConsoleWrite("Error in function ReadFile: " & _WinAPI_GetLastErrorMessage() & " for: " & "\\.\" & $TargetDevice & @crlf)
		Return SetError(1,0,0)
	EndIf
	_WinAPI_CloseHandle($hFile)
   ; Good starting point from KaFu & trancexx at the AutoIt forum
	$tBootSectorSections = DllStructCreate("align 1;" & _
								"byte Jump[3];" & _
								"char SystemName[8];" & _
								"ushort BytesPerSector;" & _
								"ubyte SectorsPerCluster;" & _
								"ushort ReservedSectors;" & _
								"ubyte[3];" & _
								"ushort;" & _
								"ubyte MediaDescriptor;" & _
								"ushort;" & _
								"ushort SectorsPerTrack;" & _
								"ushort NumberOfHeads;" & _
								"dword HiddenSectors;" & _
								"dword;" & _
								"dword;" & _
								"int64 TotalSectors;" & _
								"int64 LogicalClusterNumberforthefileMFT;" & _
								"int64 LogicalClusterNumberforthefileMFTMirr;" & _
								"dword ClustersPerFileRecordSegment;" & _
								"dword ClustersPerIndexBlock;" & _
								"int64 NTFSVolumeSerialNumber;" & _
								"dword Checksum", DllStructGetPtr($tBuffer))
	If Not DllStructGetData($tBootSectorSections, "SystemName") = "NTFS" Then Return SetError(1,0,0)
	$BytesPerSector = DllStructGetData($tBootSectorSections, "BytesPerSector")
	$SectorsPerCluster = DllStructGetData($tBootSectorSections, "SectorsPerCluster")
	$BytesPerCluster = $BytesPerSector * $SectorsPerCluster
	$ClustersPerFileRecordSegment = DllStructGetData($tBootSectorSections, "ClustersPerFileRecordSegment")
	$LogicalClusterNumberforthefileMFT = DllStructGetData($tBootSectorSections, "LogicalClusterNumberforthefileMFT")
	$MFT_Offset = $BytesPerCluster * $LogicalClusterNumberforthefileMFT
	If $ClustersPerFileRecordSegment > 127 Then
		$MFT_Record_Size = 2 ^ (256 - $ClustersPerFileRecordSegment)
	Else
		$MFT_Record_Size = $BytesPerCluster * $ClustersPerFileRecordSegment
	EndIf
	$MFT_Record_Size=Int($MFT_Record_Size)
	$ClustersPerFileRecordSegment = Ceiling($MFT_Record_Size/$BytesPerCluster)
EndFunc

Func _HexEncode($bInput)
    Local $tInput = DllStructCreate("byte[" & BinaryLen($bInput) & "]")
    DllStructSetData($tInput, 1, $bInput)
    Local $a_iCall = DllCall("crypt32.dll", "int", "CryptBinaryToString", _
            "ptr", DllStructGetPtr($tInput), _
            "dword", DllStructGetSize($tInput), _
            "dword", 11, _
            "ptr", 0, _
            "dword*", 0)

    If @error Or Not $a_iCall[0] Then
        Return SetError(1, 0, "")
    EndIf

    Local $iSize = $a_iCall[5]
    Local $tOut = DllStructCreate("char[" & $iSize & "]")

    $a_iCall = DllCall("crypt32.dll", "int", "CryptBinaryToString", _
            "ptr", DllStructGetPtr($tInput), _
            "dword", DllStructGetSize($tInput), _
            "dword", 11, _
            "ptr", DllStructGetPtr($tOut), _
            "dword*", $iSize)

    If @error Or Not $a_iCall[0] Then
        Return SetError(2, 0, "")
    EndIf

    Return SetError(0, 0, DllStructGetData($tOut, 1))

EndFunc

Func _DecodeMFTRecord($TargetDevice,$MFTEntry,$MFTMode)
;Global $IndxEntryNumberArr[1],$IndxMFTReferenceArr[1],$IndxIndexFlagsArr[1],$IndxMFTReferenceOfParentArr[1],$IndxCTimeArr[1],$IndxATimeArr[1],$IndxMTimeArr[1],$IndxRTimeArr[1],$IndxAllocSizeArr[1],$IndxRealSizeArr[1],$IndxFileFlagsArr[1],$IndxFileNameArr[1],$IndxSubNodeVCNArr[1],$IndxNameSpaceArr[1]
Global $IndxEntryNumberArr[1],$IndxMFTReferenceArr[1],$IndxMFTReferenceOfParentArr[1],$IndxCTimeArr[1],$IndxATimeArr[1],$IndxMTimeArr[1],$IndxRTimeArr[1],$IndxFileNameArr[1]
Global $SIArrValue[13][1], $SIArrOffset[13][1], $SIArrSize[13][1], $FNArrValue[14][1], $FNArrOffset[14][1], $FN_Number=0, $Header_SequenceNo='', $Header_HardLinkCount=''
Local $SI_Number, $INDEXROOT_Number
Local $INDEX_ROOT_ON="FALSE",$INDEX_ALLOCATION_ON="FALSE",$CoreData[2],$CoreDataChunk,$CoreIndexAllocation,$CoreIndexAllocationChunk,$CoreIndexAllocationName
Local $UpdSeqArrPart0, $UpdSeqArrPart1, $UpdSeqArrPart2, $RecordEnd1, $RecordEnd2
Global $DataQ[1],$Mode2Data=""
Global $IRArr[12][2],$IndxArr[20][2]

_SetArrays()
$HEADER_RecordRealSize = ""
$HEADER_MFTREcordNumber = ""
$UpdSeqArrOffset = Dec(_SwapEndian(StringMid($MFTEntry,11,4)))
$UpdSeqArrSize = Dec(_SwapEndian(StringMid($MFTEntry,15,4)))
$UpdSeqArr = StringMid($MFTEntry,3+($UpdSeqArrOffset*2),$UpdSeqArrSize*2*2)
If Not $SkipFixups Then
	If $MFT_Record_Size = 1024 Then
		$UpdSeqArrPart0 = StringMid($UpdSeqArr,1,4)
		$UpdSeqArrPart1 = StringMid($UpdSeqArr,5,4)
		$UpdSeqArrPart2 = StringMid($UpdSeqArr,9,4)
		$RecordEnd1 = StringMid($MFTEntry,1023,4)
		$RecordEnd2 = StringMid($MFTEntry,2047,4)
		If $UpdSeqArrPart0 <> $RecordEnd1 OR $UpdSeqArrPart0 <> $RecordEnd2 Then
;			_DebugOut("The record failed Fixup", $MFTEntry)
			ConsoleWrite("The record failed Fixup:" & @CRLF)
			ConsoleWrite(_HexEncode($MFTEntry) & @CRLF)
			Return -1
		EndIf
		$MFTEntry = StringMid($MFTEntry,1,1022) & $UpdSeqArrPart1 & StringMid($MFTEntry,1027,1020) & $UpdSeqArrPart2
	ElseIf $MFT_Record_Size = 4096 Then
		$UpdSeqArrPart0 = StringMid($UpdSeqArr,1,4)
		$UpdSeqArrPart1 = StringMid($UpdSeqArr,5,4)
		$UpdSeqArrPart2 = StringMid($UpdSeqArr,9,4)
		Local $UpdSeqArrPart3 = StringMid($UpdSeqArr,13,4)
		Local $UpdSeqArrPart4 = StringMid($UpdSeqArr,17,4)
		Local $UpdSeqArrPart5 = StringMid($UpdSeqArr,21,4)
		Local $UpdSeqArrPart6 = StringMid($UpdSeqArr,25,4)
		Local $UpdSeqArrPart7 = StringMid($UpdSeqArr,29,4)
		Local $UpdSeqArrPart8 = StringMid($UpdSeqArr,33,4)
		$RecordEnd1 = StringMid($MFTEntry,1023,4)
		$RecordEnd2 = StringMid($MFTEntry,2047,4)
		Local $RecordEnd3 = StringMid($MFTEntry,3071,4)
		Local $RecordEnd4 = StringMid($MFTEntry,4095,4)
		Local $RecordEnd5 = StringMid($MFTEntry,5119,4)
		Local $RecordEnd6 = StringMid($MFTEntry,6143,4)
		Local $RecordEnd7 = StringMid($MFTEntry,7167,4)
		Local $RecordEnd8 = StringMid($MFTEntry,8191,4)
		If $UpdSeqArrPart0 <> $RecordEnd1 OR $UpdSeqArrPart0 <> $RecordEnd2 OR $UpdSeqArrPart0 <> $RecordEnd3 OR $UpdSeqArrPart0 <> $RecordEnd4 OR $UpdSeqArrPart0 <> $RecordEnd5 OR $UpdSeqArrPart0 <> $RecordEnd6 OR $UpdSeqArrPart0 <> $RecordEnd7 OR $UpdSeqArrPart0 <> $RecordEnd8 Then
;			_DebugOut("The record failed Fixup", $MFTEntry)
			ConsoleWrite("The record failed Fixup:" & @CRLF)
			ConsoleWrite(_HexEncode($MFTEntry) & @CRLF)
			Return -1
		Else
			$MFTEntry =  StringMid($MFTEntry,1,1022) & $UpdSeqArrPart1 & StringMid($MFTEntry,1027,1020) & $UpdSeqArrPart2 & StringMid($MFTEntry,2051,1020) & $UpdSeqArrPart3 & StringMid($MFTEntry,3075,1020) & $UpdSeqArrPart4 & StringMid($MFTEntry,4099,1020) & $UpdSeqArrPart5 & StringMid($MFTEntry,5123,1020) & $UpdSeqArrPart6 & StringMid($MFTEntry,6147,1020) & $UpdSeqArrPart7 & StringMid($MFTEntry,7171,1020) & $UpdSeqArrPart8
		EndIf
	EndIf
EndIf
;If $SkipFixups Then
;	$record_tmp = _DoFixup($record, $FileRef)
;	If Not $record_tmp = "" Then $record = $record_tmp
;EndIf
;If $record = "" then Return ""  ;corrupt, failed fixup
;------------If Not $SkipFixups Then $record = _DoFixup($record, $ref)
$HEADER_RecordRealSize = Dec(_SwapEndian(StringMid($MFTEntry,51,8)),2)
If $UpdSeqArrOffset = 48 Then
	$HEADER_MFTREcordNumber = Dec(_SwapEndian(StringMid($MFTEntry,91,8)),2)
Else
	$HEADER_MFTREcordNumber = "NT style"
EndIf
$Header_SequenceNo = Dec(_SwapEndian(StringMid($MFTEntry,35,4)))
$Header_HardLinkCount = Dec(_SwapEndian(StringMid($MFTEntry,39,4)))

$AttributeOffset = (Dec(StringMid($MFTEntry,43,2))*2)+3

While 1
	$AttributeType = StringMid($MFTEntry,$AttributeOffset,8)
	$AttributeSize = StringMid($MFTEntry,$AttributeOffset+8,8)
	$AttributeSize = Dec(_SwapEndian($AttributeSize),2)
;	ConsoleWrite("$AttributeType: " & $AttributeType & @CRLF)
	Select
		Case $AttributeType = $STANDARD_INFORMATION
;			$STANDARD_INFORMATION_ON = "TRUE"
			$SI_Number += 1
			_Get_StandardInformation($MFTEntry,$AttributeOffset,$SI_Number)
		Case $AttributeType = $ATTRIBUTE_LIST
;			$ATTRIBUTE_LIST_ON = "TRUE"
			;$ATTRIBLIST_Number += 1
			;$MFTEntryOrig = $MFTEntry
			$AttrList = StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2)
			_DecodeAttrList($HEADER_MFTRecordNumber, $AttrList)		;produces $AttrQ - extra record list
			$str = ""
			For $i = 1 To $AttrQ[0]
				$RetRec = _FindFileMFTRecord($TargetDevice,$AttrQ[$i])
				$record = $RetRec[1]
				$str &= _StripMftRecord($record)		;no header or end marker
			Next
			$str &= "FFFFFFFF"		;add end marker
			$MFTEntry = StringMid($MFTEntry,1,($HEADER_RecordRealSize-8)*2+2) & $str       ;strip "FFFFFFFF..." first
   		Case $AttributeType = $FILE_NAME
;			$FILE_NAME_ON = "TRUE"
			$FN_Number += 1
			$attr = StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2)
			$NameSpace = StringMid($attr,179,2)
			Select
				Case $NameSpace = "00"	;POSIX
					$NameQ[2] = $attr
				Case $NameSpace = "01"	;WIN32
					$NameQ[4] = $attr
				Case $NameSpace = "02"	;DOS
					$NameQ[1] = $attr
				Case $NameSpace = "03"	;DOS+WIN32
					$NameQ[3] = $attr
			EndSelect
			_Get_FileName($MFTEntry,$AttributeOffset,$FN_Number)
		Case $AttributeType = $OBJECT_ID
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$OBJECT_ID_ON = "TRUE"
			;$OBJID_Number += 1
		Case $AttributeType = $SECURITY_DESCRIPTOR
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$SECURITY_DESCRIPTOR_ON = "TRUE"
			;$SECURITY_Number += 1
		Case $AttributeType = $VOLUME_NAME
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$VOLUME_NAME_ON = "TRUE"
			;$VOLNAME_Number += 1
		Case $AttributeType = $VOLUME_INFORMATION
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$VOLUME_INFORMATION_ON = "TRUE"
			;$VOLINFO_Number += 1
		Case $AttributeType = $DATA
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$DATA_ON = "TRUE"
			;$DATA_Number += 1
;			ConsoleWrite("$DATA_Number: " & $DATA_Number & @CRLF)
			_ArrayAdd($DataQ, StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2))
			If $MFTMode = 2 Then ;For files that we need the content of, like the shadow copy master file. It is a small file so load it to memory
				$CoreData = _GetAttributeEntry($TargetDevice,StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2))
				$CoreDataChunk = $CoreData[0]
				;$CoreDataName = $CoreData[1]
				;ConsoleWrite("Retrieved data:" & @CRLF)
				;ConsoleWrite(_HexEncode("0x"&$CoreDataChunk) & @CRLF)
				$Mode2Data = $CoreDataChunk
			ElseIf $MFTMode = 3 Then ;For the actual shadow copy files we only want to locate the clusters
				$CoreData = _GetAttributeEntryNoRead($TargetDevice,StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2))
;				ConsoleWrite("$CoreData[1]: " & $CoreData[1] & @CRLF)
;				If $CoreData[1] = "$J" Then Return 2
			EndIf
		Case $AttributeType = $INDEX_ROOT
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
			$INDEX_ROOT_ON = "TRUE"
			$INDEXROOT_Number += 1
			ReDim $IRArr[12][$INDEXROOT_Number+1]
			;INDEX_ROOT is ok to process for shadows copy data as it is resident
;			If Not $IsRawShadowCopy Then
				$CoreIndexRoot = _GetAttributeEntry($TargetDevice,StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2))
				$CoreIndexRootChunk = $CoreIndexRoot[0]
				$CoreIndexRootName = $CoreIndexRoot[1]
				If $CoreIndexRootName = "$I30" Then _Get_IndexRoot($CoreIndexRootChunk,$INDEXROOT_Number,$CoreIndexRootName)
;			EndIf
		Case $AttributeType = $INDEX_ALLOCATION
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
			$INDEX_ALLOCATION_ON = "TRUE"
			;$INDEXALLOC_Number += 1
			If $DoIndxOffsetArray Then $IsCurrentIndxOfParent=1
;			ConsoleWrite("IsShadowCopy: " & $IsRawShadowCopy & @CRLF)
;			If Not $IsRawShadowCopy Then ;INDX may point to somewhere on the volume, and not within the shadow copy file
			If $MFTMode = 1 Then ;Regular mode, only parse
				$CoreIndexAllocation = _GetAttributeEntry($TargetDevice,StringMid($MFTEntry,$AttributeOffset,$AttributeSize*2))
				$CoreIndexAllocationChunk = $CoreIndexAllocation[0]
				$CoreIndexAllocationName = $CoreIndexAllocation[1]
	;			_Arrayadd($HexDumpIndxRecord,$CoreIndexAllocationChunk)
				If $CoreIndexAllocationName = "$I30" Then _Get_IndexAllocation($CoreIndexAllocationChunk)
			EndIf
		Case $AttributeType = $BITMAP
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$BITMAP_ON = "TRUE"
			;$BITMAP_Number += 1
		Case $AttributeType = $REPARSE_POINT
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$REPARSE_POINT_ON = "TRUE"
			;$REPARSEPOINT_Number += 1
		Case $AttributeType = $EA_INFORMATION
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$EA_INFORMATION_ON = "TRUE"
			;$EAINFO_Number += 1
		Case $AttributeType = $EA
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$EA_ON = "TRUE"
			;$EA_Number += 1
		Case $AttributeType = $PROPERTY_SET
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$PROPERTY_SET_ON = "TRUE"
			;$PROPERTYSET_Number += 1
		Case $AttributeType = $LOGGED_UTILITY_STREAM
			If $IsRawShadowCopy Then Return 1 ;We are only interested in ref and name for comparison.
;			$LOGGED_UTILITY_STREAM_ON = "TRUE"
			;$LOGGEDUTILSTREAM_Number += 1
		Case $AttributeType = $ATTRIBUTE_END_MARKER
			ExitLoop
	EndSelect
	$AttributeOffset += $AttributeSize*2
WEnd
$AttributesArr[9][2] = $INDEX_ROOT_ON
$AttributesArr[10][2] = $INDEX_ALLOCATION_ON
Return 2
EndFunc

Func _UnicodeHexToStr($FileName)
	$str = ""
	For $i = 1 To StringLen($FileName) Step 4
		$str &= ChrW(Dec(_SwapEndian(StringMid($FileName, $i, 4))))
	Next
	Return $str
EndFunc

Func _SetArrays()
	$SIArrValue[0][0] = "Field name:"
	$SIArrValue[1][0] = "HEADER_Flags"
	$SIArrValue[2][0] = "CTime"
	$SIArrValue[3][0] = "ATime"
	$SIArrValue[4][0] = "MTime"
	$SIArrValue[5][0] = "RTime"
	$SIArrValue[6][0] = "DOS File Permissions"
	$SIArrValue[7][0] = "Max Versions"
	$SIArrValue[8][0] = "Version Number"
	$SIArrValue[9][0] = "Class ID"
	$SIArrValue[10][0] = "Owner ID"
	$SIArrValue[11][0] = "Security ID"
	$SIArrValue[12][0] = "USN"


	$FNArrValue[0][0] = "Field name"
	$FNArrValue[1][0] = "ParentReferenceNo"
	$FNArrValue[2][0] = "ParentSequenceNo"
	$FNArrValue[3][0] = "CTime"
	$FNArrValue[4][0] = "ATime"
	$FNArrValue[5][0] = "MTime"
	$FNArrValue[6][0] = "RTime"
	$FNArrValue[7][0] = "AllocSize"
	$FNArrValue[8][0] = "RealSize"
	$FNArrValue[9][0] = "Flags"
	$FNArrValue[10][0] = "NameLength"
	$FNArrValue[11][0] = "NameType"
	$FNArrValue[12][0] = "NameSpace"
	$FNArrValue[13][0] = "FileName"

	$IndxEntryNumberArr[0] = "Entry number"
	$IndxMFTReferenceArr[0] = "MFTReference"
	$IndxMFTRefSeqNoArr[0] = "MFTReference SeqNo"
;	$IndxIndexFlagsArr[0] = "IndexFlags"
	$IndxMFTReferenceOfParentArr[0] = "Parent MFTReference"
	$IndxMFTParentRefSeqNoArr[0] = "Parent MFTReference SeqNo"
	$IndxCTimeArr[0] = "CTime"
	$IndxATimeArr[0] = "ATime"
	$IndxMTimeArr[0] = "MTime"
	$IndxRTimeArr[0] = "RTime"
;	$IndxAllocSizeArr[0] = "AllocSize"
;	$IndxRealSizeArr[0] = "RealSize"
;	$IndxFileFlagsArr[0] = "File flags"
	$IndxFileNameArr[0] = "FileName"
;	$IndxNameSpaceArr[0] = "NameSpace"
;	$IndxSubNodeVCNArr[0] = "SubNodeVCN"
EndFunc

Func _Get_StandardInformation($MFTEntry,$SI_Offset,$SI_Number)
Redim $SIArrValue[13][$SI_Number+1]
Redim $SIArrOffset[13][$SI_Number+1]
Redim $SIArrSize[13][$SI_Number+1]
$SI_HEADER_Flags = StringMid($MFTEntry,$SI_Offset+24,4)
$SI_HEADER_Flags = _SwapEndian($SI_HEADER_Flags)
$SI_HEADER_Flags = _AttribHeaderFlags("0x" & $SI_HEADER_Flags)
;
$SI_CTime = StringMid($MFTEntry,$SI_Offset+48,16)
$SI_CTime = _SwapEndian($SI_CTime)
$SI_CTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $SI_CTime)
$SI_CTime = _WinTime_UTCFileTimeFormat(Dec($SI_CTime)-$tDelta,$DateTimeFormat,2)
$SI_CTime = $SI_CTime & ":" & _FillZero(StringRight($SI_CTime_tmp,4))
;
$SI_ATime = StringMid($MFTEntry,$SI_Offset+64,16)
$SI_ATime = _SwapEndian($SI_ATime)
$SI_ATime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $SI_ATime)
$SI_ATime = _WinTime_UTCFileTimeFormat(Dec($SI_ATime)-$tDelta,$DateTimeFormat,2)
$SI_ATime = $SI_ATime & ":" & _FillZero(StringRight($SI_ATime_tmp,4))
;
$SI_MTime = StringMid($MFTEntry,$SI_Offset+80,16)
$SI_MTime = _SwapEndian($SI_MTime)
$SI_MTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $SI_MTime)
$SI_MTime = _WinTime_UTCFileTimeFormat(Dec($SI_MTime)-$tDelta,$DateTimeFormat,2)
$SI_MTime = $SI_MTime & ":" & _FillZero(StringRight($SI_MTime_tmp,4))
;
$SI_RTime = StringMid($MFTEntry,$SI_Offset+96,16)
$SI_RTime = _SwapEndian($SI_RTime)
$SI_RTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $SI_RTime)
$SI_RTime = _WinTime_UTCFileTimeFormat(Dec($SI_RTime)-$tDelta,$DateTimeFormat,2)
$SI_RTime = $SI_RTime & ":" & _FillZero(StringRight($SI_RTime_tmp,4))
;
$SI_FilePermission = StringMid($MFTEntry,$SI_Offset+112,8)
;ConsoleWrite("$SI_FilePermission: " & $SI_FilePermission & @CRLF)
$SI_FilePermission = _SwapEndian($SI_FilePermission)
$SI_FilePermission = _File_Permissions("0x" & $SI_FilePermission)
;ConsoleWrite("$SI_FilePermission: " & $SI_FilePermission & @CRLF)
$SI_MaxVersions = StringMid($MFTEntry,$SI_Offset+120,8)
$SI_MaxVersions = Dec(_SwapEndian($SI_MaxVersions))
$SI_VersionNumber = StringMid($MFTEntry,$SI_Offset+128,8)
$SI_VersionNumber = Dec(_SwapEndian($SI_VersionNumber))
$SI_ClassID = StringMid($MFTEntry,$SI_Offset+136,8)
$SI_ClassID = Dec(_SwapEndian($SI_ClassID))
$SI_OwnerID = StringMid($MFTEntry,$SI_Offset+144,8)
$SI_OwnerID = Dec(_SwapEndian($SI_OwnerID))
$SI_SecurityID = StringMid($MFTEntry,$SI_Offset+152,8)
$SI_SecurityID = Dec(_SwapEndian($SI_SecurityID))
$SI_USN = StringMid($MFTEntry,$SI_Offset+176,16)
$SI_USN = Dec(_SwapEndian($SI_USN))
If Not $IsFirstRun Then
	$SIArrValue[1][$SI_Number] = $SI_HEADER_Flags
	$SIArrValue[2][$SI_Number] = $SI_CTime
	$SIArrValue[3][$SI_Number] = $SI_ATime
	$SIArrValue[4][$SI_Number] = $SI_MTime
	$SIArrValue[5][$SI_Number] = $SI_RTime
	$SIArrValue[6][$SI_Number] = $SI_FilePermission
	$SIArrValue[7][$SI_Number] = $SI_MaxVersions
	$SIArrValue[8][$SI_Number] = $SI_VersionNumber
	$SIArrValue[9][$SI_Number] = $SI_ClassID
	$SIArrValue[10][$SI_Number] = $SI_OwnerID
	$SIArrValue[11][$SI_Number] = $SI_SecurityID
	$SIArrValue[12][$SI_Number] = $SI_USN
;	_ArrayDisplay($SIArrValue,"$SIArrValue")
;
	$SIArrOffset[1][$SI_Number] = $SI_Offset+24
	$SIArrOffset[2][$SI_Number] = $SI_Offset+48
	$SIArrOffset[3][$SI_Number] = $SI_Offset+64
	$SIArrOffset[4][$SI_Number] = $SI_Offset+80
	$SIArrOffset[5][$SI_Number] = $SI_Offset+96
	$SIArrOffset[6][$SI_Number] = $SI_Offset+112
	$SIArrOffset[7][$SI_Number] = $SI_Offset+120
	$SIArrOffset[8][$SI_Number] = $SI_Offset+128
	$SIArrOffset[9][$SI_Number] = $SI_Offset+136
	$SIArrOffset[10][$SI_Number] = $SI_Offset+144
	$SIArrOffset[11][$SI_Number] = $SI_Offset+152
	$SIArrOffset[12][$SI_Number] = $SI_Offset+176
;
	$SIArrSize[1][$SI_Number] = 2
	$SIArrSize[2][$SI_Number] = 8
	$SIArrSize[3][$SI_Number] = 8
	$SIArrSize[4][$SI_Number] = 8
	$SIArrSize[5][$SI_Number] = 8
	$SIArrSize[6][$SI_Number] = 4
	$SIArrSize[7][$SI_Number] = 4
	$SIArrSize[8][$SI_Number] = 4
	$SIArrSize[9][$SI_Number] = 4
	$SIArrSize[10][$SI_Number] = 4
	$SIArrSize[11][$SI_Number] = 4
	$SIArrSize[12][$SI_Number] = 8
EndIf
EndFunc

Func _Get_FileName($MFTEntry,$FN_Offset,$FN_Number)
Redim $FNArrValue[14][$FN_Number+1]
Redim $FNArrOffset[14][$FN_Number+1]
$FN_ParentReferenceNo = StringMid($MFTEntry,$FN_Offset+48,12)
$FN_ParentReferenceNo = Dec(_SwapEndian($FN_ParentReferenceNo))
$FN_ParentSequenceNo = StringMid($MFTEntry,$FN_Offset+60,4)
$FN_ParentSequenceNo = Dec(_SwapEndian($FN_ParentSequenceNo))
;
$FN_CTime = StringMid($MFTEntry,$FN_Offset+64,16)
$FN_CTime = _SwapEndian($FN_CTime)
$FN_CTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $FN_CTime)
$FN_CTime = _WinTime_UTCFileTimeFormat(Dec($FN_CTime)-$tDelta,$DateTimeFormat,2)
$FN_CTime = $FN_CTime & ":" & _FillZero(StringRight($FN_CTime_tmp,4))
;
$FN_ATime = StringMid($MFTEntry,$FN_Offset+80,16)
$FN_ATime = _SwapEndian($FN_ATime)
$FN_ATime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $FN_ATime)
$FN_ATime = _WinTime_UTCFileTimeFormat(Dec($FN_ATime)-$tDelta,$DateTimeFormat,2)
$FN_ATime = $FN_ATime & ":" & _FillZero(StringRight($FN_ATime_tmp,4))
;
$FN_MTime = StringMid($MFTEntry,$FN_Offset+96,16)
$FN_MTime = _SwapEndian($FN_MTime)
$FN_MTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $FN_MTime)
$FN_MTime = _WinTime_UTCFileTimeFormat(Dec($FN_MTime)-$tDelta,$DateTimeFormat,2)
$FN_MTime = $FN_MTime & ":" & _FillZero(StringRight($FN_MTime_tmp,4))
;
$FN_RTime = StringMid($MFTEntry,$FN_Offset+112,16)
$FN_RTime = _SwapEndian($FN_RTime)
$FN_RTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $FN_RTime)
$FN_RTime = _WinTime_UTCFileTimeFormat(Dec($FN_RTime)-$tDelta,$DateTimeFormat,2)
$FN_RTime = $FN_RTime & ":" & _FillZero(StringRight($FN_RTime_tmp,4))
;
$FN_AllocSize = StringMid($MFTEntry,$FN_Offset+128,16)
$FN_AllocSize = Dec(_SwapEndian($FN_AllocSize))
$FN_RealSize = StringMid($MFTEntry,$FN_Offset+144,16)
$FN_RealSize = Dec(_SwapEndian($FN_RealSize))
$FN_Flags = StringMid($MFTEntry,$FN_Offset+160,8)
;ConsoleWrite("$FN_Flags: " & $FN_Flags & @CRLF)
$FN_Flags = _SwapEndian($FN_Flags)
$FN_Flags = _File_Permissions("0x" & $FN_Flags)
;ConsoleWrite("$FN_Flags: " & $FN_Flags & @CRLF)
$FN_NameLength = StringMid($MFTEntry,$FN_Offset+176,2)
$FN_NameLength = Dec($FN_NameLength)
$FN_NameType = StringMid($MFTEntry,$FN_Offset+178,2)
Select
	Case $FN_NameType = '00'
		$FN_NameType = 'POSIX'
	Case $FN_NameType = '01'
		$FN_NameType = 'WIN32'
	Case $FN_NameType = '02'
		$FN_NameType = 'DOS'
	Case $FN_NameType = '03'
		$FN_NameType = 'DOS+WIN32'
	Case $FN_NameType <> '00' AND $FN_NameType <> '01' AND $FN_NameType <> '02' AND $FN_NameType <> '03'
		$FN_NameType = 'UNKNOWN'
EndSelect
$FN_NameSpace = $FN_NameLength-1
$FN_FileName = StringMid($MFTEntry,$FN_Offset+180,($FN_NameLength+$FN_NameSpace)*2)
$FN_FileName = _UnicodeHexToStr($FN_FileName)
;If StringLen($FN_FileName) <> $FN_NameLength Then $INVALID_FILENAME = 1
If Not $IsFirstRun Then
	$FNArrValue[0][$FN_Number] = "FN Number " & $FN_Number
	$FNArrValue[1][$FN_Number] = $FN_ParentReferenceNo
	$FNArrValue[2][$FN_Number] = $FN_ParentSequenceNo
	$FNArrValue[3][$FN_Number] = $FN_CTime
	$FNArrValue[4][$FN_Number] = $FN_ATime
	$FNArrValue[5][$FN_Number] = $FN_MTime
	$FNArrValue[6][$FN_Number] = $FN_RTime
	$FNArrValue[7][$FN_Number] = $FN_AllocSize
	$FNArrValue[8][$FN_Number] = $FN_RealSize
	$FNArrValue[9][$FN_Number] = $FN_Flags
	$FNArrValue[10][$FN_Number] = $FN_NameLength
	$FNArrValue[11][$FN_Number] = $FN_NameType
	$FNArrValue[12][$FN_Number] = $FN_NameSpace
	$FNArrValue[13][$FN_Number] = $FN_FileName
;	_ArrayDisplay($FNArrValue,"$FNArrValue")

	$FNArrOffset[0][$FN_Number] = "Internal offset"
	$FNArrOffset[1][$FN_Number] = $FN_Offset+48
	$FNArrOffset[2][$FN_Number] = $FN_Offset+60
	$FNArrOffset[3][$FN_Number] = $FN_Offset+64
	$FNArrOffset[4][$FN_Number] = $FN_Offset+80
	$FNArrOffset[5][$FN_Number] = $FN_Offset+96
	$FNArrOffset[6][$FN_Number] = $FN_Offset+112
	$FNArrOffset[7][$FN_Number] = $FN_Offset+128
	$FNArrOffset[8][$FN_Number] = $FN_Offset+144
	$FNArrOffset[9][$FN_Number] = $FN_Offset+160
	$FNArrOffset[10][$FN_Number] = $FN_Offset+176
	$FNArrOffset[11][$FN_Number] = $FN_Offset+178
	$FNArrOffset[12][$FN_Number] = ""
	$FNArrOffset[13][$FN_Number] = $FN_Offset+180
EndIf
EndFunc

Func _DecodeAttrList($TargetFile, $AttrList)
	Local $offset, $nBytes, $hFile, $LocalName, $ALNameLength
	If StringMid($AttrList, 17, 2) = "00" Then		;attribute list is in $AttrList
		$offset = Dec(_SwapEndian(StringMid($AttrList, 41, 4)))
		$List = StringMid($AttrList, $offset*2+1)
;		$IsolatedAttributeList = $list
	Else			;attribute list is found from data run in $AttrList
		$size = Dec(_SwapEndian(StringMid($AttrList, $offset*2 + 97, 16)))
		$offset = ($offset + Dec(_SwapEndian(StringMid($AttrList, $offset*2 + 65, 4))))*2
		$DataRun = StringMid($AttrList, $offset+1, StringLen($AttrList)-$offset)
;		ConsoleWrite("Attribute_List DataRun is " & $DataRun & @CRLF)
		Global $RUN_VCN[1], $RUN_Clusters[1]
		_ExtractDataRuns()
		$tBuffer = DllStructCreate("byte[" & $BytesPerCluster & "]")
		$hFile = _WinAPI_CreateFile($TargetDrive, 2, 6, 6)
		If $hFile = 0 Then
			ConsoleWrite("Error in function CreateFile when trying to locate Attribute List." & @CRLF)
			_WinAPI_CloseHandle($hFile)
			Return SetError(1,0,0)
		EndIf
		$List = ""
		For $r = 1 To Ubound($RUN_VCN)-1
			_WinAPI_SetFilePointerEx($hFile, $ImageOffset+($RUN_VCN[$r]*$BytesPerCluster), $FILE_BEGIN)
			For $i = 1 To $RUN_Clusters[$r]
				_WinAPI_ReadFile($hFile, DllStructGetPtr($tBuffer), $BytesPerCluster, $nBytes)
				$List &= StringTrimLeft(DllStructGetData($tBuffer, 1),2)
			Next
		Next
;		_DebugOut("***AttrList New:",$List)
		_WinAPI_CloseHandle($hFile)
		$List = StringMid($List, 1, $size*2)
	EndIf
	;$IsolatedAttributeList = $list
	$offset=0
	$str=""
	While StringLen($list) > $offset*2
		$type=StringMid($List, ($offset*2)+1, 8)
		;$ALRecordLength = Dec(_SwapEndian(StringMid($List, $offset*2 + 9, 4)))
		$ALNameLength = Dec(_SwapEndian(StringMid($List, $offset*2 + 13, 2)))
		;$ALNameOffset = Dec(_SwapEndian(StringMid($List, $offset*2 + 15, 2)))
		;$TestVCN = Dec(_SwapEndian(StringMid($List, $offset*2 + 17, 16)))
		$ref=Dec(_SwapEndian(StringMid($List, $offset*2 + 33, 8)))
		;$LocalAttribID = "0x" & StringMid($List, $offset*2 + 49, 2) & StringMid($List, $offset*2 + 51, 2)
		If $ALNameLength > 0 Then
			$LocalName = StringMid($List, $offset*2 + 53, $ALNameLength*2*2)
			$LocalName = _UnicodeHexToStr($LocalName)
		Else
			$LocalName = ""
		EndIf
		If $ref <> $TargetFile Then		;new attribute
			If Not StringInStr($str, $ref) Then $str &= $ref & "-"
		EndIf
		If $type=$DATA Then
			;$DataInAttrlist=1
			;$IsolatedData=StringMid($List, ($offset*2)+1, $ALRecordLength*2)
			;If $TestVCN=0 Then $DataIsResident=1
		EndIf
		$offset += Dec(_SwapEndian(StringMid($List, $offset*2 + 9, 4)))
	WEnd
	If $str = "" Then
		ConsoleWrite("No extra MFT records found" & @CRLF)
	Else
		$AttrQ = StringSplit(StringTrimRight($str,1), "-")
;		ConsoleWrite("Decode of $ATTRIBUTE_LIST reveiled extra MFT Records to be examined = " & _ArrayToString($AttrQ, @CRLF) & @CRLF)
	EndIf
EndFunc

Func _FindFileMFTRecord($TargetDevice,$TargetFile)
	Local $nBytes, $TmpOffset, $Counter, $Counter2, $RecordJumper, $TargetFileDec, $RecordsTooMuch, $RetVal[2], $Final, $i=0
	$tBuffer = DllStructCreate("byte[" & $MFT_Record_Size & "]")
	$hFile = _WinAPI_CreateFile($TargetDevice, 2, 6, 6)
	If $hFile = 0 Then
		ConsoleWrite("Error CreateFile in function _FindFileMFTRecord(): " & _WinAPI_GetLastErrorMessage() & " for " & $TargetDevice & @CRLF)
		_WinAPI_CloseHandle($hFile)
		Return SetError(1,0,0)
	EndIf
	$TargetFile = _DecToLittleEndian($TargetFile)
	$TargetFileDec = Dec(_SwapEndian($TargetFile),2)
;	ConsoleWrite("$TargetFileDec: " & $TargetFileDec & @CRLF)
;	ConsoleWrite("$SectorsPerCluster: " & $SectorsPerCluster & @CRLF)
;	ConsoleWrite("UBound($MFT_RUN_Clusters): " & UBound($MFT_RUN_Clusters) & @CRLF)
	Local $RecordsDivisor = $MFT_Record_Size/512
;	ConsoleWrite("$RecordsDivisor: " & $RecordsDivisor & @CRLF)
	For $i = 1 To UBound($MFT_RUN_Clusters)-1
		$CurrentClusters = $MFT_RUN_Clusters[$i]
		$RecordsInCurrentRun = ($CurrentClusters*$SectorsPerCluster)/$RecordsDivisor
;		ConsoleWrite("$CurrentClusters: " & $CurrentClusters & @CRLF)
;		ConsoleWrite("$RecordsInCurrentRun: " & $RecordsInCurrentRun & @CRLF)
		$Counter+=$RecordsInCurrentRun
;		ConsoleWrite("$Counter: " & $Counter & @CRLF)
		If $Counter>$TargetFileDec Then
			ExitLoop
		EndIf
	Next
;	ConsoleWrite("$i: " & $i & @CRLF)
	$TryAt = $Counter-$RecordsInCurrentRun
;	ConsoleWrite("$TryAt: " & $TryAt & @CRLF)
	;$TryAtArrIndex = $i
	$RecordsPerCluster = $SectorsPerCluster/$RecordsDivisor
	Do
		$RecordJumper+=$RecordsPerCluster
		$Counter2+=1
		$Final = $TryAt+$RecordJumper
	Until $Final>=$TargetFileDec
	$RecordsTooMuch = $Final-$TargetFileDec
;	ConsoleWrite("$RecordsTooMuch: " & $RecordsTooMuch & @CRLF)
	_WinAPI_SetFilePointerEx($hFile, $ImageOffset+($MFT_RUN_VCN[$i]*$BytesPerCluster)+($Counter2*$BytesPerCluster)-($RecordsTooMuch*$MFT_Record_Size), $FILE_BEGIN)
	_WinAPI_ReadFile($hFile, DllStructGetPtr($tBuffer), $MFT_Record_Size, $nBytes)
	$record = DllStructGetData($tBuffer, 1)
;	ConsoleWrite("Record:" & @CRLF)
;	ConsoleWrite(_HexEncode($record) & @CRLF)
	If StringMid($record,91,8) = $TargetFile Then
		$TmpOffset = DllCall('kernel32.dll', 'int', 'SetFilePointerEx', 'ptr', $hFile, 'int64', 0, 'int64*', 0, 'dword', 1)
		$FoundOffset = Int($TmpOffset[3])-Int($MFT_Record_Size)
;		ConsoleWrite("Record number: " & Dec(_SwapEndian($TargetFile),2) & " found at volume offset: 0x" & Hex($FoundOffset) & @CRLF)
		_WinAPI_CloseHandle($hFile)
		$RetVal[0] = $FoundOffset
		$RetVal[1] = $record
		Return $RetVal
	Else
		_WinAPI_CloseHandle($hFile)
		Return ""
	EndIf
EndFunc

Func _StripMftRecord($MFTEntry)
	Local $UpdSeqArrPart0, $UpdSeqArrPart1, $UpdSeqArrPart2, $RecordEnd1, $RecordEnd2
	$UpdSeqArrOffset = Dec(_SwapEndian(StringMid($MFTEntry,11,4)))
	$UpdSeqArrSize = Dec(_SwapEndian(StringMid($MFTEntry,15,4)))
	$UpdSeqArr = StringMid($MFTEntry,3+($UpdSeqArrOffset*2),$UpdSeqArrSize*2*2)

	If $MFT_Record_Size = 1024 Then
		$UpdSeqArrPart0 = StringMid($UpdSeqArr,1,4)
		$UpdSeqArrPart1 = StringMid($UpdSeqArr,5,4)
		$UpdSeqArrPart2 = StringMid($UpdSeqArr,9,4)
		$RecordEnd1 = StringMid($MFTEntry,1023,4)
		$RecordEnd2 = StringMid($MFTEntry,2047,4)
		If $UpdSeqArrPart0 <> $RecordEnd1 OR $UpdSeqArrPart0 <> $RecordEnd2 Then
;			_DebugOut("The record failed Fixup", $MFTEntry)
			ConsoleWrite("The record failed Fixup:" & @CRLF)
			ConsoleWrite(_HexEncode($MFTEntry) & @CRLF)
			Return ""
		EndIf
		$MFTEntry = StringMid($MFTEntry,1,1022) & $UpdSeqArrPart1 & StringMid($MFTEntry,1027,1020) & $UpdSeqArrPart2
	ElseIf $MFT_Record_Size = 4096 Then
		$UpdSeqArrPart0 = StringMid($UpdSeqArr,1,4)
		$UpdSeqArrPart1 = StringMid($UpdSeqArr,5,4)
		$UpdSeqArrPart2 = StringMid($UpdSeqArr,9,4)
		Local $UpdSeqArrPart3 = StringMid($UpdSeqArr,13,4)
		Local $UpdSeqArrPart4 = StringMid($UpdSeqArr,17,4)
		Local $UpdSeqArrPart5 = StringMid($UpdSeqArr,21,4)
		Local $UpdSeqArrPart6 = StringMid($UpdSeqArr,25,4)
		Local $UpdSeqArrPart7 = StringMid($UpdSeqArr,29,4)
		Local $UpdSeqArrPart8 = StringMid($UpdSeqArr,33,4)
		$RecordEnd1 = StringMid($MFTEntry,1023,4)
		$RecordEnd2 = StringMid($MFTEntry,2047,4)
		Local $RecordEnd3 = StringMid($MFTEntry,3071,4)
		Local $RecordEnd4 = StringMid($MFTEntry,4095,4)
		Local $RecordEnd5 = StringMid($MFTEntry,5119,4)
		Local $RecordEnd6 = StringMid($MFTEntry,6143,4)
		Local $RecordEnd7 = StringMid($MFTEntry,7167,4)
		Local $RecordEnd8 = StringMid($MFTEntry,8191,4)
		If $UpdSeqArrPart0 <> $RecordEnd1 OR $UpdSeqArrPart0 <> $RecordEnd2 OR $UpdSeqArrPart0 <> $RecordEnd3 OR $UpdSeqArrPart0 <> $RecordEnd4 OR $UpdSeqArrPart0 <> $RecordEnd5 OR $UpdSeqArrPart0 <> $RecordEnd6 OR $UpdSeqArrPart0 <> $RecordEnd7 OR $UpdSeqArrPart0 <> $RecordEnd8 Then
;			_DebugOut("The record failed Fixup", $MFTEntry)
			ConsoleWrite("The record failed Fixup:" & @CRLF)
			ConsoleWrite(_HexEncode($MFTEntry) & @CRLF)
			Return ""
		Else
			$MFTEntry =  StringMid($MFTEntry,1,1022) & $UpdSeqArrPart1 & StringMid($MFTEntry,1027,1020) & $UpdSeqArrPart2 & StringMid($MFTEntry,2051,1020) & $UpdSeqArrPart3 & StringMid($MFTEntry,3075,1020) & $UpdSeqArrPart4 & StringMid($MFTEntry,4099,1020) & $UpdSeqArrPart5 & StringMid($MFTEntry,5123,1020) & $UpdSeqArrPart6 & StringMid($MFTEntry,6147,1020) & $UpdSeqArrPart7 & StringMid($MFTEntry,7171,1020) & $UpdSeqArrPart8
		EndIf
	EndIf

	$RecordSize = Dec(_SwapEndian(StringMid($MFTEntry,51,8)),2)
	$HeaderSize = Dec(_SwapEndian(StringMid($MFTEntry,43,4)),2)
	$MFTEntry = StringMid($MFTEntry,$HeaderSize*2+3,($RecordSize-$HeaderSize-8)*2)        ;strip "0x..." and "FFFFFFFF..."
	Return $MFTEntry
EndFunc

Func _GetAttributeEntry($TargetDevice,$Entry)
	Local $CoreAttribute,$CoreAttributeTmp,$CoreAttributeArr[2],$TestArray,$Bytes
	Local $ATTRIBUTE_HEADER_Length,$ATTRIBUTE_HEADER_NonResidentFlag,$ATTRIBUTE_HEADER_NameLength,$ATTRIBUTE_HEADER_NameRelativeOffset,$ATTRIBUTE_HEADER_Name,$ATTRIBUTE_HEADER_Flags,$ATTRIBUTE_HEADER_AttributeID,$ATTRIBUTE_HEADER_StartVCN,$ATTRIBUTE_HEADER_LastVCN
	Local $ATTRIBUTE_HEADER_OffsetToDataRuns,$ATTRIBUTE_HEADER_CompressionUnitSize,$ATTRIBUTE_HEADER_Padding,$ATTRIBUTE_HEADER_AllocatedSize,$ATTRIBUTE_HEADER_RealSize,$ATTRIBUTE_HEADER_InitializedStreamSize,$RunListOffset
	Local $ATTRIBUTE_HEADER_LengthOfAttribute,$ATTRIBUTE_HEADER_OffsetToAttribute
	If $IsCurrentIndxOfParent Then Global $RawOffsetIndxArray
	$ATTRIBUTE_HEADER_Length = StringMid($Entry,9,8)
	$ATTRIBUTE_HEADER_Length = Dec(StringMid($ATTRIBUTE_HEADER_Length,7,2) & StringMid($ATTRIBUTE_HEADER_Length,5,2) & StringMid($ATTRIBUTE_HEADER_Length,3,2) & StringMid($ATTRIBUTE_HEADER_Length,1,2))
	$ATTRIBUTE_HEADER_NonResidentFlag = StringMid($Entry,17,2)
;	ConsoleWrite("$ATTRIBUTE_HEADER_NonResidentFlag = " & $ATTRIBUTE_HEADER_NonResidentFlag & @crlf)
	$ATTRIBUTE_HEADER_NameLength = Dec(StringMid($Entry,19,2))
;	ConsoleWrite("$ATTRIBUTE_HEADER_NameLength = " & $ATTRIBUTE_HEADER_NameLength & @crlf)
	$ATTRIBUTE_HEADER_NameRelativeOffset = StringMid($Entry,21,4)
;	ConsoleWrite("$ATTRIBUTE_HEADER_NameRelativeOffset = " & $ATTRIBUTE_HEADER_NameRelativeOffset & @crlf)
	$ATTRIBUTE_HEADER_NameRelativeOffset = Dec(_SwapEndian($ATTRIBUTE_HEADER_NameRelativeOffset))
;	ConsoleWrite("$ATTRIBUTE_HEADER_NameRelativeOffset = " & $ATTRIBUTE_HEADER_NameRelativeOffset & @crlf)
	If $ATTRIBUTE_HEADER_NameLength > 0 Then
		$ATTRIBUTE_HEADER_Name = _UnicodeHexToStr(StringMid($Entry,$ATTRIBUTE_HEADER_NameRelativeOffset*2 + 1,$ATTRIBUTE_HEADER_NameLength*4))
	Else
		$ATTRIBUTE_HEADER_Name = ""
	EndIf
	$ATTRIBUTE_HEADER_Flags = _SwapEndian(StringMid($Entry,25,4))
;	ConsoleWrite("$ATTRIBUTE_HEADER_Flags = " & $ATTRIBUTE_HEADER_Flags & @crlf)
	$Flags = ""
	If $ATTRIBUTE_HEADER_Flags = "0000" Then
		$Flags = "NORMAL"
	Else
		If BitAND($ATTRIBUTE_HEADER_Flags,"0001") Then
			$IsCompressed = 1
			$Flags &= "COMPRESSED+"
		EndIf
		If BitAND($ATTRIBUTE_HEADER_Flags,"4000") Then
			;$IsEncrypted = 1
			$Flags &= "ENCRYPTED+"
		EndIf
		If BitAND($ATTRIBUTE_HEADER_Flags,"8000") Then
			;$IsSparse = 1
			$Flags &= "SPARSE+"
		EndIf
		$Flags = StringTrimRight($Flags,1)
	EndIf
;	ConsoleWrite("File is " & $Flags & @CRLF)
	$ATTRIBUTE_HEADER_AttributeID = StringMid($Entry,29,4)
	$ATTRIBUTE_HEADER_AttributeID = StringMid($ATTRIBUTE_HEADER_AttributeID,3,2) & StringMid($ATTRIBUTE_HEADER_AttributeID,1,2)
	If $ATTRIBUTE_HEADER_NonResidentFlag = '01' Then
		$ATTRIBUTE_HEADER_StartVCN = StringMid($Entry,33,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_StartVCN = " & $ATTRIBUTE_HEADER_StartVCN & @crlf)
		$ATTRIBUTE_HEADER_StartVCN = Dec(_SwapEndian($ATTRIBUTE_HEADER_StartVCN),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_StartVCN = " & $ATTRIBUTE_HEADER_StartVCN & @crlf)
		$ATTRIBUTE_HEADER_LastVCN = StringMid($Entry,49,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LastVCN = " & $ATTRIBUTE_HEADER_LastVCN & @crlf)
		$ATTRIBUTE_HEADER_LastVCN = Dec(_SwapEndian($ATTRIBUTE_HEADER_LastVCN),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LastVCN = " & $ATTRIBUTE_HEADER_LastVCN & @crlf)
		;$ATTRIBUTE_HEADER_VCNs = $ATTRIBUTE_HEADER_LastVCN - $ATTRIBUTE_HEADER_StartVCN
;		ConsoleWrite("$ATTRIBUTE_HEADER_VCNs = " & $ATTRIBUTE_HEADER_VCNs & @crlf)
		$ATTRIBUTE_HEADER_OffsetToDataRuns = StringMid($Entry,65,4)
		$ATTRIBUTE_HEADER_OffsetToDataRuns = Dec(StringMid($ATTRIBUTE_HEADER_OffsetToDataRuns,3,1) & StringMid($ATTRIBUTE_HEADER_OffsetToDataRuns,3,1))
		$ATTRIBUTE_HEADER_CompressionUnitSize = Dec(_SwapEndian(StringMid($Entry,69,4)))
;		ConsoleWrite("$ATTRIBUTE_HEADER_CompressionUnitSize = " & $ATTRIBUTE_HEADER_CompressionUnitSize & @crlf)
		$IsCompressed = 0
		If $ATTRIBUTE_HEADER_CompressionUnitSize = 4 Then $IsCompressed = 1
		$ATTRIBUTE_HEADER_Padding = StringMid($Entry,73,8)
		$ATTRIBUTE_HEADER_Padding = StringMid($ATTRIBUTE_HEADER_Padding,7,2) & StringMid($ATTRIBUTE_HEADER_Padding,5,2) & StringMid($ATTRIBUTE_HEADER_Padding,3,2) & StringMid($ATTRIBUTE_HEADER_Padding,1,2)
		$ATTRIBUTE_HEADER_AllocatedSize = StringMid($Entry,81,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_AllocatedSize = " & $ATTRIBUTE_HEADER_AllocatedSize & @crlf)
		$ATTRIBUTE_HEADER_AllocatedSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_AllocatedSize),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_AllocatedSize = " & $ATTRIBUTE_HEADER_AllocatedSize & @crlf)
		$ATTRIBUTE_HEADER_RealSize = StringMid($Entry,97,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_RealSize = " & $ATTRIBUTE_HEADER_RealSize & @crlf)
		$ATTRIBUTE_HEADER_RealSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_RealSize),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_RealSize = " & $ATTRIBUTE_HEADER_RealSize & @crlf)
		$ATTRIBUTE_HEADER_InitializedStreamSize = StringMid($Entry,113,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_InitializedStreamSize = " & $ATTRIBUTE_HEADER_InitializedStreamSize & @crlf)
		$ATTRIBUTE_HEADER_InitializedStreamSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_InitializedStreamSize),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_InitializedStreamSize = " & $ATTRIBUTE_HEADER_InitializedStreamSize & @crlf)
		$RunListOffset = StringMid($Entry,65,4)
;		ConsoleWrite("$RunListOffset = " & $RunListOffset & @crlf)
		$RunListOffset = Dec(_SwapEndian($RunListOffset))
;		ConsoleWrite("$RunListOffset = " & $RunListOffset & @crlf)
		If $IsCompressed AND $RunListOffset = 72 Then
			$ATTRIBUTE_HEADER_CompressedSize = StringMid($Entry,129,16)
			$ATTRIBUTE_HEADER_CompressedSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_CompressedSize),2)
		EndIf
		$DataRun = StringMid($Entry,$RunListOffset*2+1,(StringLen($Entry)-$RunListOffset)*2)
;		ConsoleWrite("$DataRun = " & $DataRun & @crlf)
	ElseIf $ATTRIBUTE_HEADER_NonResidentFlag = '00' Then
		$ATTRIBUTE_HEADER_LengthOfAttribute = StringMid($Entry,33,8)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LengthOfAttribute = " & $ATTRIBUTE_HEADER_LengthOfAttribute & @crlf)
		$ATTRIBUTE_HEADER_LengthOfAttribute = Dec(_SwapEndian($ATTRIBUTE_HEADER_LengthOfAttribute),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LengthOfAttribute = " & $ATTRIBUTE_HEADER_LengthOfAttribute & @crlf)
;		$ATTRIBUTE_HEADER_OffsetToAttribute = StringMid($Entry,41,4)
;		$ATTRIBUTE_HEADER_OffsetToAttribute = Dec(StringMid($ATTRIBUTE_HEADER_OffsetToAttribute,3,2) & StringMid($ATTRIBUTE_HEADER_OffsetToAttribute,1,2))
		$ATTRIBUTE_HEADER_OffsetToAttribute = Dec(_SwapEndian(StringMid($Entry,41,4)))
;		ConsoleWrite("$ATTRIBUTE_HEADER_OffsetToAttribute = " & $ATTRIBUTE_HEADER_OffsetToAttribute & @crlf)
		;$ATTRIBUTE_HEADER_IndexedFlag = Dec(StringMid($Entry,45,2))
		$ATTRIBUTE_HEADER_Padding = StringMid($Entry,47,2)
		$DataRun = StringMid($Entry,$ATTRIBUTE_HEADER_OffsetToAttribute*2+1,$ATTRIBUTE_HEADER_LengthOfAttribute*2)
;		ConsoleWrite("$DataRun = " & $DataRun & @crlf)
	EndIf
; Possible continuation
;	For $i = 1 To UBound($DataQ) - 1
	For $i = 1 To 1
;		_DecodeDataQEntry($DataQ[$i])
		If $ATTRIBUTE_HEADER_NonResidentFlag = '00' Then
;_ExtractResidentFile($DATA_Name, $DATA_LengthOfAttribute)
			$CoreAttribute = $DataRun
		Else
			Global $RUN_VCN[1], $RUN_Clusters[1]

			$TotalClusters = $ATTRIBUTE_HEADER_LastVCN - $ATTRIBUTE_HEADER_StartVCN + 1
			$Size = $ATTRIBUTE_HEADER_RealSize
;_ExtractDataRuns()
			$r=UBound($RUN_Clusters)
			$i=1
			$RUN_VCN[0] = 0
			$BaseVCN = $RUN_VCN[0]
			If $DataRun = "" Then $DataRun = "00"
			Do
				$RunListID = StringMid($DataRun,$i,2)
				If $RunListID = "00" Then ExitLoop
;				ConsoleWrite("$RunListID = " & $RunListID & @crlf)
				$i += 2
				$RunListClustersLength = Dec(StringMid($RunListID,2,1))
;				ConsoleWrite("$RunListClustersLength = " & $RunListClustersLength & @crlf)
				$RunListVCNLength = Dec(StringMid($RunListID,1,1))
;				ConsoleWrite("$RunListVCNLength = " & $RunListVCNLength & @crlf)
				$RunListClusters = Dec(_SwapEndian(StringMid($DataRun,$i,$RunListClustersLength*2)),2)
;				ConsoleWrite("$RunListClusters = " & $RunListClusters & @crlf)
				$i += $RunListClustersLength*2
				$RunListVCN = _SwapEndian(StringMid($DataRun, $i, $RunListVCNLength*2))
				;next line handles positive or negative move
				$BaseVCN += Dec($RunListVCN,2)-(($r>1) And (Dec(StringMid($RunListVCN,1,1))>7))*Dec(StringMid("10000000000000000",1,$RunListVCNLength*2+1),2)
				If $RunListVCN <> "" Then
					$RunListVCN = $BaseVCN
				Else
					$RunListVCN = 0			;$RUN_VCN[$r-1]		;0
				EndIf
;				ConsoleWrite("$RunListVCN = " & $RunListVCN & @crlf)
				If (($RunListVCN=0) And ($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
				;If (($RunListVCN=$RUN_VCN[$r-1]) And ($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
				;may be sparse section at end of Compression Signature
					_ArrayAdd($RUN_Clusters,Mod($RunListClusters,16))
					_ArrayAdd($RUN_VCN,$RunListVCN)
					$RunListClusters -= Mod($RunListClusters,16)
					$r += 1
				ElseIf (($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
				;may be compressed data section at start of Compression Signature
					_ArrayAdd($RUN_Clusters,$RunListClusters-Mod($RunListClusters,16))
					_ArrayAdd($RUN_VCN,$RunListVCN)
					$RunListVCN += $RUN_Clusters[$r]
					$RunListClusters = Mod($RunListClusters,16)
					$r += 1
				EndIf
			;just normal or sparse data
				_ArrayAdd($RUN_Clusters,$RunListClusters)
				_ArrayAdd($RUN_VCN,$RunListVCN)
				$r += 1
				$i += $RunListVCNLength*2
			Until $i > StringLen($DataRun)
;--------------------------------_ExtractDataRuns()
;			_ArrayDisplay($RUN_Clusters,"$RUN_Clusters")
;			_ArrayDisplay($RUN_VCN,"$RUN_VCN")
			If $TotalClusters * $BytesPerCluster >= $Size Then
;				ConsoleWrite(_ArrayToString($RUN_VCN) & @CRLF)
;				ConsoleWrite(_ArrayToString($RUN_Clusters) & @CRLF)
;ExtractFile
				Local $nBytes
				$hFile = _WinAPI_CreateFile($TargetDevice, 2, 6, 6)
				If $hFile = 0 Then
					ConsoleWrite("Error CreateFile in function _GetAttributeEntry()." & @CRLF)
					_WinAPI_CloseHandle($hFile)
					Return
				EndIf
				$tBuffer = DllStructCreate("byte[" & $BytesPerCluster * 16 & "]")
				Select
					Case UBound($RUN_VCN) = 1		;no data, do nothing
					Case (UBound($RUN_VCN) = 2) Or (Not $IsCompressed)	;may be normal or sparse
						If $RUN_VCN[1] = $RUN_VCN[0] And $DATA_Name <> "$Boot" Then		;sparse, unless $Boot
;							_DoSparse($htest)
							ConsoleWrite("Error: Sparse attributes not supported!!!" & @CRLF)
						Else								;normal
;							_DoNormalAttribute($hFile, $tBuffer)
;							Local $nBytes
							$FileSize = $ATTRIBUTE_HEADER_RealSize
							Local $TestArray[UBound($RUN_VCN)][4]
							$TestArray[0][0] = "Offset"
							$TestArray[0][1] = "Bytes Accumulated"
							$TestArray[0][2] = "Bytes per Run"
							$TestArray[0][3] = "Sectors per Run"
							For $s = 1 To UBound($RUN_VCN)-1
								;An attempt at preparing for INDX modification
								$TestArray[$s][0] = $RUN_VCN[$s]*$BytesPerCluster
								_WinAPI_SetFilePointerEx($hFile, $ImageOffset+($RUN_VCN[$s]*$BytesPerCluster), $FILE_BEGIN)
								$g = $RUN_Clusters[$s]
								While $g > 16 And $FileSize > $BytesPerCluster * 16
									$Bytes += $BytesPerCluster * 16 ;Did this impact negatively??
									_WinAPI_ReadFile($hFile, DllStructGetPtr($tBuffer), $BytesPerCluster * 16, $nBytes)
;									_WinAPI_WriteFile($htest, DllStructGetPtr($tBuffer), $BytesPerCluster * 16, $nBytes)
									$g -= 16
									$FileSize -= $BytesPerCluster * 16
									$CoreAttributeTmp = StringMid(DllStructGetData($tBuffer,1),3,$BytesPerCluster*16*2)
									$CoreAttribute &= $CoreAttributeTmp
								WEnd
								If $g <> 0 Then
									$Bytes += $BytesPerCluster * $g ;Did this impact negatively??
									_WinAPI_ReadFile($hFile, DllStructGetPtr($tBuffer), $BytesPerCluster * $g, $nBytes)
;									$CoreAttributeTmp = StringMid(DllStructGetData($tBuffer,1),3)
;									$CoreAttribute &= $CoreAttributeTmp
									If $FileSize > $BytesPerCluster * $g Then
;										_WinAPI_WriteFile($htest, DllStructGetPtr($tBuffer), $BytesPerCluster * $g, $nBytes)
										$FileSize -= $BytesPerCluster * $g
										$CoreAttributeTmp = StringMid(DllStructGetData($tBuffer,1),3,$BytesPerCluster*$g*2)
										$CoreAttribute &= $CoreAttributeTmp
									Else
;										_WinAPI_WriteFile($htest, DllStructGetPtr($tBuffer), $FileSize, $nBytes)
;										Return
										$CoreAttributeTmp = StringMid(DllStructGetData($tBuffer,1),3,$FileSize*2)
										$CoreAttribute &= $CoreAttributeTmp
									EndIf
								EndIf
								;An attempt at preparing for INDX modification
								$TestArray[$s][1] = $Bytes
							Next
;------------------_DoNormalAttribute()
						EndIf
					Case Else					;may be compressed
;						_DoCompressed($hFile, $htest, $tBuffer)
						ConsoleWrite("Error: Compressed attributes not supported!!!" & @CRLF)
				EndSelect
;------------------------ExtractFile
			EndIf
;-------------------------
		EndIf
	Next
	$CoreAttributeArr[0] = $CoreAttribute
	$CoreAttributeArr[1] = $ATTRIBUTE_HEADER_Name

	If $IsCurrentIndxOfParent And $ATTRIBUTE_HEADER_Name = "$I30" Then ;Generate the offset array for the INDX of the parent, if required
		$RawOffsetIndxArray = $TestArray
		For $i = 1 To UBound($RawOffsetIndxArray)-1
			If $i = 1 Then
				$RawOffsetIndxArray[$i][2] = $RawOffsetIndxArray[$i][1]
			Else
				$RawOffsetIndxArray[$i][2] = $RawOffsetIndxArray[$i][1] - $RawOffsetIndxArray[$i-1][1]
			EndIf
			$RawOffsetIndxArray[$i][3] = $RawOffsetIndxArray[$i][2]/512
		Next
;		_ArrayDisplay($RawOffsetIndxArray,"$RawOffsetIndxArray")
		$IsCurrentIndxOfParent=0
		$DoIndxOffsetArray=0
	EndIf
	Return $CoreAttributeArr
EndFunc

Func _GetAttributeEntryNoRead($TargetDevice,$Entry)
;	ConsoleWrite("_GetAttributeEntryNoRead()" & @crlf)
	Local $CoreAttribute,$CoreAttributeArr[2],$TestArray,$Bytes
	Local $ATTRIBUTE_HEADER_Length,$ATTRIBUTE_HEADER_NonResidentFlag,$ATTRIBUTE_HEADER_NameLength,$ATTRIBUTE_HEADER_NameRelativeOffset,$ATTRIBUTE_HEADER_Name,$ATTRIBUTE_HEADER_Flags,$ATTRIBUTE_HEADER_AttributeID,$ATTRIBUTE_HEADER_StartVCN,$ATTRIBUTE_HEADER_LastVCN
	Local $ATTRIBUTE_HEADER_OffsetToDataRuns,$ATTRIBUTE_HEADER_Padding,$ATTRIBUTE_HEADER_AllocatedSize,$ATTRIBUTE_HEADER_RealSize,$ATTRIBUTE_HEADER_InitializedStreamSize,$RunListOffset
	Local $ATTRIBUTE_HEADER_LengthOfAttribute,$ATTRIBUTE_HEADER_OffsetToAttribute
	;Global $RawTestOffsetArray

	$ATTRIBUTE_HEADER_Length = StringMid($Entry,9,8)
	$ATTRIBUTE_HEADER_Length = Dec(StringMid($ATTRIBUTE_HEADER_Length,7,2) & StringMid($ATTRIBUTE_HEADER_Length,5,2) & StringMid($ATTRIBUTE_HEADER_Length,3,2) & StringMid($ATTRIBUTE_HEADER_Length,1,2))
	$ATTRIBUTE_HEADER_NonResidentFlag = StringMid($Entry,17,2)
;	ConsoleWrite("$ATTRIBUTE_HEADER_NonResidentFlag = " & $ATTRIBUTE_HEADER_NonResidentFlag & @crlf)
	$ATTRIBUTE_HEADER_NameLength = Dec(StringMid($Entry,19,2))
;	ConsoleWrite("$ATTRIBUTE_HEADER_NameLength = " & $ATTRIBUTE_HEADER_NameLength & @crlf)
	$ATTRIBUTE_HEADER_NameRelativeOffset = StringMid($Entry,21,4)
;	ConsoleWrite("$ATTRIBUTE_HEADER_NameRelativeOffset = " & $ATTRIBUTE_HEADER_NameRelativeOffset & @crlf)
	$ATTRIBUTE_HEADER_NameRelativeOffset = Dec(_SwapEndian($ATTRIBUTE_HEADER_NameRelativeOffset))
;	ConsoleWrite("$ATTRIBUTE_HEADER_NameRelativeOffset = " & $ATTRIBUTE_HEADER_NameRelativeOffset & @crlf)
	If $ATTRIBUTE_HEADER_NameLength > 0 Then
		$ATTRIBUTE_HEADER_Name = _UnicodeHexToStr(StringMid($Entry,$ATTRIBUTE_HEADER_NameRelativeOffset*2 + 1,$ATTRIBUTE_HEADER_NameLength*4))
	Else
		$ATTRIBUTE_HEADER_Name = ""
	EndIf
	$ATTRIBUTE_HEADER_Flags = _SwapEndian(StringMid($Entry,25,4))
;	ConsoleWrite("$ATTRIBUTE_HEADER_Flags = " & $ATTRIBUTE_HEADER_Flags & @crlf)
	$Flags = ""
	If $ATTRIBUTE_HEADER_Flags = "0000" Then
		$Flags = "NORMAL"
	Else
		If BitAND($ATTRIBUTE_HEADER_Flags,"0001") Then
			$IsCompressed = 1
			$Flags &= "COMPRESSED+"
		EndIf
		If BitAND($ATTRIBUTE_HEADER_Flags,"4000") Then
			;$IsEncrypted = 1
			$Flags &= "ENCRYPTED+"
		EndIf
		If BitAND($ATTRIBUTE_HEADER_Flags,"8000") Then
			;$IsSparse = 1
			$Flags &= "SPARSE+"
		EndIf
		$Flags = StringTrimRight($Flags,1)
	EndIf
;	ConsoleWrite("File is " & $Flags & @CRLF)
	$ATTRIBUTE_HEADER_AttributeID = StringMid($Entry,29,4)
	$ATTRIBUTE_HEADER_AttributeID = StringMid($ATTRIBUTE_HEADER_AttributeID,3,2) & StringMid($ATTRIBUTE_HEADER_AttributeID,1,2)
	If $ATTRIBUTE_HEADER_NonResidentFlag = '01' Then
		$ATTRIBUTE_HEADER_StartVCN = StringMid($Entry,33,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_StartVCN = " & $ATTRIBUTE_HEADER_StartVCN & @crlf)
		$ATTRIBUTE_HEADER_StartVCN = Dec(_SwapEndian($ATTRIBUTE_HEADER_StartVCN),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_StartVCN = " & $ATTRIBUTE_HEADER_StartVCN & @crlf)
		$ATTRIBUTE_HEADER_LastVCN = StringMid($Entry,49,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LastVCN = " & $ATTRIBUTE_HEADER_LastVCN & @crlf)
		$ATTRIBUTE_HEADER_LastVCN = Dec(_SwapEndian($ATTRIBUTE_HEADER_LastVCN),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LastVCN = " & $ATTRIBUTE_HEADER_LastVCN & @crlf)
		;$ATTRIBUTE_HEADER_VCNs = $ATTRIBUTE_HEADER_LastVCN - $ATTRIBUTE_HEADER_StartVCN
;		ConsoleWrite("$ATTRIBUTE_HEADER_VCNs = " & $ATTRIBUTE_HEADER_VCNs & @crlf)
		$ATTRIBUTE_HEADER_OffsetToDataRuns = StringMid($Entry,65,4)
		$ATTRIBUTE_HEADER_OffsetToDataRuns = Dec(StringMid($ATTRIBUTE_HEADER_OffsetToDataRuns,3,1) & StringMid($ATTRIBUTE_HEADER_OffsetToDataRuns,3,1))
		;$ATTRIBUTE_HEADER_CompressionUnitSize = Dec(_SwapEndian(StringMid($Entry,69,4)))
;		ConsoleWrite("$ATTRIBUTE_HEADER_CompressionUnitSize = " & $ATTRIBUTE_HEADER_CompressionUnitSize & @crlf)
		$IsCompressed = 0
;		If $ATTRIBUTE_HEADER_CompressionUnitSize = 4 Then $IsCompressed = 1
		$ATTRIBUTE_HEADER_Padding = StringMid($Entry,73,8)
		$ATTRIBUTE_HEADER_Padding = StringMid($ATTRIBUTE_HEADER_Padding,7,2) & StringMid($ATTRIBUTE_HEADER_Padding,5,2) & StringMid($ATTRIBUTE_HEADER_Padding,3,2) & StringMid($ATTRIBUTE_HEADER_Padding,1,2)
		$ATTRIBUTE_HEADER_AllocatedSize = StringMid($Entry,81,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_AllocatedSize = " & $ATTRIBUTE_HEADER_AllocatedSize & @crlf)
		$ATTRIBUTE_HEADER_AllocatedSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_AllocatedSize),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_AllocatedSize = " & $ATTRIBUTE_HEADER_AllocatedSize & @crlf)
		$ATTRIBUTE_HEADER_RealSize = StringMid($Entry,97,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_RealSize = " & $ATTRIBUTE_HEADER_RealSize & @crlf)
		$ATTRIBUTE_HEADER_RealSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_RealSize),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_RealSize = " & $ATTRIBUTE_HEADER_RealSize & @crlf)
		$ATTRIBUTE_HEADER_InitializedStreamSize = StringMid($Entry,113,16)
;		ConsoleWrite("$ATTRIBUTE_HEADER_InitializedStreamSize = " & $ATTRIBUTE_HEADER_InitializedStreamSize & @crlf)
		$ATTRIBUTE_HEADER_InitializedStreamSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_InitializedStreamSize),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_InitializedStreamSize = " & $ATTRIBUTE_HEADER_InitializedStreamSize & @crlf)
		$RunListOffset = StringMid($Entry,65,4)
;		ConsoleWrite("$RunListOffset = " & $RunListOffset & @crlf)
		$RunListOffset = Dec(_SwapEndian($RunListOffset))
;		ConsoleWrite("$RunListOffset = " & $RunListOffset & @crlf)
		If $IsCompressed AND $RunListOffset = 72 Then
			$ATTRIBUTE_HEADER_CompressedSize = StringMid($Entry,129,16)
			$ATTRIBUTE_HEADER_CompressedSize = Dec(_SwapEndian($ATTRIBUTE_HEADER_CompressedSize),2)
		EndIf
		$DataRun = StringMid($Entry,$RunListOffset*2+1,(StringLen($Entry)-$RunListOffset)*2)
;		ConsoleWrite("$DataRun = " & $DataRun & @crlf)
	ElseIf $ATTRIBUTE_HEADER_NonResidentFlag = '00' Then
		$ATTRIBUTE_HEADER_LengthOfAttribute = StringMid($Entry,33,8)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LengthOfAttribute = " & $ATTRIBUTE_HEADER_LengthOfAttribute & @crlf)
		$ATTRIBUTE_HEADER_LengthOfAttribute = Dec(_SwapEndian($ATTRIBUTE_HEADER_LengthOfAttribute),2)
;		ConsoleWrite("$ATTRIBUTE_HEADER_LengthOfAttribute = " & $ATTRIBUTE_HEADER_LengthOfAttribute & @crlf)
;		$ATTRIBUTE_HEADER_OffsetToAttribute = StringMid($Entry,41,4)
;		$ATTRIBUTE_HEADER_OffsetToAttribute = Dec(StringMid($ATTRIBUTE_HEADER_OffsetToAttribute,3,2) & StringMid($ATTRIBUTE_HEADER_OffsetToAttribute,1,2))
		$ATTRIBUTE_HEADER_OffsetToAttribute = Dec(_SwapEndian(StringMid($Entry,41,4)))
;		ConsoleWrite("$ATTRIBUTE_HEADER_OffsetToAttribute = " & $ATTRIBUTE_HEADER_OffsetToAttribute & @crlf)
		;$ATTRIBUTE_HEADER_IndexedFlag = Dec(StringMid($Entry,45,2))
		$ATTRIBUTE_HEADER_Padding = StringMid($Entry,47,2)
		$DataRun = StringMid($Entry,$ATTRIBUTE_HEADER_OffsetToAttribute*2+1,$ATTRIBUTE_HEADER_LengthOfAttribute*2)
;		ConsoleWrite("$DataRun = " & $DataRun & @crlf)
	EndIf
	$CoreAttributeArr[1] = $ATTRIBUTE_HEADER_Name
	If $ATTRIBUTE_HEADER_Name <> "$J" Then Return $CoreAttributeArr
	If $ATTRIBUTE_HEADER_RealSize <> 0 Then $GlobUsnJrnlFileSize = $ATTRIBUTE_HEADER_RealSize

; Possible continuation
;	For $i = 1 To UBound($DataQ) - 1
	For $i = 1 To 1
;		_DecodeDataQEntry($DataQ[$i])
		If $ATTRIBUTE_HEADER_NonResidentFlag = '00' Then
;_ExtractResidentFile($DATA_Name, $DATA_LengthOfAttribute)
			$CoreAttribute = $DataRun
		Else
			Global $RUN_VCN[1], $RUN_Clusters[1]

			$TotalClusters = $ATTRIBUTE_HEADER_LastVCN - $ATTRIBUTE_HEADER_StartVCN + 1
			$Size = $ATTRIBUTE_HEADER_RealSize
;_ExtractDataRuns()
			$r=UBound($RUN_Clusters)
			$i=1
			$RUN_VCN[0] = 0
			$BaseVCN = $RUN_VCN[0]
			If $DataRun = "" Then $DataRun = "00"
			Do
				$RunListID = StringMid($DataRun,$i,2)
				If $RunListID = "00" Then ExitLoop
;				ConsoleWrite("$RunListID = " & $RunListID & @crlf)
				$i += 2
				$RunListClustersLength = Dec(StringMid($RunListID,2,1))
;				ConsoleWrite("$RunListClustersLength = " & $RunListClustersLength & @crlf)
				$RunListVCNLength = Dec(StringMid($RunListID,1,1))
;				ConsoleWrite("$RunListVCNLength = " & $RunListVCNLength & @crlf)
				$RunListClusters = Dec(_SwapEndian(StringMid($DataRun,$i,$RunListClustersLength*2)),2)
;				ConsoleWrite("$RunListClusters = " & $RunListClusters & @crlf)
				$i += $RunListClustersLength*2
				$RunListVCN = _SwapEndian(StringMid($DataRun, $i, $RunListVCNLength*2))
				;next line handles positive or negative move
				$BaseVCN += Dec($RunListVCN,2)-(($r>1) And (Dec(StringMid($RunListVCN,1,1))>7))*Dec(StringMid("10000000000000000",1,$RunListVCNLength*2+1),2)
				If $RunListVCN <> "" Then
					$RunListVCN = $BaseVCN
				Else
					$RunListVCN = 0			;$RUN_VCN[$r-1]		;0
				EndIf
;				ConsoleWrite("$RunListVCN = " & $RunListVCN & @crlf)
				If (($RunListVCN=0) And ($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
				;If (($RunListVCN=$RUN_VCN[$r-1]) And ($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
				;may be sparse section at end of Compression Signature
					_ArrayAdd($RUN_Clusters,Mod($RunListClusters,16))
					_ArrayAdd($RUN_VCN,$RunListVCN)
					$RunListClusters -= Mod($RunListClusters,16)
					$r += 1
				ElseIf (($RunListClusters>16) And (Mod($RunListClusters,16)>0)) Then
				;may be compressed data section at start of Compression Signature
					_ArrayAdd($RUN_Clusters,$RunListClusters-Mod($RunListClusters,16))
					_ArrayAdd($RUN_VCN,$RunListVCN)
					$RunListVCN += $RUN_Clusters[$r]
					$RunListClusters = Mod($RunListClusters,16)
					$r += 1
				EndIf
			;just normal or sparse data
				_ArrayAdd($RUN_Clusters,$RunListClusters)
				_ArrayAdd($RUN_VCN,$RunListVCN)
				$r += 1
				$i += $RunListVCNLength*2
			Until $i > StringLen($DataRun)
;--------------------------------_ExtractDataRuns()
;			_ArrayDisplay($RUN_Clusters,"$RUN_Clusters")
;			_ArrayDisplay($RUN_VCN,"$RUN_VCN")
;			ConsoleWrite("$Size: " & $Size & @CRLF)
;			ConsoleWrite("$TotalClusters * $BytesPerCluster: " & $TotalClusters * $BytesPerCluster & @CRLF)
;			ConsoleWrite("$GlobUsnJrnlFileSize 1: " & $GlobUsnJrnlFileSize & @CRLF)
			If $TotalClusters * $BytesPerCluster >= $Size Then
;				ConsoleWrite(_ArrayToString($RUN_VCN) & @CRLF)
;				ConsoleWrite(_ArrayToString($RUN_Clusters) & @CRLF)
;ExtractFile
				;Local $nBytes
				$hFile = _WinAPI_CreateFile($TargetDevice, 2, 6, 6)
				If $hFile = 0 Then
					ConsoleWrite("Error CreateFile in function _GetAttributeEntryNoRead()" & @CRLF)
					_WinAPI_CloseHandle($hFile)
					Return
				EndIf
				$tBuffer = DllStructCreate("byte[" & $BytesPerCluster * 16 & "]")
				Select
					Case UBound($RUN_VCN) = 1		;no data, do nothing
					Case (UBound($RUN_VCN) = 2) Or (Not $IsCompressed)	;may be normal or sparse
						If $ATTRIBUTE_HEADER_RealSize <> 0 Then
							$FileSize = $ATTRIBUTE_HEADER_RealSize
						Else
							$FileSize = $GlobUsnJrnlFileSize
						EndIf
						Local $TestArray[UBound($RUN_VCN)][4]
						$TestArray[0][0] = "Offset"
						$TestArray[0][1] = "Bytes Accumulated"
						$TestArray[0][2] = "Bytes per Run"
						$TestArray[0][3] = "Sectors per Run"
						For $s = 1 To UBound($RUN_VCN)-1
							If $RUN_VCN[$s] = 0 Then
								$TestArray[$s][0] = 0
								$GlobUsnJrnlSparseBytes += $BytesPerCluster * $RUN_Clusters[$s]
;								$Bytes += $BytesPerCluster * $RUN_Clusters[$s]
								$TestArray[$s][1] = $BytesPerCluster * $RUN_Clusters[$s]
								$FileSize -= $BytesPerCluster * $RUN_Clusters[$s]
								Continueloop
							EndIf
							$TestArray[$s][0] = $RUN_VCN[$s]*$BytesPerCluster
							$g = $RUN_Clusters[$s]
							While $g > 16 And $FileSize > $BytesPerCluster * 16
								$Bytes += $BytesPerCluster * 16
								$g -= 16
								$FileSize -= $BytesPerCluster * 16
							WEnd
;							If $g = 0 Or $FileSize = 0 Then ExitLoop ;Return $FileSize
;							If $g > 16 Then $g = 16
							If $g <> 0 Then
								If $FileSize > $BytesPerCluster * $g Then
									$Bytes += $BytesPerCluster * $g
									$FileSize -= $BytesPerCluster * $g
								Else
									$Bytes += $FileSize
								EndIf
;								$Bytes += $BytesPerCluster * $g
							EndIf
							$TestArray[$s][1] = $Bytes
						Next
						$GlobUsnJrnlFileSize = $FileSize
					Case Else					;may be compressed
;						_DoCompressed($hFile, $htest, $tBuffer)
						ConsoleWrite("Error: Compressed attributes not supported!!!" & @CRLF)
				EndSelect
;------------------------ExtractFile
			Else
				$GlobUsnJrnlFileSize -= $TotalClusters * $BytesPerCluster
				$GlobUsnJrnlSparseBytes += $TotalClusters * $BytesPerCluster
			EndIf
;-------------------------
		EndIf
	Next
;	ConsoleWrite("$GlobUsnJrnlFileSize 2: " & $GlobUsnJrnlFileSize & @CRLF)
;	ConsoleWrite("$GlobUsnJrnlSparseBytes: " & $GlobUsnJrnlSparseBytes & @CRLF)
	$CoreAttributeArr[0] = $CoreAttribute
	$CoreAttributeArr[1] = $ATTRIBUTE_HEADER_Name

	$RawTestOffsetArray = $TestArray
;	_ArrayDisplay($TestArray,"$TestArray")

	For $i = 1 To UBound($RawTestOffsetArray)-1
		If $RawTestOffsetArray[$i][0] = 0 Then ContinueLoop
;		$RawTestOffsetArray[$i-1][0] = 0 Then $FirstRealRun = $i
;		If $i = 1 Then
		If $RawTestOffsetArray[$i-1][0] = 0 Then
			$RawTestOffsetArray[$i][2] = $RawTestOffsetArray[$i][1]
		Else
			$RawTestOffsetArray[$i][2] = $RawTestOffsetArray[$i][1] - $RawTestOffsetArray[$i-1][1]
		EndIf
		$RawTestOffsetArray[$i][3] = $RawTestOffsetArray[$i][2]/512
	Next
;	_ArrayDisplay($RawTestOffsetArray,"$RawTestOffsetArray")
	Return $CoreAttributeArr
EndFunc

Func _Get_IndexRoot($Entry,$Current_Attrib_Number,$CurrentAttributeName)
	Local $LocalAttributeOffset = 1,$AttributeType,$CollationRule,$SizeOfIndexAllocationEntry,$ClustersPerIndexRoot
	$AttributeType = StringMid($Entry,$LocalAttributeOffset,8)
;	$AttributeType = _SwapEndian($AttributeType)
	$CollationRule = StringMid($Entry,$LocalAttributeOffset+8,8)
	$CollationRule = _SwapEndian($CollationRule)
	$SizeOfIndexAllocationEntry = StringMid($Entry,$LocalAttributeOffset+16,8)
	$SizeOfIndexAllocationEntry = Dec(_SwapEndian($SizeOfIndexAllocationEntry),2)
	$ClustersPerIndexRoot = Dec(StringMid($Entry,$LocalAttributeOffset+24,2))
;	$IRPadding = StringMid($Entry,$LocalAttributeOffset+26,6)
	$OffsetToFirstEntry = StringMid($Entry,$LocalAttributeOffset+32,8)
	$OffsetToFirstEntry = Dec(_SwapEndian($OffsetToFirstEntry),2)
	$TotalSizeOfEntries = StringMid($Entry,$LocalAttributeOffset+40,8)
	$TotalSizeOfEntries = Dec(_SwapEndian($TotalSizeOfEntries),2)
	$AllocatedSizeOfEntries = StringMid($Entry,$LocalAttributeOffset+48,8)
	$AllocatedSizeOfEntries = Dec(_SwapEndian($AllocatedSizeOfEntries),2)
	$Flags = StringMid($Entry,$LocalAttributeOffset+56,2)
	If $Flags = "01" Then
		$Flags = "01 (Index Allocation needed)"
		$ResidentIndx = 0
	Else
		$Flags = "00 (Fits in Index Root)"
		$ResidentIndx = 1
	EndIf
;	$IRPadding2 = StringMid($Entry,$LocalAttributeOffset+58,6)
	$IRArr[0][$Current_Attrib_Number] = "IndexRoot Number " & $Current_Attrib_Number
	$IRArr[1][$Current_Attrib_Number] = $CurrentAttributeName
	$IRArr[2][$Current_Attrib_Number] = $AttributeType
	$IRArr[3][$Current_Attrib_Number] = $CollationRule
	$IRArr[4][$Current_Attrib_Number] = $SizeOfIndexAllocationEntry
	$IRArr[5][$Current_Attrib_Number] = $ClustersPerIndexRoot
;	$IRArr[6][$Current_Attrib_Number] = $IRPadding
	$IRArr[7][$Current_Attrib_Number] = $OffsetToFirstEntry
	$IRArr[8][$Current_Attrib_Number] = $TotalSizeOfEntries
	$IRArr[9][$Current_Attrib_Number] = $AllocatedSizeOfEntries
	$IRArr[10][$Current_Attrib_Number] = $Flags
;	$IRArr[11][$Current_Attrib_Number] = $IRPadding2
	If $ResidentIndx And $AttributeType=$FILE_NAME Then
		$TheResidentIndexEntry = StringMid($Entry,$LocalAttributeOffset+64)
		_DecodeIndxEntries($TheResidentIndexEntry)
	EndIf
EndFunc

; start: by Ascend4nt -----------------------------
Func _WinTime_GetUTCToLocalFileTimeDelta()
	Local $iUTCFileTime=864000000000		; exactly 24 hours from the origin (although 12 hours would be more appropriate (max variance = 12))
	$iLocalFileTime=_WinTime_UTCFileTimeToLocalFileTime($iUTCFileTime)
	If @error Then Return SetError(@error,@extended,-1)
	Return $iLocalFileTime-$iUTCFileTime	; /36000000000 = # hours delta (effectively giving the offset in hours from UTC/GMT)
EndFunc

Func _WinTime_UTCFileTimeToLocalFileTime($iUTCFileTime)
	If $iUTCFileTime<0 Then Return SetError(1,0,-1)
	Local $aRet=DllCall($_COMMON_KERNEL32DLL,"bool","FileTimeToLocalFileTime","uint64*",$iUTCFileTime,"uint64*",0)
	If @error Then Return SetError(2,@error,-1)
	If Not $aRet[0] Then Return SetError(3,0,-1)
	Return $aRet[2]
EndFunc

Func _WinTime_UTCFileTimeFormat($iUTCFileTime,$iFormat=4,$iPrecision=0,$bAMPMConversion=False)
;~ 	If $iUTCFileTime<0 Then Return SetError(1,0,"")	; checked in below call

	; First convert file time (UTC-based file time) to 'local file time'
	Local $iLocalFileTime=_WinTime_UTCFileTimeToLocalFileTime($iUTCFileTime)
	If @error Then Return SetError(@error,@extended,"")
	; Rare occassion: a filetime near the origin (January 1, 1601!!) is used,
	;	causing a negative result (for some timezones). Return as invalid param.
	If $iLocalFileTime<0 Then Return SetError(1,0,"")

	; Then convert file time to a system time array & format & return it
	Local $vReturn=_WinTime_LocalFileTimeFormat($iLocalFileTime,$iFormat,$iPrecision,$bAMPMConversion)
	Return SetError(@error,@extended,$vReturn)
EndFunc

Func _WinTime_LocalFileTimeFormat($iLocalFileTime,$iFormat=4,$iPrecision=0,$bAMPMConversion=False)
;~ 	If $iLocalFileTime<0 Then Return SetError(1,0,"")	; checked in below call

	; Convert file time to a system time array & return result
	Local $aSysTime=_WinTime_LocalFileTimeToSystemTime($iLocalFileTime)
	If @error Then Return SetError(@error,@extended,"")

	; Return only the SystemTime array?
	If $iFormat=0 Then Return $aSysTime

	Local $vReturn=_WinTime_FormatTime($aSysTime[0],$aSysTime[1],$aSysTime[2],$aSysTime[3], _
		$aSysTime[4],$aSysTime[5],$aSysTime[6],$aSysTime[7],$iFormat,$iPrecision,$bAMPMConversion)
	Return SetError(@error,@extended,$vReturn)
EndFunc

Func _WinTime_LocalFileTimeToSystemTime($iLocalFileTime)
	Local $aRet,$stSysTime,$aSysTime[8]=[-1,-1,-1,-1,-1,-1,-1,-1]

	; Negative values unacceptable
	If $iLocalFileTime<0 Then Return SetError(1,0,$aSysTime)

	; SYSTEMTIME structure [Year,Month,DayOfWeek,Day,Hour,Min,Sec,Milliseconds]
	$stSysTime=DllStructCreate("ushort[8]")

	$aRet=DllCall($_COMMON_KERNEL32DLL,"bool","FileTimeToSystemTime","uint64*",$iLocalFileTime,"ptr",DllStructGetPtr($stSysTime))
	If @error Then Return SetError(2,@error,$aSysTime)
	If Not $aRet[0] Then Return SetError(3,0,$aSysTime)
	Dim $aSysTime[8]=[DllStructGetData($stSysTime,1,1),DllStructGetData($stSysTime,1,2),DllStructGetData($stSysTime,1,4),DllStructGetData($stSysTime,1,5), _
		DllStructGetData($stSysTime,1,6),DllStructGetData($stSysTime,1,7),DllStructGetData($stSysTime,1,8),DllStructGetData($stSysTime,1,3)]
	Return $aSysTime
EndFunc

Func _WinTime_FormatTime($iYear,$iMonth,$iDay,$iHour,$iMin,$iSec,$iMilSec,$iDayOfWeek,$iFormat=4,$iPrecision=0,$bAMPMConversion=False)
	Local Static $_WT_aMonths[12]=["January","February","March","April","May","June","July","August","September","October","November","December"]
	Local Static $_WT_aDays[7]=["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

	If Not $iFormat Or $iMonth<1 Or $iMonth>12 Or $iDayOfWeek>6 Then Return SetError(1,0,"")

	; Pad MM,DD,HH,MM,SS,MSMSMSMS as necessary
	Local $sMM=StringRight(0&$iMonth,2),$sDD=StringRight(0&$iDay,2),$sMin=StringRight(0&$iMin,2)
	; $sYY = $iYear	; (no padding)
	;	[technically Year can be 1-x chars - but this is generally used for 4-digit years. And SystemTime only goes up to 30827/30828]
	Local $sHH,$sSS,$sMS,$sAMPM

	; 'Extra precision 1': +SS (Seconds)
	If $iPrecision Then
		$sSS=StringRight(0&$iSec,2)
		; 'Extra precision 2': +MSMSMSMS (Milliseconds)
		If $iPrecision>1 Then
;			$sMS=StringRight('000'&$iMilSec,4)
			$sMS=StringRight('000'&$iMilSec,3);Fixed an erronous 0 in front of the milliseconds
		Else
			$sMS=""
		EndIf
	Else
		$sSS=""
		$sMS=""
	EndIf
	If $bAMPMConversion Then
		If $iHour>11 Then
			$sAMPM=" PM"
			; 12 PM will cause 12-12 to equal 0, so avoid the calculation:
			If $iHour=12 Then
				$sHH="12"
			Else
				$sHH=StringRight(0&($iHour-12),2)
			EndIf
		Else
			$sAMPM=" AM"
			If $iHour Then
				$sHH=StringRight(0&$iHour,2)
			Else
			; 00 military = 12 AM
				$sHH="12"
			EndIf
		EndIf
	Else
		$sAMPM=""
		$sHH=StringRight(0 & $iHour,2)
	EndIf

	Local $sDateTimeStr,$aReturnArray[3]

	; Return an array? [formatted string + "Month" + "DayOfWeek"]
	If BitAND($iFormat,0x10) Then
		$aReturnArray[1]=$_WT_aMonths[$iMonth-1]
		If $iDayOfWeek>=0 Then
			$aReturnArray[2]=$_WT_aDays[$iDayOfWeek]
		Else
			$aReturnArray[2]=""
		EndIf
		; Strip the 'array' bit off (array[1] will now indicate if an array is to be returned)
		$iFormat=BitAND($iFormat,0xF)
	Else
		; Signal to below that the array isn't to be returned
		$aReturnArray[1]=""
	EndIf

	; Prefix with "DayOfWeek "?
	If BitAND($iFormat,8) Then
		If $iDayOfWeek<0 Then Return SetError(1,0,"")	; invalid
		$sDateTimeStr=$_WT_aDays[$iDayOfWeek]&', '
		; Strip the 'DayOfWeek' bit off
		$iFormat=BitAND($iFormat,0x7)
	Else
		$sDateTimeStr=""
	EndIf

	If $iFormat<2 Then
		; Basic String format: YYYYMMDDHHMM[SS[MSMSMSMS[ AM/PM]]]
		$sDateTimeStr&=$iYear&$sMM&$sDD&$sHH&$sMin&$sSS&$sMS&$sAMPM
	Else
		; one of 4 formats which ends with " HH:MM[:SS[:MSMSMSMS[ AM/PM]]]"
		Switch $iFormat
			; /, : Format - MM/DD/YYYY
			Case 2
				$sDateTimeStr&=$sMM&'/'&$sDD&'/'
			; /, : alt. Format - DD/MM/YYYY
			Case 3
				$sDateTimeStr&=$sDD&'/'&$sMM&'/'
			; "Month DD, YYYY" format
			Case 4
				$sDateTimeStr&=$_WT_aMonths[$iMonth-1]&' '&$sDD&', '
			; "DD Month YYYY" format
			Case 5
				$sDateTimeStr&=$sDD&' '&$_WT_aMonths[$iMonth-1]&' '
			Case 6
				$sDateTimeStr&=$iYear&'-'&$sMM&'-'&$sDD
				$iYear=''
			Case Else
				Return SetError(1,0,"")
		EndSwitch
		$sDateTimeStr&=$iYear&' '&$sHH&':'&$sMin
		If $iPrecision Then
			$sDateTimeStr&=':'&$sSS
			If $iPrecision>1 Then $sDateTimeStr&=':'&$sMS
		EndIf
		$sDateTimeStr&=$sAMPM
	EndIf
	If $aReturnArray[1]<>"" Then
		$aReturnArray[0]=$sDateTimeStr
		Return $aReturnArray
	EndIf
	Return $sDateTimeStr
EndFunc

Func _WinTime_SystemTimeToLocalFileTime($iYear,$iMonth,$iDay,$iHour,$iMin,$iSec,$iMilSec,$iDayOfWeek=-1)
	; Least\Greatest year check
	If $iYear<1601 Or $iYear>30827 Then Return SetError(1,0,-1)
	; SYSTEMTIME structure [Year,Month,DayOfWeek,Day,Hour,Min,Sec,Milliseconds]
	Local $stSysTime=DllStructCreate("ushort[8]")
	DllStructSetData($stSysTime,1,$iYear,1)
	DllStructSetData($stSysTime,1,$iMonth,2)
	DllStructSetData($stSysTime,1,$iDayOfWeek,3)
	DllStructSetData($stSysTime,1,$iDay,4)
	DllStructSetData($stSysTime,1,$iHour,5)
	DllStructSetData($stSysTime,1,$iMin,6)
	DllStructSetData($stSysTime,1,$iSec,7)
	DllStructSetData($stSysTime,1,$iMilSec,8)
	Local $aRet=DllCall($_COMMON_KERNEL32DLL,"bool","SystemTimeToFileTime","ptr",DllStructGetPtr($stSysTime),"int64*",0)
	If @error Then Return SetError(2,@error,-1)
	If Not $aRet[0] Then Return SetError(3,0,-1)
	Return $aRet[2]
EndFunc
; end: by Ascend4nt ----------------------------

Func _Get_IndexAllocation(ByRef $Entry)
;	ConsoleWrite("Starting function _Get_IndexAllocation()" & @crlf)
	Local $NextPosition = 1,$IndxHdrMagic,$IndxEntries,$TotalIndxEntries
;	ConsoleWrite("INDX record:" & @crlf)
;	ConsoleWrite(_HexEncode("0x"& StringMid($Entry,1)) & @crlf)
;	ConsoleWrite("StringLen of chunk = " & StringLen($Entry) & @crlf)
;	ConsoleWrite("Expected records = " & StringLen($Entry)/8192 & @crlf)
	$NextPosition = 1
	Do
		$IndxHdrMagic = StringMid($Entry,$NextPosition,8)
;		ConsoleWrite("$IndxHdrMagic = " & $IndxHdrMagic & @crlf)
		$IndxHdrMagic = _HexToString($IndxHdrMagic)
;		ConsoleWrite("$IndxHdrMagic = " & $IndxHdrMagic & @crlf)
		If $IndxHdrMagic <> "INDX" Then
;			ConsoleWrite("$IndxHdrMagic: " & $IndxHdrMagic & @crlf)
;			ConsoleWrite("Error: Record is not of type INDX, and this was not expected.." & @crlf)
			$NextPosition += 8192
			ContinueLoop
		EndIf
		$IndxEntries = _StripIndxRecord(StringMid($Entry,$NextPosition,8192))
		$TotalIndxEntries &= $IndxEntries
		$NextPosition += 8192
;		ConsoleWrite("$NextPosition: " & $NextPosition & @crlf)
	Until $NextPosition >= StringLen($Entry)+32
;	ConsoleWrite("INDX record:" & @crlf)
;	ConsoleWrite(_HexEncode("0x"& StringMid($Entry,1)) & @crlf)
;	ConsoleWrite("Total chunk of stripped INDX entries:" & @crlf)
;	ConsoleWrite(_HexEncode("0x"& StringMid($TotalIndxEntries,1)) & @crlf)
	_DecodeIndxEntries($TotalIndxEntries)
EndFunc

Func _DecodeIndxEntries(ByRef $Entry)
;	ConsoleWrite("Starting function _DecodeIndxEntries()" & @crlf)
	;Local $LocalAttributeOffset = 1,$NewLocalAttributeOffset,$IndxHdrMagic,$IndxHdrUpdateSeqArrOffset,$IndxHdrUpdateSeqArrSize,$IndxHdrLogFileSequenceNo,$IndxHdrVCNOfIndx,$IndxHdrOffsetToIndexEntries,$IndxHdrSizeOfIndexEntries,$IndxHdrAllocatedSizeOfIndexEntries
	;Local $IndxHdrFlag,$IndxHdrPadding,$IndxHdrUpdateSequence,$IndxHdrUpdSeqArr,$IndxHdrUpdSeqArrPart0,$IndxHdrUpdSeqArrPart1,$IndxHdrUpdSeqArrPart2,$IndxHdrUpdSeqArrPart3,$IndxRecordEnd4,$IndxRecordEnd1,$IndxRecordEnd2,$IndxRecordEnd3,$IndxRecordEnd4
	Local $tmp1=0,$EntryCounter=1,$NextEntryOffset
	Local $NewLocalAttributeOffset = 1
	$MFTReference = StringMid($Entry,$NewLocalAttributeOffset,12)
	$MFTReference = _SwapEndian($MFTReference)
	$MFTReference = Dec($MFTReference,2)
	$IndexFlags = StringMid($Entry,$NewLocalAttributeOffset+24,4)
	#cs
	$MFTReferenceSeqNo = StringMid($Entry,$NewLocalAttributeOffset+12,4)
	$MFTReferenceSeqNo = Dec(StringMid($MFTReferenceSeqNo,3,2)&StringMid($MFTReferenceSeqNo,1,2))
	$IndexEntryLength = StringMid($Entry,$NewLocalAttributeOffset+16,4)
	$IndexEntryLength = Dec(StringMid($IndexEntryLength,3,2)&StringMid($IndexEntryLength,3,2))
	$OffsetToFileName = StringMid($Entry,$NewLocalAttributeOffset+20,4)
	$OffsetToFileName = Dec(StringMid($OffsetToFileName,3,2)&StringMid($OffsetToFileName,3,2))
	$IndexFlags = StringMid($Entry,$NewLocalAttributeOffset+24,4)
;	$Padding = StringMid($Entry,$NewLocalAttributeOffset+28,4)
	$MFTReferenceOfParent = StringMid($Entry,$NewLocalAttributeOffset+32,12)
	$MFTReferenceOfParent = StringMid($MFTReferenceOfParent,7,2)&StringMid($MFTReferenceOfParent,5,2)&StringMid($MFTReferenceOfParent,3,2)&StringMid($MFTReferenceOfParent,1,2)
	$MFTReferenceOfParent = Dec($MFTReferenceOfParent)
	$MFTReferenceOfParentSeqNo = StringMid($Entry,$NewLocalAttributeOffset+44,4)
	$MFTReferenceOfParentSeqNo = Dec(StringMid($MFTReferenceOfParentSeqNo,3,2) & StringMid($MFTReferenceOfParentSeqNo,3,2))
	$Indx_CTime = StringMid($Entry,$NewLocalAttributeOffset+48,16)
	$Indx_CTime = StringMid($Indx_CTime,15,2) & StringMid($Indx_CTime,13,2) & StringMid($Indx_CTime,11,2) & StringMid($Indx_CTime,9,2) & StringMid($Indx_CTime,7,2) & StringMid($Indx_CTime,5,2) & StringMid($Indx_CTime,3,2) & StringMid($Indx_CTime,1,2)
	$Indx_CTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_CTime)
	$Indx_CTime = _WinTime_UTCFileTimeFormat(Dec($Indx_CTime)-$tDelta,$DateTimeFormat,2)
	If @error Then
		$Indx_CTime = "-"
	Else
		$Indx_CTime = $Indx_CTime & ":" & _FillZero(StringRight($Indx_CTime_tmp,4))
	EndIf
	$Indx_ATime = StringMid($Entry,$NewLocalAttributeOffset+64,16)
	$Indx_ATime = StringMid($Indx_ATime,15,2) & StringMid($Indx_ATime,13,2) & StringMid($Indx_ATime,11,2) & StringMid($Indx_ATime,9,2) & StringMid($Indx_ATime,7,2) & StringMid($Indx_ATime,5,2) & StringMid($Indx_ATime,3,2) & StringMid($Indx_ATime,1,2)
	$Indx_ATime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_ATime)
	$Indx_ATime = _WinTime_UTCFileTimeFormat(Dec($Indx_ATime)-$tDelta,$DateTimeFormat,2)
	If @error Then
		$Indx_ATime = "-"
	Else
		$Indx_ATime = $Indx_ATime & ":" & _FillZero(StringRight($Indx_ATime_tmp,4))
	EndIf
	$Indx_MTime = StringMid($Entry,$NewLocalAttributeOffset+80,16)
	$Indx_MTime = StringMid($Indx_MTime,15,2) & StringMid($Indx_MTime,13,2) & StringMid($Indx_MTime,11,2) & StringMid($Indx_MTime,9,2) & StringMid($Indx_MTime,7,2) & StringMid($Indx_MTime,5,2) & StringMid($Indx_MTime,3,2) & StringMid($Indx_MTime,1,2)
	$Indx_MTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_MTime)
	$Indx_MTime = _WinTime_UTCFileTimeFormat(Dec($Indx_MTime)-$tDelta,$DateTimeFormat,2)
	If @error Then
		$Indx_MTime = "-"
	Else
		$Indx_MTime = $Indx_MTime & ":" & _FillZero(StringRight($Indx_MTime_tmp,4))
	EndIf
	$Indx_RTime = StringMid($Entry,$NewLocalAttributeOffset+96,16)
	$Indx_RTime = StringMid($Indx_RTime,15,2) & StringMid($Indx_RTime,13,2) & StringMid($Indx_RTime,11,2) & StringMid($Indx_RTime,9,2) & StringMid($Indx_RTime,7,2) & StringMid($Indx_RTime,5,2) & StringMid($Indx_RTime,3,2) & StringMid($Indx_RTime,1,2)
	$Indx_RTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_RTime)
	$Indx_RTime = _WinTime_UTCFileTimeFormat(Dec($Indx_RTime)-$tDelta,$DateTimeFormat,2)
	If @error Then
		$Indx_RTime = "-"
	Else
		$Indx_RTime = $Indx_RTime & ":" & _FillZero(StringRight($Indx_RTime_tmp,4))
	EndIf

	#ce
	#cs
	$Indx_AllocSize = StringMid($Entry,$NewLocalAttributeOffset+112,16)
	$Indx_AllocSize = Dec(StringMid($Indx_AllocSize,15,2) & StringMid($Indx_AllocSize,13,2) & StringMid($Indx_AllocSize,11,2) & StringMid($Indx_AllocSize,9,2) & StringMid($Indx_AllocSize,7,2) & StringMid($Indx_AllocSize,5,2) & StringMid($Indx_AllocSize,3,2) & StringMid($Indx_AllocSize,1,2))
	$Indx_RealSize = StringMid($Entry,$NewLocalAttributeOffset+128,16)
	$Indx_RealSize = Dec(StringMid($Indx_RealSize,15,2) & StringMid($Indx_RealSize,13,2) & StringMid($Indx_RealSize,11,2) & StringMid($Indx_RealSize,9,2) & StringMid($Indx_RealSize,7,2) & StringMid($Indx_RealSize,5,2) & StringMid($Indx_RealSize,3,2) & StringMid($Indx_RealSize,1,2))

	$Indx_File_Flags = StringMid($Entry,$NewLocalAttributeOffset+144,16)
	$Indx_File_Flags = StringMid($Indx_File_Flags,15,2) & StringMid($Indx_File_Flags,13,2) & StringMid($Indx_File_Flags,11,2) & StringMid($Indx_File_Flags,9,2)&StringMid($Indx_File_Flags,7,2) & StringMid($Indx_File_Flags,5,2) & StringMid($Indx_File_Flags,3,2) & StringMid($Indx_File_Flags,1,2)
	$Indx_File_Flags = StringMid($Indx_File_Flags,13,8)
	$Indx_File_Flags = _File_Attributes("0x" & $Indx_File_Flags)
	#ce
	$Indx_NameLength = StringMid($Entry,$NewLocalAttributeOffset+160,2)
	$Indx_NameLength = Dec($Indx_NameLength)
	$Indx_NameSpace = StringMid($Entry,$NewLocalAttributeOffset+162,2)
	Select
		Case $Indx_NameSpace = "00"	;POSIX
			$Indx_NameSpace = "POSIX"
		Case $Indx_NameSpace = "01"	;WIN32
			$Indx_NameSpace = "WIN32"
		Case $Indx_NameSpace = "02"	;DOS
			$Indx_NameSpace = "DOS"
		Case $Indx_NameSpace = "03"	;DOS+WIN32
			$Indx_NameSpace = "DOS+WIN32"
	EndSelect
	$Indx_FileName = StringMid($Entry,$NewLocalAttributeOffset+164,$Indx_NameLength*4)
;	$Indx_FileName = _UnicodeHexToStr($Indx_FileName)
	$Indx_FileName = BinaryToString("0x"&$Indx_FileName,2)
;	ConsoleWrite("$Indx_FileName: " & $Indx_FileName & @crlf)
	$tmp1 = 164+($Indx_NameLength*4)
	#cs
	Do ; Calculate the length of the padding - 8 byte aligned
		$tmp2 = $tmp1/16
		If Not IsInt($tmp2) Then
			$tmp0 = 2
			$tmp1 += $tmp0
			$tmp3 += $tmp0
		EndIf
	Until IsInt($tmp2)
	$PaddingLength = $tmp3
	#ce
	$PaddingLength = 0
	If Mod($tmp1,16) Then
		While 1
			$PaddingLength+=1
			$tmp1 += 1
			If Mod($tmp1,16) = 0 Then ExitLoop
		WEnd
	EndIf


;	$Padding2 = StringMid($Entry,$NewLocalAttributeOffset+164+($Indx_NameLength*2*2),$PaddingLength)
	If $IndexFlags <> "0000" Then
;		$SubNodeVCN = StringMid($Entry,$NewLocalAttributeOffset+164+($Indx_NameLength*2*2)+$PaddingLength,16)
		$SubNodeVCNLength = 16
	Else
;		$SubNodeVCN = ""
		$SubNodeVCNLength = 0
	EndIf
	ReDim $IndxEntryNumberArr[1+$EntryCounter]
	ReDim $IndxMFTReferenceArr[1+$EntryCounter]
;	ReDim $IndxMFTRefSeqNoArr[1+$EntryCounter]
;	ReDim $IndxIndexFlagsArr[1+$EntryCounter]
;	ReDim $IndxMFTReferenceOfParentArr[1+$EntryCounter]
;	ReDim $IndxMFTParentRefSeqNoArr[1+$EntryCounter]
;	ReDim $IndxCTimeArr[1+$EntryCounter]
;	ReDim $IndxATimeArr[1+$EntryCounter]
;	ReDim $IndxMTimeArr[1+$EntryCounter]
;	ReDim $IndxRTimeArr[1+$EntryCounter]
;	ReDim $IndxAllocSizeArr[1+$EntryCounter]
;	ReDim $IndxRealSizeArr[1+$EntryCounter]
;	ReDim $IndxFileFlagsArr[1+$EntryCounter]
	ReDim $IndxFileNameArr[1+$EntryCounter]
;	ReDim $IndxNameSpaceArr[1+$EntryCounter]
;	ReDim $IndxSubNodeVCNArr[1+$EntryCounter]
	$IndxEntryNumberArr[$EntryCounter] = $EntryCounter
	$IndxMFTReferenceArr[$EntryCounter] = $MFTReference
;	$IndxMFTRefSeqNoArr[$EntryCounter] = $MFTReferenceSeqNo
;	$IndxIndexFlagsArr[$EntryCounter] = $IndexFlags
;	$IndxMFTReferenceOfParentArr[$EntryCounter] = $MFTReferenceOfParent
;	$IndxMFTParentRefSeqNoArr[$EntryCounter] = $MFTReferenceOfParentSeqNo
;	$IndxCTimeArr[$EntryCounter] = $Indx_CTime
;	$IndxATimeArr[$EntryCounter] = $Indx_ATime
;	$IndxMTimeArr[$EntryCounter] = $Indx_MTime
;	$IndxRTimeArr[$EntryCounter] = $Indx_RTime
;	$IndxAllocSizeArr[$EntryCounter] = $Indx_AllocSize
;	$IndxRealSizeArr[$EntryCounter] = $Indx_RealSize
;	$IndxFileFlagsArr[$EntryCounter] = $Indx_File_Flags
	$IndxFileNameArr[$EntryCounter] = $Indx_FileName
;	$IndxNameSpaceArr[$EntryCounter] = $Indx_NameSpace
;	$IndxSubNodeVCNArr[$EntryCounter] = $SubNodeVCN
; Work through the rest of the index entries
	$NextEntryOffset = $NewLocalAttributeOffset+164+($Indx_NameLength*2*2)+$PaddingLength+$SubNodeVCNLength
	If $NextEntryOffset+64 >= StringLen($Entry) Then Return
	Do
		$EntryCounter += 1
;		If Mod($EntryCounter,1000) = 0 Then ConsoleWrite($EntryCounter & @crlf)
;		ConsoleWrite("$EntryCounter = " & $EntryCounter & @crlf)
		$MFTReference = StringMid($Entry,$NextEntryOffset,12)
;		ConsoleWrite("$MFTReference = " & $MFTReference & @crlf)
;		$MFTReference = StringMid($MFTReference,7,2)&StringMid($MFTReference,5,2)&StringMid($MFTReference,3,2)&StringMid($MFTReference,1,2)
		$MFTReference = _SwapEndian($MFTReference)
;		$MFTReference = StringMid($MFTReference,15,2)&StringMid($MFTReference,13,2)&StringMid($MFTReference,11,2)&StringMid($MFTReference,9,2)&StringMid($MFTReference,7,2)&StringMid($MFTReference,5,2)&StringMid($MFTReference,3,2)&StringMid($MFTReference,1,2)
;		ConsoleWrite("$MFTReference = " & $MFTReference & @crlf)
		$MFTReference = Dec($MFTReference,2)
		$IndexFlags = StringMid($Entry,$NextEntryOffset+24,4)
		#cs
		$MFTReferenceSeqNo = StringMid($Entry,$NextEntryOffset+12,4)
		$MFTReferenceSeqNo = Dec(StringMid($MFTReferenceSeqNo,3,2)&StringMid($MFTReferenceSeqNo,1,2))
		$IndexEntryLength = StringMid($Entry,$NextEntryOffset+16,4)
;		ConsoleWrite("$IndexEntryLength = " & $IndexEntryLength & @crlf)
		$IndexEntryLength = Dec(StringMid($IndexEntryLength,3,2)&StringMid($IndexEntryLength,3,2))
;		ConsoleWrite("$IndexEntryLength = " & $IndexEntryLength & @crlf)
		$OffsetToFileName = StringMid($Entry,$NextEntryOffset+20,4)
;		ConsoleWrite("$OffsetToFileName = " & $OffsetToFileName & @crlf)
		$OffsetToFileName = Dec(StringMid($OffsetToFileName,3,2)&StringMid($OffsetToFileName,3,2))
;		ConsoleWrite("$OffsetToFileName = " & $OffsetToFileName & @crlf)
		$IndexFlags = StringMid($Entry,$NextEntryOffset+24,4)
;		ConsoleWrite("$IndexFlags = " & $IndexFlags & @crlf)
;		$Padding = StringMid($Entry,$NextEntryOffset+28,4)
;		ConsoleWrite("$Padding = " & $Padding & @crlf)
		$MFTReferenceOfParent = StringMid($Entry,$NextEntryOffset+32,12)
;		ConsoleWrite("$MFTReferenceOfParent = " & $MFTReferenceOfParent & @crlf)
		$MFTReferenceOfParent = StringMid($MFTReferenceOfParent,7,2)&StringMid($MFTReferenceOfParent,5,2)&StringMid($MFTReferenceOfParent,3,2)&StringMid($MFTReferenceOfParent,1,2)
;		$MFTReferenceOfParent = StringMid($MFTReferenceOfParent,15,2)&StringMid($MFTReferenceOfParent,13,2)&StringMid($MFTReferenceOfParent,11,2)&StringMid($MFTReferenceOfParent,9,2)&StringMid($MFTReferenceOfParent,7,2)&StringMid($MFTReferenceOfParent,5,2)&StringMid($MFTReferenceOfParent,3,2)&StringMid($MFTReferenceOfParent,1,2)
;		ConsoleWrite("$MFTReferenceOfParent = " & $MFTReferenceOfParent & @crlf)
		$MFTReferenceOfParent = Dec($MFTReferenceOfParent)
		$MFTReferenceOfParentSeqNo = StringMid($Entry,$NextEntryOffset+44,4)
		$MFTReferenceOfParentSeqNo = Dec(StringMid($MFTReferenceOfParentSeqNo,3,2) & StringMid($MFTReferenceOfParentSeqNo,3,2))

		$Indx_CTime = StringMid($Entry,$NextEntryOffset+48,16)
		$Indx_CTime = StringMid($Indx_CTime,15,2) & StringMid($Indx_CTime,13,2) & StringMid($Indx_CTime,11,2) & StringMid($Indx_CTime,9,2) & StringMid($Indx_CTime,7,2) & StringMid($Indx_CTime,5,2) & StringMid($Indx_CTime,3,2) & StringMid($Indx_CTime,1,2)
		$Indx_CTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_CTime)
		$Indx_CTime = _WinTime_UTCFileTimeFormat(Dec($Indx_CTime)-$tDelta,$DateTimeFormat,2)
		$Indx_CTime = $Indx_CTime & ":" & _FillZero(StringRight($Indx_CTime_tmp,4))
;		ConsoleWrite("$Indx_CTime = " & $Indx_CTime & @crlf)
;
		$Indx_ATime = StringMid($Entry,$NextEntryOffset+64,16)
		$Indx_ATime = StringMid($Indx_ATime,15,2) & StringMid($Indx_ATime,13,2) & StringMid($Indx_ATime,11,2) & StringMid($Indx_ATime,9,2) & StringMid($Indx_ATime,7,2) & StringMid($Indx_ATime,5,2) & StringMid($Indx_ATime,3,2) & StringMid($Indx_ATime,1,2)
		$Indx_ATime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_ATime)
		$Indx_ATime = _WinTime_UTCFileTimeFormat(Dec($Indx_ATime)-$tDelta,$DateTimeFormat,2)
		$Indx_ATime = $Indx_ATime & ":" & _FillZero(StringRight($Indx_ATime_tmp,4))
;		ConsoleWrite("$Indx_ATime = " & $Indx_ATime & @crlf)
;
		$Indx_MTime = StringMid($Entry,$NextEntryOffset+80,16)
		$Indx_MTime = StringMid($Indx_MTime,15,2) & StringMid($Indx_MTime,13,2) & StringMid($Indx_MTime,11,2) & StringMid($Indx_MTime,9,2) & StringMid($Indx_MTime,7,2) & StringMid($Indx_MTime,5,2) & StringMid($Indx_MTime,3,2) & StringMid($Indx_MTime,1,2)
		$Indx_MTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_MTime)
		$Indx_MTime = _WinTime_UTCFileTimeFormat(Dec($Indx_MTime)-$tDelta,$DateTimeFormat,2)
		$Indx_MTime = $Indx_MTime & ":" & _FillZero(StringRight($Indx_MTime_tmp,4))
;		ConsoleWrite("$Indx_MTime = " & $Indx_MTime & @crlf)
;
		$Indx_RTime = StringMid($Entry,$NextEntryOffset+96,16)
		$Indx_RTime = StringMid($Indx_RTime,15,2) & StringMid($Indx_RTime,13,2) & StringMid($Indx_RTime,11,2) & StringMid($Indx_RTime,9,2) & StringMid($Indx_RTime,7,2) & StringMid($Indx_RTime,5,2) & StringMid($Indx_RTime,3,2) & StringMid($Indx_RTime,1,2)
		$Indx_RTime_tmp = _WinTime_UTCFileTimeToLocalFileTime("0x" & $Indx_RTime)
		$Indx_RTime = _WinTime_UTCFileTimeFormat(Dec($Indx_RTime)-$tDelta,$DateTimeFormat,2)
		$Indx_RTime = $Indx_RTime & ":" & _FillZero(StringRight($Indx_RTime_tmp,4))
;		ConsoleWrite("$Indx_RTime = " & $Indx_RTime & @crlf)
;
		#ce
#cs
		$Indx_AllocSize = StringMid($Entry,$NextEntryOffset+112,16)
		$Indx_AllocSize = Dec(StringMid($Indx_AllocSize,15,2) & StringMid($Indx_AllocSize,13,2) & StringMid($Indx_AllocSize,11,2) & StringMid($Indx_AllocSize,9,2) & StringMid($Indx_AllocSize,7,2) & StringMid($Indx_AllocSize,5,2) & StringMid($Indx_AllocSize,3,2) & StringMid($Indx_AllocSize,1,2))
;		ConsoleWrite("$Indx_AllocSize = " & $Indx_AllocSize & @crlf)
		$Indx_RealSize = StringMid($Entry,$NextEntryOffset+128,16)
		$Indx_RealSize = Dec(StringMid($Indx_RealSize,15,2) & StringMid($Indx_RealSize,13,2) & StringMid($Indx_RealSize,11,2) & StringMid($Indx_RealSize,9,2) & StringMid($Indx_RealSize,7,2) & StringMid($Indx_RealSize,5,2) & StringMid($Indx_RealSize,3,2) & StringMid($Indx_RealSize,1,2))
;		ConsoleWrite("$Indx_RealSize = " & $Indx_RealSize & @crlf)
		$Indx_File_Flags = StringMid($Entry,$NextEntryOffset+144,16)
;		ConsoleWrite("$Indx_File_Flags = " & $Indx_File_Flags & @crlf)
		$Indx_File_Flags = StringMid($Indx_File_Flags,15,2) & StringMid($Indx_File_Flags,13,2) & StringMid($Indx_File_Flags,11,2) & StringMid($Indx_File_Flags,9,2)&StringMid($Indx_File_Flags,7,2) & StringMid($Indx_File_Flags,5,2) & StringMid($Indx_File_Flags,3,2) & StringMid($Indx_File_Flags,1,2)
;		ConsoleWrite("$Indx_File_Flags = " & $Indx_File_Flags & @crlf)
		$Indx_File_Flags = StringMid($Indx_File_Flags,13,8)
		$Indx_File_Flags = _File_Attributes("0x" & $Indx_File_Flags)
;		ConsoleWrite("$Indx_File_Flags = " & $Indx_File_Flags & @crlf)
#ce
		$Indx_NameLength = StringMid($Entry,$NextEntryOffset+160,2)
		$Indx_NameLength = Dec($Indx_NameLength)
;		ConsoleWrite("$Indx_NameLength = " & $Indx_NameLength & @crlf)
		$Indx_NameSpace = StringMid($Entry,$NextEntryOffset+162,2)
;		ConsoleWrite("$Indx_NameSpace = " & $Indx_NameSpace & @crlf)
		Select
			Case $Indx_NameSpace = "00"	;POSIX
				$Indx_NameSpace = "POSIX"
			Case $Indx_NameSpace = "01"	;WIN32
				$Indx_NameSpace = "WIN32"
			Case $Indx_NameSpace = "02"	;DOS
				$Indx_NameSpace = "DOS"
			Case $Indx_NameSpace = "03"	;DOS+WIN32
				$Indx_NameSpace = "DOS+WIN32"
		EndSelect
		$Indx_FileName = StringMid($Entry,$NextEntryOffset+164,$Indx_NameLength*4)
;		ConsoleWrite("$Indx_FileName = " & $Indx_FileName & @crlf)
;		$Indx_FileName = _UnicodeHexToStr($Indx_FileName)
		$Indx_FileName = BinaryToString("0x"&$Indx_FileName,2)
;		ConsoleWrite("$Indx_FileName: " & $Indx_FileName & @crlf)
		#cs
		$tmp0 = 0
		$tmp2 = 0
		$tmp3 = 0
		$tmp1 = 164+($Indx_NameLength*2*2)
		Do ; Calculate the length of the padding - 8 byte aligned
			$tmp2 = $tmp1/16
			If Not IsInt($tmp2) Then
				$tmp0 = 2
				$tmp1 += $tmp0
				$tmp3 += $tmp0
			EndIf
		Until IsInt($tmp2)
		$PaddingLength = $tmp3
;		ConsoleWrite("$PaddingLength = " & $PaddingLength & @crlf)
		$Padding = StringMid($Entry,$NextEntryOffset+164+($Indx_NameLength*2*2),$PaddingLength)
		#ce
		$tmp1 = 164+($Indx_NameLength*2*2)
		$PaddingLength = 0
		If Mod($tmp1,16) Then
			While 1
				$PaddingLength+=1
				$tmp1 += 1
				If Mod($tmp1,16) = 0 Then ExitLoop
			WEnd
		EndIf
;		ConsoleWrite("$Padding = " & $Padding & @crlf)
		If $IndexFlags <> "0000" Then
;			$SubNodeVCN = StringMid($Entry,$NextEntryOffset+164+($Indx_NameLength*2*2)+$PaddingLength,16)
			$SubNodeVCNLength = 16
		Else
;			$SubNodeVCN = ""
			$SubNodeVCNLength = 0
		EndIf
;		ConsoleWrite("$SubNodeVCN = " & $SubNodeVCN & @crlf)
		$NextEntryOffset = $NextEntryOffset+164+($Indx_NameLength*2*2)+$PaddingLength+$SubNodeVCNLength
		ReDim $IndxEntryNumberArr[1+$EntryCounter]
		ReDim $IndxMFTReferenceArr[1+$EntryCounter]
;		Redim $IndxMFTRefSeqNoArr[1+$EntryCounter]
;		ReDim $IndxIndexFlagsArr[1+$EntryCounter]
;		ReDim $IndxMFTReferenceOfParentArr[1+$EntryCounter]
;		ReDim $IndxMFTParentRefSeqNoArr[1+$EntryCounter]
;		ReDim $IndxCTimeArr[1+$EntryCounter]
;		ReDim $IndxATimeArr[1+$EntryCounter]
;		ReDim $IndxMTimeArr[1+$EntryCounter]
;		ReDim $IndxRTimeArr[1+$EntryCounter]
;		ReDim $IndxAllocSizeArr[1+$EntryCounter]
;		ReDim $IndxRealSizeArr[1+$EntryCounter]
;		ReDim $IndxFileFlagsArr[1+$EntryCounter]
		ReDim $IndxFileNameArr[1+$EntryCounter]
;		ReDim $IndxNameSpaceArr[1+$EntryCounter]
;		ReDim $IndxSubNodeVCNArr[1+$EntryCounter]
		$IndxEntryNumberArr[$EntryCounter] = $EntryCounter
		$IndxMFTReferenceArr[$EntryCounter] = $MFTReference
;		$IndxMFTRefSeqNoArr[$EntryCounter] = $MFTReferenceSeqNo
;		$IndxIndexFlagsArr[$EntryCounter] = $IndexFlags
;		$IndxMFTReferenceOfParentArr[$EntryCounter] = $MFTReferenceOfParent
;		$IndxMFTParentRefSeqNoArr[$EntryCounter] = $MFTReferenceOfParentSeqNo
;		$IndxCTimeArr[$EntryCounter] = $Indx_CTime
;		$IndxATimeArr[$EntryCounter] = $Indx_ATime
;		$IndxMTimeArr[$EntryCounter] = $Indx_MTime
;		$IndxRTimeArr[$EntryCounter] = $Indx_RTime
;		$IndxAllocSizeArr[$EntryCounter] = $Indx_AllocSize
;		$IndxRealSizeArr[$EntryCounter] = $Indx_RealSize
;		$IndxFileFlagsArr[$EntryCounter] = $Indx_File_Flags
		$IndxFileNameArr[$EntryCounter] = $Indx_FileName
;		$IndxNameSpaceArr[$EntryCounter] = $Indx_NameSpace
;		$IndxSubNodeVCNArr[$EntryCounter] = $SubNodeVCN
;		_ArrayDisplay($IndxFileNameArr,"$IndxFileNameArr")
	Until $NextEntryOffset+32 >= StringLen($Entry)
;	If $DummyVar Then _ArrayDisplay($IndxFileNameArr,"$IndxFileNameArr")
EndFunc

Func _AttribHeaderFlags($AHinput)
	Local $AHoutput = ""
	If BitAND($AHinput,0x0001) Then $AHoutput &= 'COMPRESSED+'
	If BitAND($AHinput,0x4000) Then $AHoutput &= 'ENCRYPTED+'
	If BitAND($AHinput,0x8000) Then $AHoutput &= 'SPARSE+'
	$AHoutput = StringTrimRight($AHoutput,1)
	Return $AHoutput
EndFunc

Func _File_Permissions($FPinput)
	Local $FPoutput = ""
	If BitAND($FPinput,0x0001) Then $FPoutput &= 'read_only+'
	If BitAND($FPinput,0x0002) Then $FPoutput &= 'hidden+'
	If BitAND($FPinput,0x0004) Then $FPoutput &= 'system+'
	If BitAND($FPinput,0x0020) Then $FPoutput &= 'archive+'
	If BitAND($FPinput,0x0040) Then $FPoutput &= 'device+'
	If BitAND($FPinput,0x0080) Then $FPoutput &= 'normal+'
	If BitAND($FPinput,0x0100) Then $FPoutput &= 'temporary+'
	If BitAND($FPinput,0x0200) Then $FPoutput &= 'sparse_file+'
	If BitAND($FPinput,0x0400) Then $FPoutput &= 'reparse_point+'
	If BitAND($FPinput,0x0800) Then $FPoutput &= 'compressed+'
	If BitAND($FPinput,0x1000) Then $FPoutput &= 'offline+'
	If BitAND($FPinput,0x2000) Then $FPoutput &= 'not_indexed+'
	If BitAND($FPinput,0x4000) Then $FPoutput &= 'encrypted+'
	If BitAND($FPinput,0x10000000) Then $FPoutput &= 'directory+'
	If BitAND($FPinput,0x20000000) Then $FPoutput &= 'index_view+'
	$FPoutput = StringTrimRight($FPoutput,1)
	Return $FPoutput
EndFunc

Func _FillZero($inp)
	Local $inplen, $out, $tmp = ""
	$inplen = StringLen($inp)
	For $i = 1 To 4-$inplen
		$tmp &= "0"
	Next
	$out = $tmp & $inp
	Return $out
EndFunc

Func _DecToLittleEndian($DecimalInput)
	Return _SwapEndian(Hex($DecimalInput,8))
EndFunc

Func _StripIndxRecord($Entry)
;	ConsoleWrite("Starting function _StripIndxRecord()" & @crlf)
	Local $LocalAttributeOffset = 1,$IndxHdrUpdateSeqArrOffset,$IndxHdrUpdateSeqArrSize,$IndxHdrUpdSeqArr,$IndxHdrUpdSeqArrPart0,$IndxHdrUpdSeqArrPart1,$IndxHdrUpdSeqArrPart2,$IndxHdrUpdSeqArrPart3,$IndxHdrUpdSeqArrPart4,$IndxHdrUpdSeqArrPart5,$IndxHdrUpdSeqArrPart6,$IndxHdrUpdSeqArrPart7,$IndxHdrUpdSeqArrPart8
	Local $IndxRecordEnd1,$IndxRecordEnd2,$IndxRecordEnd3,$IndxRecordEnd4,$IndxRecordEnd5,$IndxRecordEnd6,$IndxRecordEnd7,$IndxRecordEnd8,$IndxRecordSize,$IndxHeaderSize,$IsNotLeafNode
;	ConsoleWrite("Unfixed INDX record:" & @crlf)
;	ConsoleWrite(_HexEncode("0x"&$Entry) & @crlf)
;	ConsoleWrite(_HexEncode("0x" & StringMid($Entry,1,4096)) & @crlf)
	$IndxHdrUpdateSeqArrOffset = Dec(_SwapEndian(StringMid($Entry,$LocalAttributeOffset+8,4)))
;	ConsoleWrite("$IndxHdrUpdateSeqArrOffset = " & $IndxHdrUpdateSeqArrOffset & @crlf)
	$IndxHdrUpdateSeqArrSize = Dec(_SwapEndian(StringMid($Entry,$LocalAttributeOffset+12,4)))
;	ConsoleWrite("$IndxHdrUpdateSeqArrSize = " & $IndxHdrUpdateSeqArrSize & @crlf)
	$IndxHdrUpdSeqArr = StringMid($Entry,1+($IndxHdrUpdateSeqArrOffset*2),$IndxHdrUpdateSeqArrSize*2*2)
;	ConsoleWrite("$IndxHdrUpdSeqArr = " & $IndxHdrUpdSeqArr & @crlf)
	$IndxHdrUpdSeqArrPart0 = StringMid($IndxHdrUpdSeqArr,1,4)
	$IndxHdrUpdSeqArrPart1 = StringMid($IndxHdrUpdSeqArr,5,4)
	$IndxHdrUpdSeqArrPart2 = StringMid($IndxHdrUpdSeqArr,9,4)
	$IndxHdrUpdSeqArrPart3 = StringMid($IndxHdrUpdSeqArr,13,4)
	$IndxHdrUpdSeqArrPart4 = StringMid($IndxHdrUpdSeqArr,17,4)
	$IndxHdrUpdSeqArrPart5 = StringMid($IndxHdrUpdSeqArr,21,4)
	$IndxHdrUpdSeqArrPart6 = StringMid($IndxHdrUpdSeqArr,25,4)
	$IndxHdrUpdSeqArrPart7 = StringMid($IndxHdrUpdSeqArr,29,4)
	$IndxHdrUpdSeqArrPart8 = StringMid($IndxHdrUpdSeqArr,33,4)
	$IndxRecordEnd1 = StringMid($Entry,1021,4)
	$IndxRecordEnd2 = StringMid($Entry,2045,4)
	$IndxRecordEnd3 = StringMid($Entry,3069,4)
	$IndxRecordEnd4 = StringMid($Entry,4093,4)
	$IndxRecordEnd5 = StringMid($Entry,5117,4)
	$IndxRecordEnd6 = StringMid($Entry,6141,4)
	$IndxRecordEnd7 = StringMid($Entry,7165,4)
	$IndxRecordEnd8 = StringMid($Entry,8189,4)
	If $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd1 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd2 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd3 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd4 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd5 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd6 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd7 OR $IndxHdrUpdSeqArrPart0 <> $IndxRecordEnd8 Then
		ConsoleWrite("Error the INDX record is corrupt" & @CRLF)
		Return ; Not really correct because I think in theory chunks of 1024 bytes can be invalid and not just everything or nothing for the given INDX record.
	Else
		$Entry = StringMid($Entry,1,1020) & $IndxHdrUpdSeqArrPart1 & StringMid($Entry,1025,1020) & $IndxHdrUpdSeqArrPart2 & StringMid($Entry,2049,1020) & $IndxHdrUpdSeqArrPart3 & StringMid($Entry,3073,1020) & $IndxHdrUpdSeqArrPart4 & StringMid($Entry,4097,1020) & $IndxHdrUpdSeqArrPart5 & StringMid($Entry,5121,1020) & $IndxHdrUpdSeqArrPart6 & StringMid($Entry,6145,1020) & $IndxHdrUpdSeqArrPart7 & StringMid($Entry,7169,1020) & $IndxHdrUpdSeqArrPart8
	EndIf
	$IndxRecordSize = Dec(_SwapEndian(StringMid($Entry,$LocalAttributeOffset+56,8)),2)
;	ConsoleWrite("$IndxRecordSize = " & $IndxRecordSize & @crlf)
	$IndxHeaderSize = Dec(_SwapEndian(StringMid($Entry,$LocalAttributeOffset+48,8)),2)
;	ConsoleWrite("$IndxHeaderSize = " & $IndxHeaderSize & @crlf)
	$IsNotLeafNode = StringMid($Entry,$LocalAttributeOffset+72,2) ;1 if not leaf node
	$Entry = StringMid($Entry,$LocalAttributeOffset+48+($IndxHeaderSize*2),($IndxRecordSize-$IndxHeaderSize-16)*2)
	If $IsNotLeafNode = "01" Then  ; This flag leads to the entry being 8 bytes of 00's longer than the others. Can be stripped I think.
		$Entry = StringTrimRight($Entry,16)
;		ConsoleWrite("Is not leaf node..." & @crlf)
	EndIf
	Return $Entry
EndFunc

Func _Prep($TargetDevice,$IndexNumber,$TargetFileName)
	Local $RetRec[2],$PathTmp,$NewRecord,$TmpOffsetTarget

	If StringIsDigit($IndexNumber) Then ;Target specified by IndexNumber
		Global $DataQ[1]
		;Target
;		ConsoleWrite("Target specified by IndexNumber" & @CRLF)
		$RetRec = _FindFileMFTRecord($TargetDevice,$IndexNumber)
		$TmpOffsetTarget = $RetRec[0]
		$NewRecord = $RetRec[1]
		If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
			ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($TmpOffsetTarget) & @CRLF)
			Return 0
		EndIf
		_DecodeNameQ($NameQ)
		$InfoArrShadowMainTarget[0] = $HEADER_MFTREcordNumber
		$InfoArrShadowMainTarget[1] = $FN_FileName
		$InfoArrShadowMainTarget[2] = $TmpOffsetTarget
		$InfoArrShadowParent[0] = $FN_ParentReferenceNo
		;If StringInStr($SIArrValue[8][1],"directory") Then
		If StringInStr($FNArrValue[9][1],"directory") Then
			;$IsDirectory = 1
		Else
			;$IsDirectory = 0
		EndIf

		If $HEADER_MFTREcordNumber = $FN_ParentReferenceNo = 5 Then
			;$ParentMode=0
		Else
			;$ParentMode=1
		EndIf
		;Parent of target
		$RetRec = _FindFileMFTRecord($TargetDevice,$FN_ParentReferenceNo)
		$TmpOffsetTarget = $RetRec[0]
		$NewRecord = $RetRec[1]
		$DoIndxOffsetArray=1
		If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
			ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($TmpOffsetTarget) & @CRLF)
			Return 0
		EndIf
		$DoIndxOffsetArray=0
		$IsCurrentIndxOfParent=0
		If $InfoArrShadowParent[0] <> $HEADER_MFTREcordNumber Then
			ConsoleWrite("Error: Validating ref of target record" & @CRLF)
			Return 0
		EndIf
		$InfoArrShadowParent[1] = $FN_FileName
		$InfoArrShadowParent[2] = $TmpOffsetTarget
;		If Not _PopulateIndxTimestamps($InfoArrShadowMainTarget[1],$InfoArrShadowMainTarget[0]) Then
;			ConsoleWrite("Error: Retrieving INDX timestamps failed" & @CRLF)
;			Return 0
;		EndIf
	Else
;		$TargetFileNameTmp = $TargetFileName
;		If StringInStr($TargetFileNameTmp,"GLOBALROOT\Device\HarddiskVolumeShadowCopy") Then $TargetFileNameTmp = StringReplace($TargetFileNameTmp,"GLOBALROOT\Device\HarddiskVolumeShadowCopy","GLOBALROOTDeviceHarddiskVolumeShadowCopy")

		;Target specified by full path
		$PathTmp = _SplitPath($TargetFileName)
		If @error Then
			ConsoleWrite("Error in _SplitPath() resolving path to: " & $TargetFileName & @CRLF)
			Return 0
		EndIf
;		_ArrayDisplay($PathTmp,"$PathTmp")
;		If StringInStr($PathTmp[0],"GLOBALROOTDeviceHarddiskVolumeShadowCopy") Then $PathTmp[0] = StringReplace($PathTmp[0],"GLOBALROOTDeviceHarddiskVolumeShadowCopy","GLOBALROOT\Device\HarddiskVolumeShadowCopy")
		Select
			Case $PathTmp[2] = "" And $PathTmp[1] = "" ;Root directory
;				ConsoleWrite("Case 1" & @CRLF)
				;Target
				$RetRec = _FindFileMFTRecord($TargetDevice,5)
				$TmpOffsetTarget = $RetRec[0]
				$NewRecord = $RetRec[1]
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($RetRec[0]) & @CRLF)
					Return 0
				EndIf
				If 5 <> $HEADER_MFTREcordNumber Then
					ConsoleWrite("Error: Validating ref of target record" & @CRLF)
					Return 0
				EndIf
				If StringInStr($FNArrValue[9][1],"directory") Then
					;$IsDirectory = 1
				Else
					;$IsDirectory = 0
				EndIf
				$InfoArrShadowMainTarget[0] = $HEADER_MFTREcordNumber
				$InfoArrShadowMainTarget[1] = $FN_FileName
				$InfoArrShadowMainTarget[2] = $TmpOffsetTarget
				$InfoArrShadowParent[0] = ""
				;$ParentMode=0
			Case $PathTmp[2] = "" And $PathTmp[1] <> "" ;1 level down from root
;				ConsoleWrite("Case 2" & @CRLF)
				;Parent of target
				$RetRec = _FindFileMFTRecord($TargetDevice,5)
				$TmpOffsetTarget = $RetRec[0]
				$NewRecord = $RetRec[1]
				$DoIndxOffsetArray=1
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($RetRec[0]) & @CRLF)
					Return 0
				EndIf
				$DoIndxOffsetArray=0
				$IsCurrentIndxOfParent=0
				If 5 <> $HEADER_MFTREcordNumber Then
					ConsoleWrite("Error: Validating ref of target record" & @CRLF)
					Return 0
				EndIf
				$InfoArrShadowParent[0] = $HEADER_MFTREcordNumber
				$InfoArrShadowParent[1] = $FN_FileName
				$InfoArrShadowParent[2] = $TmpOffsetTarget
	;			ConsoleWrite("$InfoArrShadowParent[0]: " & $InfoArrShadowParent[0] & @CRLF)
	;			ConsoleWrite("$InfoArrShadowParent[1]: " & $InfoArrShadowParent[1] & @CRLF)
	;			ConsoleWrite("$InfoArrShadowParent[2]: " & $InfoArrShadowParent[2] & @CRLF)
				;Target
;				$TmpRef = _RawResolveRef($TargetDevice,$PathTmp[0], $PathTmp[1])
				$TmpRef = _RawResolveRef2($TargetDevice,$PathTmp[0], $PathTmp[1], 0)
				If $TmpRef Then
	;				ConsoleWrite("$TmpRef: " & $TmpRef & @CRLF)
				Else
					ConsoleWrite("Error resolving: " & $PathTmp[0] & "\" & $PathTmp[1] & @CRLF)
					Return 0
				EndIf
				$RetRec = _FindFileMFTRecord($TargetDevice,$TmpRef)
				$TmpOffsetTarget = $RetRec[0]
				$NewRecord = $RetRec[1]
;				If Not _PopulateIndxTimestamps($PathTmp[1],$TmpRef) Then
;					ConsoleWrite("Error: Retrieving INDX timestamps failed" & @CRLF)
;					Return 0
;				EndIf
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($RetRec[0]) & @CRLF)
					Return 0
				EndIf
				If $TmpRef <> $HEADER_MFTREcordNumber And 5 <> $FN_ParentReferenceNo Then
					ConsoleWrite("Error: Validating refs of target record" & @CRLF)
					Return 0
				EndIf
				If StringInStr($FNArrValue[9][1],"directory") Then
					;$IsDirectory = 1
				Else
					;$IsDirectory = 0
				EndIf
				$InfoArrShadowMainTarget[0] = $HEADER_MFTREcordNumber
				$InfoArrShadowMainTarget[1] = $FN_FileName
				$InfoArrShadowMainTarget[2] = $TmpOffsetTarget
	;			ConsoleWrite("$InfoArrShadowMainTarget[0]: " & $InfoArrShadowMainTarget[0] & @CRLF)
	;			ConsoleWrite("$InfoArrShadowMainTarget[1]: " & $InfoArrShadowMainTarget[1] & @CRLF)
	;			ConsoleWrite("$InfoArrShadowMainTarget[2]: " & $InfoArrShadowMainTarget[2] & @CRLF)
				;$ParentMode=1
			Case $PathTmp[2] <> "" And $PathTmp[1] <> "" ;Anything from 2 or more levels down from root
;				ConsoleWrite("Case 3" & @CRLF)
				;Parent of target
;				$TmpRef = _RawResolveRef($TargetDevice,$PathTmp[0], $PathTmp[1])
				$TmpRef = _RawResolveRef2($TargetDevice,$PathTmp[0], $PathTmp[1], 0)
				If $TmpRef Then
	;				ConsoleWrite("$TmpRef: " & $TmpRef & @CRLF)
				Else
					ConsoleWrite("Error resolving: " & $PathTmp[0] & "\" & $PathTmp[1] & @CRLF)
					Return 0
				EndIf
				$RetRec = _FindFileMFTRecord($TargetDevice,$TmpRef)
				$TmpOffsetTarget = $RetRec[0]
				$NewRecord = $RetRec[1]
				$DoIndxOffsetArray=1
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($RetRec[0]) & @CRLF)
					Return 0
				EndIf
				$DoIndxOffsetArray=0
				$IsCurrentIndxOfParent=0
				If $TmpRef <> $HEADER_MFTREcordNumber Then
					ConsoleWrite("Error: Validating ref of target record" & @CRLF)
					return 0
				EndIf
				$InfoArrShadowParent[0] = $HEADER_MFTREcordNumber
				$InfoArrShadowParent[1] = $FN_FileName
				$InfoArrShadowParent[2] = $TmpOffsetTarget
				;Target
;				$TmpRef = _RawResolveRef($TargetDevice,$PathTmp[0] & "\" & $PathTmp[1], $PathTmp[2])
				$TmpRef = _RawResolveRef2($TargetDevice,$PathTmp[0] & "\" & $PathTmp[1], $PathTmp[2], 0)
				If $TmpRef Then
	;				ConsoleWrite("$TmpRef: " & $TmpRef & @CRLF)
				Else
					ConsoleWrite("Error resolving: " & $PathTmp[0] & "\" & $PathTmp[1] & "\" & $PathTmp[2] & @CRLF)
					Return 0
				EndIf
				$RetRec = _FindFileMFTRecord($TargetDevice,$TmpRef)
				$TmpOffsetTarget = $RetRec[0]
				$NewRecord = $RetRec[1]
;				If Not _PopulateIndxTimestamps($PathTmp[2],$TmpRef) Then
;					ConsoleWrite("Error: Retrieving INDX timestamps failed" & @CRLF)
;					Return 0
;				EndIf
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & Hex($RetRec[0]) & @CRLF)
					Return 0
				EndIf
				If $TmpRef <> $HEADER_MFTREcordNumber And 5 <> $FN_ParentReferenceNo Then
					ConsoleWrite("Error: Validating refs of target record" & @CRLF)
					Return 0
				EndIf
				;If StringInStr($SIArrValue[8][1],"directory") Then
				If StringInStr($FNArrValue[9][1],"directory") Then
					;$IsDirectory = 1
				Else
					;$IsDirectory = 0
				EndIf
				$InfoArrShadowMainTarget[0] = $HEADER_MFTREcordNumber
				$InfoArrShadowMainTarget[1] = $FN_FileName
				$InfoArrShadowMainTarget[2] = $TmpOffsetTarget
				;$ParentMode=1
		EndSelect
	EndIf
	ConsoleWrite("Target filename: " & $InfoArrShadowMainTarget[1] & @CRLF)
	ConsoleWrite("Target fileref: " & $InfoArrShadowMainTarget[0] & @CRLF)
	ConsoleWrite("Target MFT record offset: 0x" & Hex($InfoArrShadowMainTarget[2]) & @CRLF)
	ConsoleWrite("Parent filename: " & $InfoArrShadowParent[1] & @CRLF)
	ConsoleWrite("Parent fileref: " & $InfoArrShadowParent[0] & @CRLF)
	ConsoleWrite("Parent MFT record offset: 0x" & Hex($InfoArrShadowParent[2]) & @CRLF & @CRLF)
	Global $IndxFileNameFromParentCurrentArr = $IndxFileNameFromParentArr
	Global $IndxMFTReferenceFromParentCurrentArr = $IndxMFTReferenceFromParentArr
	Global $IndxMFTReferenceOfParentFromParentCurrentArr = $IndxMFTReferenceOfParentFromParentArr
	Global $IndxCTimeFromParentCurrentArr = $IndxCTimeFromParentArr
	Global $IndxATimeFromParentCurrentArr = $IndxATimeFromParentArr
	Global $IndxMTimeFromParentCurrentArr = $IndxMTimeFromParentArr
	Global $IndxRTimeFromParentCurrentArr = $IndxRTimeFromParentArr
	Return 1
EndFunc

Func _SplitPath($InPath)
	Local $Reconstruct,$FilePathSplit[3], $DirArray
;	ConsoleWrite("_SplitPath()" & @CRLF)
;	ConsoleWrite("$InPath: " & $InPath & @CRLF)
	If StringRight($InPath,1) = "\" Then $InPath = StringTrimRight($InPath,1)
	$DirArray = StringSplit($InPath,"\")
;	ConsoleWrite("$DirArray[0]: " & $DirArray[0] & @CRLF)
	If StringLen($InPath) = 2 Then
		$FilePathSplit[0] = $InPath
		$FilePathSplit[1] = ""
		$FilePathSplit[2] = ""
		Return $FilePathSplit
	EndIf
	If $DirArray[0] = 2 Then
		$FilePathSplit[0] = $DirArray[1]
		$FilePathSplit[1] = $DirArray[2]
		$FilePathSplit[2] = ""
		Return $FilePathSplit
	EndIf
	For $i = 1 To $DirArray[0]-2
;		ConsoleWrite("$DirArray[$i]: " & $DirArray[$i] & @CRLF)
		$Reconstruct &= $DirArray[$i]&"\"
	Next
	$Reconstruct = StringTrimRight($Reconstruct,1)
	$FilePathSplit[0] = $Reconstruct
	$FilePathSplit[1] = $DirArray[Ubound($DirArray)-2]
	$FilePathSplit[2] = $DirArray[Ubound($DirArray)-1]
	Return $FilePathSplit
EndFunc

Func _GenDirArray($InPath)
	Local $Reconstruct
;	ConsoleWrite("_GenDirArray()" & @CRLF)
;	ConsoleWrite("$InPath: " & $InPath & @CRLF)
	Global $DirArray = StringSplit($InPath,"\")
	;$LockedFileName = $DirArray[$DirArray[0]]
	For $i = 1 To $DirArray[0]-1
		$Reconstruct &= $DirArray[$i]&"\"
	Next
	$Reconstruct = StringTrimRight($Reconstruct,1)
	Return $Reconstruct
EndFunc

Func _RawResolveRef($TargetDevice,$ParentPath, $FileName)
	Local $NextRef,$RetRec[2],$NewRecord
	Local $ResolvedRef=0 ;We don't use this function for resolving $MFT itself anyway
;	ConsoleWrite("_RawResolveRef()" & @CRLF)
;	ConsoleWrite("$ParentPath: " & $ParentPath & @CRLF)
;	ConsoleWrite("$FileName: " & $FileName & @CRLF)
	If StringLen($ParentPath)=2 Then $ParentPath&="\"
	;$ParentDir = _GenDirArray($ParentPath)
	 _GenDirArray($ParentPath)
;	ConsoleWrite("$ParentDir: " & $ParentDir & @CRLF)
	Global $MftRefArray[$DirArray[0]+1]
	_ReadBootSector($TargetDevice)
	$BytesPerCluster = $SectorsPerCluster*$BytesPerSector
	$MFTEntry = _FindMFT($TargetDevice,0)
	If _DecodeMFTRecord($TargetDrive,$MFTEntry,0) < 1 Then
		ConsoleWrite("Could not verify MFT record of MFT itself (volume corrupt)" & @CRLF)
		Return 0
	EndIf
	_DecodeDataQEntry($DataQ[1])
	$MFTSize = $DATA_RealSize
	Global $RUN_VCN[1], $RUN_Clusters[1]
	_ExtractDataRuns()
	$MFT_RUN_VCN = $RUN_VCN
	$MFT_RUN_Clusters = $RUN_Clusters
	$NextRef = 5
	$MftRefArray[1]=$NextRef
	;$ResolvedPath = $DirArray[1]
	For $i = 2 To $DirArray[0]
		Global $DataQ[1]
		$RetRec = _FindFileMFTRecord($TargetDevice,$NextRef)
		$NewRecord = $RetRec[1]
		If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
			ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
			Return 0
		EndIf
		$NextRef = _ParseIndex($DirArray[$i])
		$MftRefArray[$i]=$NextRef
;		ConsoleWrite("$MftRefArray[$i]: " & $MftRefArray[$i] & @CRLF)
		If @error Then
			Global $DataQ[1]
			$RetRec = _FindFileMFTRecord($TargetDevice,$MftRefArray[$i-1])
			$NewRecord = $RetRec[1]
			If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
				ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
				Return 0
			EndIf
			$ResolvedRef = _GetMftRefFromIndex($FileName)
		ElseIf $i=$DirArray[0] Then
			Global $DataQ[1]
;			ConsoleWrite("$MftRefArray[$i]: " & $MftRefArray[$i] & @CRLF)
			$RetRec = _FindFileMFTRecord($TargetDevice,$MftRefArray[$i])
			$NewRecord = $RetRec[1]
			If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
				ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
				Return 0
			EndIf
			;$LastCheck = _DisplayList($ResolvedPath & "\" & $DirArray[$i])
;			ConsoleWrite("$FileName: " & $FileName & @CRLF)
			$ResolvedRef = _GetMftRefFromIndex($FileName)
			If @error Then ; In case last part was a file and not a directory
				Global $DataQ[1]
				$RetRec = _FindFileMFTRecord($TargetDevice,$MftRefArray[$i-1])
				$NewRecord = $RetRec[1]
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
					Return 0
				EndIf
				;$LastCheck = _DisplayList($ResolvedPath)
				$ResolvedRef = _GetMftRefFromIndex($FileName)
			EndIf
		ElseIf StringIsDigit($NextRef) Then
			;$ResolvedPath &= "\" & $DirArray[$i]
			ContinueLoop
		Else
			ConsoleWrite("Error: Something went wrong" & @CRLF)
			ExitLoop
		EndIf
	Next
;	If StringRight($ParentPath,1) = "\" Then $ParentPath = StringTrimRight($ParentPath,1)
;	If $FileName <> "$MFT" And $ResolvedRef <> 0 Then ConsoleWrite("MFT Ref of " & $ParentPath & "\" & $FileName & ": " & $ResolvedRef & @CRLF)
	Return $ResolvedRef
EndFunc

Func _RawResolveRef2($TargetDevice,$ParentPath, $FileName, $ReParseNtfs)
	Local $NextRef,$RetRec[2],$NewRecord
	Local $ResolvedRef=0 ;We don't use this function for resolving $MFT itself anyway
;	ConsoleWrite("$ParentPath: " & $ParentPath & @CRLF)
;	ConsoleWrite("$FileName: " & $FileName & @CRLF)
	If StringLen($ParentPath)=2 Then $ParentPath&="\"
	;$ParentDir = _GenDirArray($ParentPath)
	_GenDirArray($ParentPath)
;	ConsoleWrite("$ParentDir: " & $ParentDir & @CRLF)
	Global $MftRefArray[$DirArray[0]+1]

	If $ReParseNtfs Then
		_ReadBootSector($TargetDevice)
		If @error Then
			ConsoleWrite("Error: Filesystem not NTFS" & @CRLF)
			Exit
		EndIf

		$hDisk = _WinAPI_CreateFile($TargetDevice,2,2,7)
		If $hDisk = 0 Then
			ConsoleWrite("CreateFile: " & _WinAPI_GetLastErrorMessage() & @CRLF)
			Exit
		EndIf
		$MFTEntry = _FindMFT($TargetDevice,0)
		If $MFTEntry = "" Then ;something wrong with record for $MFT
			ConsoleWrite("Error: Getting MFT record 0" & @CRLF)
			Exit
		EndIf
		$MFT = _DecodeMFTRecord0($MFTEntry, 0)        ;produces DataQ for $MFT, record 0
		If $MFT = "" Then
			ConsoleWrite("Error: Parsing the MFT record 0" & @CRLF)
			Exit
		EndIf
		_GetRunsFromAttributeListMFT0() ;produces datarun for $MFT and converts datarun to RUN_VCN[] and RUN_Clusters[]
		_WinAPI_CloseHandle($hDisk)
		$MFTSize = $DATA_RealSize
		$MFT_RUN_VCN = $RUN_VCN
		$MFT_RUN_Clusters = $RUN_Clusters
		_GenRefArray()
	EndIf
	$NextRef = 5
	$MftRefArray[1]=$NextRef
	;$ResolvedPath = $DirArray[1]
	For $i = 2 To $DirArray[0]
		Global $DataQ[1]
;		ConsoleWrite("Test A: " & $i & @CRLF)
		$RetRec = _FindFileMFTRecord($TargetDevice,$NextRef)
		If Not IsArray($RetRec) Then Return SetError(1,0,0)
		$NewRecord = $RetRec[1]
;		ConsoleWrite("Test B: " & $i & @CRLF)
		If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
			ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
			Return 0
		EndIf
;		ConsoleWrite("Test C: " & $i & @CRLF)
		$NextRef = _ParseIndex($DirArray[$i])
;		ConsoleWrite("Test D: " & $i & @CRLF)
		$MftRefArray[$i]=$NextRef
		If @error Then
			Global $DataQ[1]
			$RetRec = _FindFileMFTRecord($TargetDevice,$MftRefArray[$i-1])
			If Not IsArray($RetRec) Then Return SetError(1,0,0)
			$NewRecord = $RetRec[1]
			If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
				ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
				Return 0
			EndIf
			$ResolvedRef = _GetMftRefFromIndex($FileName)
		ElseIf $i=$DirArray[0] Then
			Global $DataQ[1]
			$RetRec = _FindFileMFTRecord($TargetDevice,$MftRefArray[$i])
			If Not IsArray($RetRec) Then Return SetError(1,0,0)
			$NewRecord = $RetRec[1]
			If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
				ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
				Return 0
			EndIf
			$ResolvedRef = _GetMftRefFromIndex($FileName)
			If @error Then ; In case last part was a file and not a directory
				Global $DataQ[1]
				$RetRec = _FindFileMFTRecord($TargetDevice,$MftRefArray[$i-1])
				If Not IsArray($RetRec) Then Return SetError(1,0,0)
				$NewRecord = $RetRec[1]
				If _DecodeMFTRecord($TargetDevice,$NewRecord,1) < 1 Then
					ConsoleWrite("Could not verify MFT record at offset: 0x" & $RetRec[0] & @CRLF)
					Return 0
				EndIf
				$ResolvedRef = _GetMftRefFromIndex($FileName)
			EndIf
		ElseIf StringIsDigit($NextRef) Then
			;$ResolvedPath &= "\" & $DirArray[$i]
			ContinueLoop
		Else
			ConsoleWrite("Error: Something went wrong" & @CRLF)
			ExitLoop
		EndIf
	Next
;	If StringRight($ParentPath,1) = "\" Then $ParentPath = StringTrimRight($ParentPath,1)
;	If $FileName <> "$MFT" And $ResolvedRef <> 0 Then ConsoleWrite("MFT Ref of " & $ParentPath & "\" & $FileName & ": " & $ResolvedRef & @CRLF)
	Return $ResolvedRef
EndFunc

Func _DecodeNameQ($NameQ)
	For $name = 1 To UBound($NameQ) - 1
		$NameString = $NameQ[$name]
		If $NameString = "" Then ContinueLoop
		;$FN_AllocSize = Dec(_SwapEndian(StringMid($NameString,129,16)),2)
		;$FN_RealSize = Dec(_SwapEndian(StringMid($NameString,145,16)),2)
		$FN_NameLength = Dec(StringMid($NameString,177,2))
		$FN_NameSpace = StringMid($NameString,179,2)
		Select
			Case $FN_NameSpace = '00'
				$FN_NameSpace = 'POSIX'
			Case $FN_NameSpace = '01'
				$FN_NameSpace = 'WIN32'
			Case $FN_NameSpace = '02'
				$FN_NameSpace = 'DOS'
			Case $FN_NameSpace = '03'
				$FN_NameSpace = 'DOS+WIN32'
			Case Else
				$FN_NameSpace = 'UNKNOWN'
		EndSelect
		$FN_FileName = StringMid($NameString,181,$FN_NameLength*4)
		$FN_FileName = _UnicodeHexToStr($FN_FileName)
		;If StringLen($FN_FileName) <> $FN_NameLength Then $INVALID_FILENAME = 1
	Next
	Return
EndFunc

Func _ParseIndex($TestName)
	If $AttributesArr[10][2] = "TRUE" Then; $INDEX_ALLOCATION
		For $j = 1 To Ubound($IndxFileNameArr)-1
			If $IndxFileNameArr[$j] = $TestName Then
				Return $IndxMFTReferenceArr[$j]
			Else
;				Return SetError(1,0,0)
			EndIf
		Next
	ElseIf $AttributesArr[9][2] = "TRUE" Then ;And $ResidentIndx Then ; $INDEX_ROOT
		For $j = 1 To Ubound($IndxFileNameArr)-1
			If $IndxFileNameArr[$j] = $TestName Then
				Return $IndxMFTReferenceArr[$j]
			Else
;				Return SetError(1,0,0)
			EndIf
		Next
	Else
;		ConsoleWrite("Error: No index found for: " & $TestName & @CRLF)
		Return SetError(1,0,0)
	EndIf
EndFunc

Func _GetMftRefFromIndex($TargetName)
	If $AttributesArr[10][2] = "TRUE" Then
		;ConsoleWrite("Directory listing for: " & $DirListPath & @CRLF & @CRLF)
		For $j = 1 To Ubound($IndxFileNameArr)-1
			If $IndxFileNameArr[$j] = $TargetName Then
				$ResolvedMftRef = $IndxMFTReferenceArr[$j]
				Return $ResolvedMftRef
			EndIf
		Next
	ElseIf $AttributesArr[9][2] = "TRUE" Then
		;ConsoleWrite("Directory listing for: " & $DirListPath & @CRLF & @CRLF)
		For $j = 1 To Ubound($IndxFileNameArr)-1
			If $IndxFileNameArr[$j] = $TargetName Then
				$ResolvedMftRef = $IndxMFTReferenceArr[$j]
				Return $ResolvedMftRef
			EndIf
		Next
	Else
;		ConsoleWrite("Error: There was no index found for the parent folder." & @CRLF)
		Return SetError(1,0,0)
	EndIf
EndFunc
#cs
Func _PopulateIndxTimestamps($InputFileName,$InputIndexNumber)
	Local $Counter=0
;	ConsoleWrite("$InputFileName: " & $InputFileName & @CRLF & @CRLF)
;	ConsoleWrite("$InputIndexNumber: " & $InputIndexNumber & @CRLF & @CRLF)
	Global $IndxCTimeFromParent,$IndxATimeFromParent,$IndxMTimeFromParent,$IndxRTimeFromParent
	Global $IndxFileNameFromParentArr[1],$IndxMFTReferenceFromParentArr[1],$IndxMFTReferenceOfParentFromParentArr[1],$IndxCTimeFromParentArr[1],$IndxATimeFromParentArr[1],$IndxMTimeFromParentArr[1],$IndxRTimeFromParentArr[1]
	If $AttributesArr[10][2] = "TRUE" Then; $INDEX_ALLOCATION
		;_ArrayDisplay($IndxATimeArr,"$IndxATimeArr")
		;_ArrayDisplay($IndxFileNameArr,"$IndxFileNameArr")
		;_ArrayDisplay($IndxMFTReferenceArr,"$IndxMFTReferenceArr")
		For $j = 1 To Ubound($IndxFileNameArr)-1
			If $IndxMFTReferenceArr[$j] = $InputIndexNumber Then
			;If $IndxFileNameArr[$j] = $InputFileName And $IndxMFTReferenceArr[$j] = $InputIndexNumber Then ;Comparing against the shortname will not always work as the GetShortPathName api will throw Acess Denied on certain files
				$Counter+=1
				Redim $IndxFileNameFromParentArr[$Counter]
				Redim $IndxMFTReferenceFromParentArr[$Counter]
				Redim $IndxMFTReferenceOfParentFromParentArr[$Counter]
				Redim $IndxCTimeFromParentArr[$Counter]
				Redim $IndxATimeFromParentArr[$Counter]
				Redim $IndxMTimeFromParentArr[$Counter]
				Redim $IndxRTimeFromParentArr[$Counter]
				$IndxFileNameFromParentArr[$Counter-1] = $IndxFileNameArr[$j]
				$IndxMFTReferenceFromParentArr[$Counter-1] = $IndxMFTReferenceArr[$j]
				$IndxMFTReferenceOfParentFromParentArr[$Counter-1] = $IndxMFTReferenceOfParentArr[$j]
				$IndxCTimeFromParentArr[$Counter-1] = $IndxCTimeArr[$j]
				$IndxATimeFromParentArr[$Counter-1] = $IndxATimeArr[$j]
				$IndxMTimeFromParentArr[$Counter-1] = $IndxMTimeArr[$j]
				$IndxRTimeFromParentArr[$Counter-1] = $IndxRTimeArr[$j]
;				Return 1
			EndIf
		Next
		If $Counter Then Return 1
	ElseIf $AttributesArr[9][2] = "TRUE" And $ResidentIndx Then ; $INDEX_ROOT
		;_ArrayDisplay($IndxFileNameArr,"$IndxFileNameArr")
		;_ArrayDisplay($IndxMFTReferenceArr,"$IndxMFTReferenceArr")
		For $j = 1 To Ubound($IndxFileNameArr)-1
;			If $DummyVar Then ConsoleWrite("$IndxFileNameArr[$j]: " & $IndxFileNameArr[$j] & @crlf)
			If $IndxMFTReferenceArr[$j] = $InputIndexNumber Then
			;If $IndxFileNameArr[$j] = $InputFileName And $IndxMFTReferenceArr[$j] = $InputIndexNumber Then
				$Counter+=1
				Redim $IndxFileNameFromParentArr[$Counter]
				Redim $IndxMFTReferenceFromParentArr[$Counter]
				Redim $IndxMFTReferenceOfParentFromParentArr[$Counter]
				Redim $IndxCTimeFromParentArr[$Counter]
				Redim $IndxATimeFromParentArr[$Counter]
				Redim $IndxMTimeFromParentArr[$Counter]
				Redim $IndxRTimeFromParentArr[$Counter]
				$IndxFileNameFromParentArr[$Counter-1] = $IndxFileNameArr[$j]
				$IndxMFTReferenceFromParentArr[$Counter-1] = $IndxMFTReferenceArr[$j]
				$IndxMFTReferenceOfParentFromParentArr[$Counter-1] = $IndxMFTReferenceOfParentArr[$j]
				$IndxCTimeFromParentArr[$Counter-1] = $IndxCTimeArr[$j]
				$IndxATimeFromParentArr[$Counter-1] = $IndxATimeArr[$j]
				$IndxMTimeFromParentArr[$Counter-1] = $IndxMTimeArr[$j]
				$IndxRTimeFromParentArr[$Counter-1] = $IndxRTimeArr[$j]
;				Return 1
			EndIf
		Next
		If $Counter Then Return 1
	EndIf
	Return 0
EndFunc
#ce
Func _DoSparse($r,$hFile,$FileSize)
   Local $nBytes
   If Not IsDllStruct($sBuffer) Then _CreateSparseBuffer()
   $i = $RUN_Clusters[$r]
   While $i > 16 And $FileSize > $BytesPerCluster * 16
	 _WinAPI_WriteFile($hFile, DllStructGetPtr($sBuffer), $BytesPerCluster * 16, $nBytes)
	 $i -= 16
	 $FileSize -= $BytesPerCluster * 16
	 ;$ProgressSize = $FileSize
   WEnd
   If $i <> 0 Then
 	 If $FileSize > $BytesPerCluster * $i Then
		_WinAPI_WriteFile($hFile, DllStructGetPtr($sBuffer), $BytesPerCluster * $i, $nBytes)
		$FileSize -= $BytesPerCluster * $i
		;$ProgressSize = $FileSize
	 Else
		_WinAPI_WriteFile($hFile, DllStructGetPtr($sBuffer), $FileSize, $nBytes)
		;$ProgressSize = 0
		Return 0
	 EndIf
   EndIf
   Return $FileSize
EndFunc

Func _CreateSparseBuffer()
   $sBuffer = DllStructCreate("byte[" & $BytesPerCluster * 16 & "]")
   For $i = 1 To $BytesPerCluster * 16
	  DllStructSetData ($sBuffer, $i, 0)
   Next
EndFunc

Func _DecodeMFTRecord0($record, $FileRef)      ;produces DataQ
	;$MftAttrListString=","
;	ConsoleWrite(_HexEncode($record)&@CRLF)
	$record = _DoFixup($record)
	If $record = "" then Return ""  ;corrupt, failed fixup
	$RecordSize = Dec(_SwapEndian(StringMid($record,51,8)),2)
	$AttributeOffset = (Dec(StringMid($record,43,2))*2)+3
	While 1		;only want Attribute List and Data Attributes
		$Type = Dec(_SwapEndian(StringMid($record,$AttributeOffset,8)),2)
		If $Type > 256 Then ExitLoop		;attributes may not be in numerical order
		$AttributeSize = Dec(_SwapEndian(StringMid($record,$AttributeOffset+8,8)),2)
		If $Type = 32 Then
			$AttrList = StringMid($record,$AttributeOffset,$AttributeSize*2)	;whole attribute
			$AttrList = _DecodeAttrList2($FileRef, $AttrList)		;produces $AttrQ - extra record list
;			ConsoleWrite("$AttrList: " & $AttrList & @CRLF)
			If $AttrList = "" Then
;				_DebugOut($FileRef & " Bad Attribute List signature", $record)
				ConsoleWrite($FileRef & " Bad Attribute List signature" & @CRLF)
				Return ""
			Else
				If $AttrQ[0] = "" Then ContinueLoop		;no new records
				$str = ""
				For $i = 1 To $AttrQ[0]
					;$MftAttrListString &= $AttrQ[$i] & ","
;					ConsoleWrite("$AttrQ[$i]: " & $AttrQ[$i] & @CRLF)
					If Not IsNumber(Int($AttrQ[$i])) Then
;						_DebugOut($FileRef & " Overwritten extra record (" & $AttrQ[$i] & ")", $record)
						Return ""
					EndIf
;					ConsoleWrite("$AttrQ[$i]: " & $AttrQ[$i] & @CRLF)
					$rec = _GetAttrListMFTRecord(($AttrQ[$i]*$MFT_Record_Size)+($LogicalClusterNumberforthefileMFT*$BytesPerCluster))
					If StringMid($rec,3,8) <> $RecordSignature Then
;						_DebugOut($FileRef & " Bad signature for extra record", $record)
;						_DebugOut($FileRef & " Bad signature for extra record", $rec)
						Return ""
					EndIf
					If Dec(_SwapEndian(StringMid($rec,67,8)),2) <> $FileRef Then
;						_DebugOut($FileRef & " Bad extra record", $record)
						Return ""
					EndIf
;					$rec = _StripMftRecord($rec, $FileRef)
					$rec = _StripMftRecord($rec)
					If $rec = "" Then
;						_DebugOut($FileRef & " Extra record failed Fixup", $record)
						Return ""
					EndIf
					$str &= $rec		;no header or end marker
				Next
				$record = StringMid($record,1,($RecordSize-8)*2+2) & $str & "FFFFFFFF"       ;strip end first then add
			EndIf
		ElseIf $Type = 128 Then
			ReDim $DataQ[UBound($DataQ) + 1]
			$DataQ[UBound($DataQ) - 1] = StringMid($record,$AttributeOffset,$AttributeSize*2) 		;whole data attribute
		EndIf
		$AttributeOffset += $AttributeSize*2
	WEnd
	Return $record
EndFunc

Func _GetRunsFromAttributeListMFT0()
	For $i = 1 To UBound($DataQ) - 1
		_DecodeDataQEntry($DataQ[$i])
		If $NonResidentFlag = '00' Then
;			ConsoleWrite("Resident" & @CRLF)
		Else
			Global $RUN_VCN[1], $RUN_Clusters[1]
			$TotalClusters = $Data_Clusters
			$RealSize = $DATA_RealSize		;preserve file sizes
			If Not $InitState Then $DATA_InitSize = $DATA_RealSize
			$InitSize = $DATA_InitSize
			_ExtractDataRuns()
			If $TotalClusters * $BytesPerCluster >= $RealSize Then
;				_ExtractFile($MFTRecord)
			Else 		 ;code to handle attribute list
				$Flag = $IsCompressed		;preserve compression state
				For $j = $i + 1 To UBound($DataQ) -1
					_DecodeDataQEntry($DataQ[$j])
					$TotalClusters += $Data_Clusters
					_ExtractDataRuns()
					If $TotalClusters * $BytesPerCluster >= $RealSize Then
						$DATA_RealSize = $RealSize		;restore file sizes
						$DATA_InitSize = $InitSize
						$IsCompressed = $Flag		;recover compression state
						ExitLoop
					EndIf
				Next
				$i = $j
			EndIf
		EndIf
	Next
EndFunc

Func _GetAttrListMFTRecord($Pos)
	Local $nBytes
	Local $rBuffer = DllStructCreate("byte["&$MFT_Record_Size&"]")
	Local $hFile = _WinAPI_CreateFile($TargetDrive,2,2,7)
	If $hFile = 0 then
		ConsoleWrite("Error CreateFile: " & _WinAPI_GetLastErrorMessage() & " for: " & $TargetDrive & @crlf)
		Return SetError(1,0,0)
	EndIf
   _WinAPI_SetFilePointerEx($hFile, $ImageOffset+$Pos, $FILE_BEGIN)
   _WinAPI_ReadFile($hFile, DllStructGetPtr($rBuffer), $MFT_Record_Size, $nBytes)
	$record = DllStructGetData($rBuffer, 1)
	_WinAPI_CloseHandle($hFile)
	Return $record		;returns MFT record for file
EndFunc

Func _DecodeAttrList2($FileRef, $AttrList)
   Local $offset, $nBytes, $List = "", $str = ""
	Local $hFile = _WinAPI_CreateFile($TargetDrive,2,2,7)
	If $hFile = 0 then
		ConsoleWrite("Error CreateFile: " & _WinAPI_GetLastErrorMessage() & " for: " & $TargetDrive & @crlf)
		Return SetError(1,0,0)
	EndIf
   If StringMid($AttrList, 17, 2) = "00" Then		;attribute list is resident in AttrList
	  $offset = Dec(_SwapEndian(StringMid($AttrList, 41, 4)))
	  $List = StringMid($AttrList, $offset*2+1)		;gets list when resident
   Else			;attribute list is found from data run in $AttrList
	  $size = Dec(_SwapEndian(StringMid($AttrList, $offset*2 + 97, 16)))
	  $offset = ($offset + Dec(_SwapEndian(StringMid($AttrList, $offset*2 + 65, 4))))*2
	  $DataRun = StringMid($AttrList, $offset+1, StringLen($AttrList)-$offset)
	  Global $RUN_VCN[1], $RUN_Clusters[1]		;redim arrays
	  _ExtractDataRuns()
	  $cBuffer = DllStructCreate("byte[" & $BytesPerCluster & "]")
	  For $r = 1 To Ubound($RUN_VCN)-1
		 _WinAPI_SetFilePointerEx($hFile, $ImageOffset+($RUN_VCN[$r]*$BytesPerCluster), $FILE_BEGIN)
		 For $i = 1 To $RUN_Clusters[$r]
			_WinAPI_ReadFile($hFile, DllStructGetPtr($cBuffer), $BytesPerCluster, $nBytes)
			$List &= StringTrimLeft(DllStructGetData($cBuffer, 1),2)
		 Next
	  Next
	  $List = StringMid($List, 1, $size*2)
   EndIf
   _WinAPI_CloseHandle($hFile)
   If StringMid($List, 1, 8) <> "10000000" Then Return ""		;bad signature
   $offset = 0
   While StringLen($list) > $offset*2
	  $ref = Dec(_SwapEndian(StringMid($List, $offset*2 + 33, 8)))
	  If $ref <> $FileRef Then		;new attribute
		 If Not StringInStr($str, $ref) Then $str &= $ref & "-"
	  EndIf
	  $offset += Dec(_SwapEndian(StringMid($List, $offset*2 + 9, 4)))
   WEnd
   $AttrQ[0] = ""
   If $str <> "" Then $AttrQ = StringSplit(StringTrimRight($str,1), "-")
   Return $List
EndFunc

Func _DoFixup($record)		;handles NT and XP style
	$UpdSeqArrOffset = Dec(_SwapEndian(StringMid($record,11,4)))
	$UpdSeqArrSize = Dec(_SwapEndian(StringMid($record,15,4)))
	$UpdSeqArr = StringMid($record,3+($UpdSeqArrOffset*2),$UpdSeqArrSize*2*2)
	If $MFT_Record_Size = 1024 Then
		$UpdSeqArrPart0 = StringMid($UpdSeqArr,1,4)
		$UpdSeqArrPart1 = StringMid($UpdSeqArr,5,4)
		$UpdSeqArrPart2 = StringMid($UpdSeqArr,9,4)
		$RecordEnd1 = StringMid($record,1023,4)
		$RecordEnd2 = StringMid($record,2047,4)
		If $UpdSeqArrPart0 <> $RecordEnd1 OR $UpdSeqArrPart0 <> $RecordEnd2 Then
;			_DebugOut($FileRef & " The record failed Fixup", $record)
			Return ""
		EndIf
		Return StringMid($record,1,1022) & $UpdSeqArrPart1 & StringMid($record,1027,1020) & $UpdSeqArrPart2
	ElseIf $MFT_Record_Size = 4096 Then
		$UpdSeqArrPart0 = StringMid($UpdSeqArr,1,4)
		$UpdSeqArrPart1 = StringMid($UpdSeqArr,5,4)
		$UpdSeqArrPart2 = StringMid($UpdSeqArr,9,4)
		$UpdSeqArrPart3 = StringMid($UpdSeqArr,13,4)
		$UpdSeqArrPart4 = StringMid($UpdSeqArr,17,4)
		$UpdSeqArrPart5 = StringMid($UpdSeqArr,21,4)
		$UpdSeqArrPart6 = StringMid($UpdSeqArr,25,4)
		$UpdSeqArrPart7 = StringMid($UpdSeqArr,29,4)
		$UpdSeqArrPart8 = StringMid($UpdSeqArr,33,4)
		$RecordEnd1 = StringMid($record,1023,4)
		$RecordEnd2 = StringMid($record,2047,4)
		$RecordEnd3 = StringMid($record,3071,4)
		$RecordEnd4 = StringMid($record,4095,4)
		$RecordEnd5 = StringMid($record,5119,4)
		$RecordEnd6 = StringMid($record,6143,4)
		$RecordEnd7 = StringMid($record,7167,4)
		$RecordEnd8 = StringMid($record,8191,4)
		If $UpdSeqArrPart0 <> $RecordEnd1 OR $UpdSeqArrPart0 <> $RecordEnd2 OR $UpdSeqArrPart0 <> $RecordEnd3 OR $UpdSeqArrPart0 <> $RecordEnd4 OR $UpdSeqArrPart0 <> $RecordEnd5 OR $UpdSeqArrPart0 <> $RecordEnd6 OR $UpdSeqArrPart0 <> $RecordEnd7 OR $UpdSeqArrPart0 <> $RecordEnd8 Then
;			_DebugOut($FileRef & " The record failed Fixup", $record)
			Return ""
		Else
			Return StringMid($record,1,1022) & $UpdSeqArrPart1 & StringMid($record,1027,1020) & $UpdSeqArrPart2 & StringMid($record,2051,1020) & $UpdSeqArrPart3 & StringMid($record,3075,1020) & $UpdSeqArrPart4 & StringMid($record,4099,1020) & $UpdSeqArrPart5 & StringMid($record,5123,1020) & $UpdSeqArrPart6 & StringMid($record,6147,1020) & $UpdSeqArrPart7 & StringMid($record,7171,1020) & $UpdSeqArrPart8
		EndIf
	EndIf
EndFunc

Func _GenRefArray()
	Local $nBytes, $MFTClustersToKeep=0, $DoKeepCluster=0, $Subtr, $ArrSize, $BytesToGet=0
	Local $rBuffer = DllStructCreate("byte["&$MFT_Record_Size&"]")
	Global $SplitMftRecArr[1]
	Local $hFile = _WinAPI_CreateFile($TargetDrive, 2, 2, 7)
	If $hFile = 0 Then
		ConsoleWrite("Error CreateFile: " & _WinAPI_GetLastErrorMessage() & " for " & $TargetDrive & @CRLF)
		Return SetError(1,0,0)
	EndIf
	$ref = -1
	$begin = TimerInit()
	For $r = 1 To Ubound($MFT_RUN_VCN)-1
;		ConsoleWrite("$r: " & $r & @CRLF)
		$DoKeepCluster=$MFTClustersToKeep
		$MFTClustersToKeep = Mod($MFT_RUN_Clusters[$r]+($ClustersPerFileRecordSegment-$MFTClustersToKeep),$ClustersPerFileRecordSegment)
		If $MFTClustersToKeep <> 0 Then
			$MFTClustersToKeep = $ClustersPerFileRecordSegment - $MFTClustersToKeep ;How many clusters are we missing to get the full MFT record
		EndIf
		$Pos = $MFT_RUN_VCN[$r]*$BytesPerCluster
		_WinAPI_SetFilePointerEx($hFile, $ImageOffset+$Pos, $FILE_BEGIN)
		;This needs to be verified:
		If $MFTClustersToKeep Or $DoKeepCluster Then
			$Subtr = 0
		Else
			$Subtr = $MFT_Record_Size
		EndIf
		$EndOfRun = $MFT_RUN_Clusters[$r]*$BytesPerCluster-$Subtr
		For $i = 0 To $MFT_RUN_Clusters[$r]*$BytesPerCluster-$Subtr Step $MFT_Record_Size
			If $MFTClustersToKeep Then
				If $i >= $EndOfRun-(($ClustersPerFileRecordSegment-$MFTClustersToKeep)*$BytesPerCluster) Then
					$BytesToGet = ($ClustersPerFileRecordSegment-$MFTClustersToKeep)*$BytesPerCluster
;					$CurrentOffset = DllCall('kernel32.dll', 'int', 'SetFilePointerEx', 'ptr', $hDisk, 'int64', 0, 'int64*', 0, 'dword', 1)
					_WinAPI_ReadFile($hFile, DllStructGetPtr($rBuffer), $BytesToGet, $nBytes)
					;$TmpRecord = StringMid(DllStructGetData($rBuffer, 1),1, 2+($BytesToGet*2))
					$ArrSize = UBound($SplitMftRecArr)
					ReDim $SplitMftRecArr[$ArrSize+1]
;					$SplitMftRecArr[$ArrSize] = $ref+1 & '?' & $CurrentOffset[3] & ',' & $BytesToGet
					$SplitMftRecArr[$ArrSize] = $ref+1 & '?' & ($Pos + $i) & ',' & $BytesToGet
					ContinueLoop
				EndIf
			EndIf
			$ref += 1
;			ConsoleWrite("$ref: " & $ref & @CRLF)
			If $i = 0 And $DoKeepCluster Then
				;If $TmpRecord <> "" Then $record = $TmpRecord
				$BytesToGet = $DoKeepCluster*$BytesPerCluster
				if $BytesToGet > $MFT_Record_Size Then
					MsgBox(0,"Error","$BytesToGet > $MFT_Record_Size")
					$BytesToGet = $MFT_Record_Size
				EndIf
				;$CurrentOffset = DllCall('kernel32.dll', 'int', 'SetFilePointerEx', 'ptr', $hFile, 'int64', 0, 'int64*', 0, 'dword', 1)
				DllCall('kernel32.dll', 'int', 'SetFilePointerEx', 'ptr', $hFile, 'int64', 0, 'int64*', 0, 'dword', 1)
				_WinAPI_ReadFile($hFile, DllStructGetPtr($rBuffer), $BytesToGet, $nBytes)
				;$record &= StringMid(DllStructGetData($rBuffer, 1),3, $BytesToGet*2)
				;$TmpRecord=""
;				ConsoleWrite(_HexEncode($record) & @CRLF)
;				$SplitMftRecArr[$ArrSize] &= '|' & $CurrentOffset[3] & ',' & $BytesToGet
				$SplitMftRecArr[$ArrSize] &= '|' & ($Pos + $i) & ',' & $BytesToGet
;			Else
;				_WinAPI_SetFilePointerEx($hDisk, $ImageOffset+$Pos+$i+$MFT_Record_Size, $FILE_BEGIN)
			EndIf
;			$FileTree[$ref] = $Pos + $i - $Add
;			If $i = 0 And $DoKeepCluster Then $FileTree[$ref] &= "/" & $ArrSize  ;Mark record as being split across 2 runs
		Next
	Next
	_WinAPI_CloseHandle($hFile)
;	ConsoleWrite("_GenRefArray()2" & @CRLF)
;	_ArrayDisplay($SplitMftRecArr,"$SplitMftRecArr")
EndFunc

Func _GetInputParams()
	Local $TmpOutPath, $TmpImageFile, $TmpDevicePath, $TmpImageVolume, $TmpOutName
	For $i = 1 To $cmdline[0]
		;ConsoleWrite("Param " & $i & ": " & $cmdline[$i] & @CRLF)
		If StringLeft($cmdline[$i],12) = "/DevicePath:" Then $TmpDevicePath = StringMid($cmdline[$i],13)
		If StringLeft($cmdline[$i],12) = "/OutputPath:" Then $TmpOutPath = StringMid($cmdline[$i],13)
		If StringLeft($cmdline[$i],11) = "/ImageFile:" Then $TmpImageFile = StringMid($cmdline[$i],12)
		If StringLeft($cmdline[$i],13) = "/ImageVolume:" Then $TmpImageVolume = StringMid($cmdline[$i],14)
		If StringLeft($cmdline[$i],12) = "/OutputName:" Then $TmpOutName = StringMid($cmdline[$i],13)
	Next
	If $cmdline[0] = 0 Then
		_PrintHelp()
		Exit
	EndIf

	If StringLen($TmpOutPath) > 0 Then
		If FileExists($TmpOutPath) Then
			$OutPutPath = $TmpOutPath
		Else
			$OutPutPath = @ScriptDir
		EndIf
	Else
		$OutPutPath = @ScriptDir
	EndIf

	If StringLen($TmpOutName) > 0 Then
		$OutPutName = $TmpOutName
		If StringInStr($OutPutName, "\") Then
			$OutPutName = _GetFilenameFromPath($OutPutName)
		EndIf
		$OutPutName = _FixWindowsFilename($OutPutName)
	EndIf

	If StringLen($TmpImageVolume) > 0 Then
		If Not StringIsDigit($TmpImageVolume) Then
			ConsoleWrite("Error: ImageVolume must be a digit starting from 1" & @CRLF)
			_PrintHelp()
			Exit
		EndIf
	EndIf

	If StringLen($TmpImageFile) > 0 Then
		If FileExists($TmpImageFile) Then
			$IsImage=1
			$TargetImageFile = $TmpImageFile
			$NtfsCheck = _ProcessImage($TargetImageFile)
			If Not $NtfsCheck Then
				ConsoleWrite("Sorry, no NTFS volume found in that file." & @CRLF)
				$IsImage=0
				$TargetImageFile = ""
			Else
				If $TmpImageVolume > UBound($VolumesArray)-1 Then
					ConsoleWrite("Error: Volume " & $TmpImageVolume & " does not exist in image." & @CRLF)
					ConsoleWrite("Found volumes are:" & @CRLF)
					For $i = 1 To UBound($VolumesArray)-1
						ConsoleWrite("Volume " & $i & ", StartOffset " & $VolumesArray[$i][1] & ", Size " & Round(($VolumesArray[$i][2]*512)/1024/1024/1024,2) & "GB" & @CRLF)
					Next
					_PrintHelp()
					Exit
				EndIf
				$ImageOffset = $VolumesArray[$TmpImageVolume][1]
				$TargetDrive = $TargetImageFile
				;ConsoleWrite("$Entries: " & $Entries & @CRLF)
				;_ArrayDisplay($VolumesArray,"$VolumesArray")
			EndIf
		Else
			ConsoleWrite("Error: Image file not found: " & $TmpImageFile & @CRLF)
			Exit
		EndIf
	EndIf

	If StringLen($TmpDevicePath) > 0 Then
		If StringIsAlpha(StringMid($TmpDevicePath,StringLen($TmpDevicePath)-1,1)) And StringRight($TmpDevicePath,1)=":" Then
			$IsPhysicalDrive=True
		EndIf
		If StringInStr($TmpDevicePath,"Harddisk") And StringInStr($TmpDevicePath,"Partition") Then
			$IsPhysicalDrive=True
		EndIf
		If StringInStr($TmpDevicePath,"PhysicalDrive") Then
			$IsPhysicalDrive=True
			$TargetImageFile = $TmpDevicePath
			$TargetDrive = $TmpDevicePath
			$NtfsCheck = _ProcessImage($TargetDrive)
			If Not $NtfsCheck Then
				ConsoleWrite("Sorry, no NTFS volume found in that file." & @CRLF)
				$IsImage=0
				$TargetImageFile = ""
			Else
				If $TmpImageVolume > UBound($VolumesArray)-1 Then
					ConsoleWrite("Error: Volume " & $TmpImageVolume & " does not exist in image." & @CRLF)
					ConsoleWrite("Found volumes are:" & @CRLF)
					For $i = 1 To UBound($VolumesArray)-1
						ConsoleWrite("Volume " & $i & ", StartOffset " & $VolumesArray[$i][1] & ", Size " & Round(($VolumesArray[$i][2]*512)/1024/1024/1024,2) & "GB" & @CRLF)
					Next
					_PrintHelp()
					Exit
				EndIf
				$ImageOffset = $VolumesArray[$TmpImageVolume][1]
				$TargetDrive = $TargetImageFile
				;ConsoleWrite("$Entries: " & $Entries & @CRLF)
				;_ArrayDisplay($VolumesArray,"$VolumesArray")
			EndIf
		EndIf
		If StringInStr($TmpDevicePath,"HarddiskVolume") Then
			$IsPhysicalDrive=True
		EndIf
		If StringInStr($TmpDevicePath,"HarddiskVolumeShadowCopy") Then
			$IsPhysicalDrive=True
		EndIf
		If $IsPhysicalDrive Then
			$TargetDrive = $TmpDevicePath
		EndIf
		If StringMid($TargetDrive,1,4) <> "\\.\" Then
			$TargetDrive = "\\.\" & $TargetDrive
		EndIf
	ElseIf $IsImage=0 Then
		ConsoleWrite("Error: DevicePath param not specified" & @CRLF)
		Exit
	EndIf
EndFunc

Func _PrintHelp()
	ConsoleWrite("Syntax:" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /ImageFile:FullPath\ImageFilename /ImageVolume:[1,2...n] /DevicePath:DevicePath /OutputPath:FullPath /OutputName:FileName" & @CRLF)
	ConsoleWrite("Examples:" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /ImageFile:e:\images\disk.dd /ImageVolume:1 /OutputPath:e:\temp" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /ImageFile:e:\images\disk.dd /ImageVolume:1 /OutputPath:e:\temp /OutputName:$UsnJrnl_vol1.bin" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /DevicePath:c:" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /DevicePath:\\.\HarddiskVolumeShadowCopy1 /OutputPath:e:\temp" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /DevicePath:\\.\HarddiskVolumeShadowCopy24 /OutputPath:e:\temp /OutputName:SC24_$UsnJrnl" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /DevicePath:\\.\Harddisk0Partition2 /OutputPath:e:\temp" & @CRLF)
	ConsoleWrite("ExtractUsnJrnl /DevicePath:\\.\PhysicalDrive0 /ImageVolume:3 /OutputPath:e:\temp" & @CRLF)
EndFunc

Func _ProcessImage($TargetImageFile)
	If Not $IsPhysicalDrive Then
		If Not FileExists($TargetImageFile) Then
			ConsoleWrite("Error: Image file not found: " & $TargetImageFile & @CRLF)
			Return
		EndIf
	EndIf
	If StringMid($TargetImageFile,1,4) <> "\\.\" Then
		$TargetImageFile = "\\.\" & $TargetImageFile
	EndIf
	$Entries = ''
	_CheckMBR($TargetImageFile)
	If $Entries = "" Then
		Return 0
	Else
		Return 1
	EndIf
EndFunc   ;==>_ProcessImage

Func _CheckMBR($TargetImageFile)
	Local $nbytes, $PartitionNumber, $PartitionEntry,$FilesystemDescriptor
	Local $StartingSector,$NumberOfSectors
	;Local $hImage = _WinAPI_CreateFile($TargetImageFile,2,2,2)
	Local $hImage = _WinAPI_CreateFileEx($TargetImageFile, $OPEN_EXISTING, $GENERIC_ALL, BitOR($FILE_SHARE_READ,$FILE_SHARE_WRITE),$FILE_ATTRIBUTE_NORMAL)
	$tBuffer = DllStructCreate("byte[512]")
	Local $read = _WinAPI_ReadFile($hImage, DllStructGetPtr($tBuffer), 512, $nBytes)
	If $read = 0 Then Return ""
	Local $sector = DllStructGetData($tBuffer, 1)
	;ConsoleWrite(_HexEncode($sector) & @CRLF)
	For $PartitionNumber = 0 To 3
		$PartitionEntry = StringMid($sector,($PartitionNumber*32)+3+892,32)
		If $PartitionEntry = "00000000000000000000000000000000" Then ExitLoop ; No more entries
		$FilesystemDescriptor = StringMid($PartitionEntry,9,2)
		$StartingSector = Dec(_SwapEndian(StringMid($PartitionEntry,17,8)),2)
		$NumberOfSectors = Dec(_SwapEndian(StringMid($PartitionEntry,25,8)),2)
		If ($FilesystemDescriptor = "EE" and $StartingSector = 1 and $NumberOfSectors = 4294967295) Then ; A typical dummy partition to prevent overwriting of GPT data, also known as "protective MBR"
			_CheckGPT($hImage)
		ElseIf $FilesystemDescriptor = "05" Or $FilesystemDescriptor = "0F" Then ;Extended partition
			_CheckExtendedPartition($StartingSector, $hImage)
		Else
			If Not _TestNTFS($hImage, $StartingSector) Then
				ReDim $VolumesArray[UBound($VolumesArray)+1][3]
				$VolumesArray[UBound($VolumesArray)-1][0] = "Non-NTFS"
				$VolumesArray[UBound($VolumesArray)-1][1] = $StartingSector
				$VolumesArray[UBound($VolumesArray)-1][2] = $NumberOfSectors
				ContinueLoop
			Else
				$Entries &= _GenComboDescription($StartingSector,$NumberOfSectors)
			EndIf
		EndIf
    Next
	If $Entries = "" Then ;Also check if pure partition image (without mbr)
		$NtfsVolumeSize = _TestNTFS($hImage, 0)
		If $NtfsVolumeSize Then $Entries = _GenComboDescription(0,$NtfsVolumeSize)
	EndIf
	_WinAPI_CloseHandle($hImage)
EndFunc   ;==>_CheckMBR

Func _CheckGPT($hImage) ; Assume GPT to be present at sector 1, which is not fool proof
   ;Actually it is. While LBA1 may not be at sector 1 on the disk, it will always be there in an image.
   ;ConsoleWrite("_CheckGPT()" & @CRLF)
	Local $nbytes,$read,$sector,$GPTSignature,$StartLBA,$Processed=0,$FirstLBA,$LastLBA
	$tBuffer = DllStructCreate("byte[512]")
	$read = _WinAPI_ReadFile($hImage, DllStructGetPtr($tBuffer), 512, $nBytes)		;read second sector
	If $read = 0 Then Return ""
	$sector = DllStructGetData($tBuffer, 1)
	$GPTSignature = StringMid($sector,3,16)
	If $GPTSignature <> "4546492050415254" Then
		ConsoleWrite("Error: Could not find GPT signature: " & _HexEncode(StringMid($sector,3)) & @CRLF)
		Return
	EndIf
	$StartLBA = Dec(_SwapEndian(StringMid($sector,147,16)),2)
	$PartitionsInArray = Dec(_SwapEndian(StringMid($sector,163,8)),2)
	$PartitionEntrySize = Dec(_SwapEndian(StringMid($sector,171,8)),2)
	_WinAPI_SetFilePointerEx($hImage, $StartLBA*512, $FILE_BEGIN)
	$SizeNeeded = $PartitionsInArray*$PartitionEntrySize ;Set buffer size -> maximum number of partition entries that can fit in the array
	$tBuffer = DllStructCreate("byte[" & $SizeNeeded & "]")
	$read = _WinAPI_ReadFile($hImage, DllStructGetPtr($tBuffer), $SizeNeeded, $nBytes)
	If $read = 0 Then Return ""
	$sector = DllStructGetData($tBuffer, 1)
	Do
		$FirstLBA = Dec(_SwapEndian(StringMid($sector,67+($Processed*2),16)),2)
		$LastLBA = Dec(_SwapEndian(StringMid($sector,83+($Processed*2),16)),2)
		If $FirstLBA = 0 And $LastLBA = 0 Then ExitLoop ; No more entries
		$Processed += $PartitionEntrySize
		#cs
		If Not _TestNTFS($hImage, $FirstLBA) Then
			ContinueLoop ;Continue the loop if filesystem not NTFS
		EndIf
		$Entries &= _GenComboDescription($FirstLBA,$LastLBA-$FirstLBA)
		#ce
		If Not _TestNTFS($hImage, $FirstLBA) Then
			ReDim $VolumesArray[UBound($VolumesArray)+1][3]
			$VolumesArray[UBound($VolumesArray)-1][0] = "Non-NTFS"
			$VolumesArray[UBound($VolumesArray)-1][1] = $FirstLBA
			$VolumesArray[UBound($VolumesArray)-1][2] = $LastLBA-$FirstLBA
			ContinueLoop
		Else
			$Entries &= _GenComboDescription($FirstLBA,$LastLBA-$FirstLBA)
		EndIf
	Until $Processed >= $SizeNeeded
EndFunc   ;==>_CheckGPT

Func _CheckExtendedPartition($StartSector, $hImage)	;Extended partitions can only contain Logical Drives, but can be more than 4
	Local $nbytes,$read,$sector,$NextEntry=0,$StartingSector,$NumberOfSectors,$PartitionTable,$FilesystemDescriptor
	$tBuffer = DllStructCreate("byte[512]")
	While 1
		_WinAPI_SetFilePointerEx($hImage, ($StartSector + $NextEntry) * 512, $FILE_BEGIN)
		$read = _WinAPI_ReadFile($hImage, DllStructGetPtr($tBuffer), 512, $nBytes)
		If $read = 0 Then Return ""
		$sector = DllStructGetData($tBuffer, 1)
		;ConsoleWrite(_HexEncode($sector) & @CRLF)
		$PartitionTable = StringMid($sector,3+892,64)
		$FilesystemDescriptor = StringMid($PartitionTable,9,2)
		$StartingSector = $StartSector+$NextEntry+Dec(_SwapEndian(StringMid($PartitionTable,17,8)),2)
		$NumberOfSectors = Dec(_SwapEndian(StringMid($PartitionTable,25,8)),2)
		If $FilesystemDescriptor = "06" Or $FilesystemDescriptor = "07" Then
			If Not _TestNTFS($hImage, $StartingSector) Then
				ReDim $VolumesArray[UBound($VolumesArray)+1][3]
				$VolumesArray[UBound($VolumesArray)-1][0] = "Non-NTFS"
				$VolumesArray[UBound($VolumesArray)-1][1] = $StartingSector
				$VolumesArray[UBound($VolumesArray)-1][2] = $NumberOfSectors
			Else
				$Entries &= _GenComboDescription($StartingSector,$NumberOfSectors)
			EndIf
		ElseIf $FilesystemDescriptor <> "05" And $FilesystemDescriptor <> "0F" Then
			ReDim $VolumesArray[UBound($VolumesArray)+1][3]
			$VolumesArray[UBound($VolumesArray)-1][0] = "Non-NTFS"
			$VolumesArray[UBound($VolumesArray)-1][1] = $StartingSector
			$VolumesArray[UBound($VolumesArray)-1][2] = $NumberOfSectors
		EndIf
		If StringMid($PartitionTable,33) = "00000000000000000000000000000000" Then ExitLoop ; No more entries
		$NextEntry = Dec(_SwapEndian(StringMid($PartitionTable,49,8)),2)
	WEnd
EndFunc   ;==>_CheckExtendedPartition

Func _TestNTFS($hImage, $PartitionStartSector)
	Local $nbytes, $TotalSectors
	If $PartitionStartSector <> 0 Then
		_WinAPI_SetFilePointerEx($hImage, $PartitionStartSector*512, $FILE_BEGIN)
	Else
		_WinAPI_CloseHandle($hImage)
		$hImage = _WinAPI_CreateFile($TargetImageFile,2,2,7)
	EndIf
	$tBuffer = DllStructCreate("byte[512]")
	$read = _WinAPI_ReadFile($hImage, DllStructGetPtr($tBuffer), 512, $nBytes)
	If $read = 0 Then Return ""
	$sector = DllStructGetData($tBuffer, 1)
	$TestSig = StringMid($sector,9,8)
	$TotalSectors = Dec(_SwapEndian(StringMid($sector,83,8)),2)
	If $TestSig = "4E544653" Then
		ReDim $VolumesArray[UBound($VolumesArray)+1][3]
		$VolumesArray[UBound($VolumesArray)-1][0] = "NTFS"
		$VolumesArray[UBound($VolumesArray)-1][1] = $PartitionStartSector*512
		$VolumesArray[UBound($VolumesArray)-1][2] = $TotalSectors
		Return $TotalSectors		; Volume is NTFS
	EndIf
	ConsoleWrite("Error: Could not find NTFS on " & $TargetImageFile & " at offset " & $PartitionStartSector*512 & @CRLF)
    Return 0
EndFunc   ;==>_TestNTFS

Func _GenComboDescription($StartSector,$SectorNumber)
	Return "Offset = " & $StartSector*512 & ": Volume size = " & Round(($SectorNumber*512)/1024/1024/1024,2) & " GB|"
EndFunc

Func _GetFilenameFromPath($FileNamePath)
	$stringlength = StringLen($FileNamePath)
	If $stringlength = 0 Then Return SetError(1,0,0)
	$TmpOffset = StringInStr($FileNamePath, "\", 1, -1)
	If $TmpOffset = 0 Then Return $FileNamePath
	Return StringMid($FileNamePath,$TmpOffset+1)
EndFunc

Func _FixWindowsFilename($input)
	$input = StringReplace($input, "/", "")
	$input = StringReplace($input, "\", "")
	$input = StringReplace($input, ":", "")
	$input = StringReplace($input, "*", "")
	$input = StringReplace($input, "?", "")
	$input = StringReplace($input, '"', "")
	$input = StringReplace($input, "<", "")
	$input = StringReplace($input, ">", "")
	Return $input
EndFunc