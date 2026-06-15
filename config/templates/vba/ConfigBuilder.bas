Attribute VB_Name = "ConfigBuilder"
Option Explicit

Private Const START_SHEET As String = "START HERE"
Private Const RELEASE_SHEET As String = "Release Setup"
Private Const CHARTS_SHEET As String = "Charts"
Private Const DATA_SOURCES_SHEET As String = "Data Sources"
Private Const DATA_ARGS_SHEET As String = "Data Args"
Private Const SECTOR_SHEET As String = "Sector Defaults"
Private Const CHART_DEFAULTS_SHEET As String = "Chart Defaults"
Private Const RUN_CONTROL_SHEET As String = "Run Control"
Private Const VALIDATION_SHEET As String = "Validation"

Private Const TECH_RELEASE As String = "release_settings"
Private Const TECH_SECTOR As String = "settings_sector"
Private Const TECH_PLOTS As String = "plots"
Private Const TECH_PLOT_ARGS As String = "plot_args"
Private Const TECH_DATA_SOURCES As String = "data_sources"
Private Const TECH_DATA_ARGS As String = "data_args"
Private Const TECH_RUN_CONTROL As String = "run_control"
Private Const TECH_PALETTES As String = "palettes"

Public Sub InstallBuilderButtons()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Worksheets(START_SHEET)

    On Error Resume Next
    ws.Buttons.Delete
    On Error GoTo 0

    AddButton ws, "Build R Config", "BuildRConfig", 20, 250, 150, 28
    AddButton ws, "Export Chart Config", "ExportChartConfigXlsx", 180, 250, 170, 28

    MsgBox "Buttons installed on START HERE.", vbInformation
End Sub

Public Sub BuildRConfig()
    Application.ScreenUpdating = False

    ResetSheet TECH_RELEASE, Array("setting_name", "setting_value", "setting_type", "notes")
    ResetSheet TECH_SECTOR, Array("sector", "active", "palette", "output_subfolder", "notes")
    ResetSheet TECH_PLOTS, Array("plot_id", "sector", "active", "plot_function", "data_source_id", "output_file", "title", "subtitle", "sort_order", "notes")
    ResetSheet TECH_PLOT_ARGS, Array("plot_id", "arg_name", "arg_value", "arg_type", "notes")
    ResetSheet TECH_DATA_SOURCES, Array("data_source_id", "source_type", "source_ref", "sheet", "range", "data_function", "cache", "notes")
    ResetSheet TECH_DATA_ARGS, Array("data_source_id", "arg_name", "arg_value", "arg_type", "notes")
    ResetSheet TECH_RUN_CONTROL, Array("setting_name", "setting_value", "setting_type", "notes")
    ResetSheet TECH_PALETTES, Array("palette", "item", "hex", "notes")
    ResetSheet VALIDATION_SHEET, Array("Level", "Sheet", "Row", "Message")

    BuildReleaseSettings
    BuildSectorSettings
    BuildDataSources
    BuildDataArgs
    BuildRunControl
    BuildPlotsAndPlotArgs

    HideTechnicalSheets
    Application.ScreenUpdating = True

    MsgBox "Technical config sheets have been rebuilt.", vbInformation
End Sub

Public Sub ExportChartConfigXlsx()
    Dim releaseYear As String
    Dim releaseRound As String
    Dim outDir As String
    Dim outPath As String
    Dim wbOut As Workbook
    Dim sheetNames As Variant
    Dim i As Long

    BuildRConfig

    releaseYear = CStr(GetReleaseValue("release_year"))
    releaseRound = CleanPathPart(CStr(GetReleaseValue("release_round")))
    If Len(releaseYear) = 0 Or Len(releaseRound) = 0 Then
        MsgBox "Release year and release round are required before export.", vbExclamation
        Exit Sub
    End If

    outDir = ConfigRootPath() & "\releases\" & releaseYear & "\" & releaseRound
    EnsureFolder outDir
    outPath = outDir & "\chart_config.xlsx"

    sheetNames = TechnicalSheetNames()

    Set wbOut = Workbooks.Add(xlWBATWorksheet)
    Application.DisplayAlerts = False

    For i = LBound(sheetNames) To UBound(sheetNames)
        ThisWorkbook.Worksheets(CStr(sheetNames(i))).Visible = xlSheetVisible
        ThisWorkbook.Worksheets(CStr(sheetNames(i))).Copy After:=wbOut.Sheets(wbOut.Sheets.Count)
    Next i

    Do While wbOut.Worksheets.Count > UBound(sheetNames) - LBound(sheetNames) + 1
        wbOut.Worksheets(1).Delete
    Loop

    wbOut.SaveAs Filename:=outPath, FileFormat:=xlOpenXMLWorkbook
    wbOut.Close SaveChanges:=False
    Application.DisplayAlerts = True

    HideTechnicalSheets

    MsgBox "Exported " & outPath, vbInformation
End Sub

Private Sub BuildReleaseSettings()
    CopySettingSheet RELEASE_SHEET, TECH_RELEASE
End Sub

Private Sub BuildRunControl()
    CopySettingSheet RUN_CONTROL_SHEET, TECH_RUN_CONTROL
End Sub

Private Sub BuildSectorSettings()
    Dim src As Worksheet
    Dim dst As Worksheet
    Dim map As Object
    Dim r As Long
    Dim outRow As Long
    Dim last As Long
    Dim sectorName As String

    Set src = ThisWorkbook.Worksheets(SECTOR_SHEET)
    Set dst = ThisWorkbook.Worksheets(TECH_SECTOR)
    Set map = HeaderMap(src, 1)
    last = LastRow(src, 1)
    outRow = 2

    For r = 2 To last
        sectorName = CleanText(CellByHeader(src, r, map, "Sector"))
        If Len(sectorName) > 0 Then
            dst.Cells(outRow, 1).Value = sectorName
            dst.Cells(outRow, 2).Value = CellByHeader(src, r, map, "Active")
            dst.Cells(outRow, 3).Value = CellByHeader(src, r, map, "Palette")
            dst.Cells(outRow, 4).Value = CellByHeader(src, r, map, "Output Subfolder")
            dst.Cells(outRow, 5).Value = CellByHeader(src, r, map, "Notes")
            outRow = outRow + 1
        End If
    Next r
End Sub

Private Sub BuildDataSources()
    Dim src As Worksheet
    Dim dst As Worksheet
    Dim map As Object
    Dim r As Long
    Dim outRow As Long
    Dim last As Long
    Dim id As String

    Set src = ThisWorkbook.Worksheets(DATA_SOURCES_SHEET)
    Set dst = ThisWorkbook.Worksheets(TECH_DATA_SOURCES)
    Set map = HeaderMap(src, 1)
    last = LastRow(src, 1)
    outRow = 2

    For r = 2 To last
        id = CleanText(CellByHeader(src, r, map, "Data Source ID"))
        If Len(id) > 0 Then
            dst.Cells(outRow, 1).Value = id
            dst.Cells(outRow, 2).Value = CellByHeader(src, r, map, "Source Type")
            dst.Cells(outRow, 3).Value = CellByHeader(src, r, map, "Source Ref")
            dst.Cells(outRow, 4).Value = CellByHeader(src, r, map, "Sheet")
            dst.Cells(outRow, 5).Value = CellByHeader(src, r, map, "Range")
            dst.Cells(outRow, 6).Value = CellByHeader(src, r, map, "Data Function")
            dst.Cells(outRow, 7).Value = CellByHeader(src, r, map, "Cache")
            dst.Cells(outRow, 8).Value = CellByHeader(src, r, map, "Notes")
            outRow = outRow + 1
        End If
    Next r
End Sub

Private Sub BuildDataArgs()
    Dim src As Worksheet
    Dim dst As Worksheet
    Dim map As Object
    Dim r As Long
    Dim outRow As Long
    Dim last As Long
    Dim id As String
    Dim argName As String

    Set src = ThisWorkbook.Worksheets(DATA_ARGS_SHEET)
    Set dst = ThisWorkbook.Worksheets(TECH_DATA_ARGS)
    Set map = HeaderMap(src, 1)
    last = LastRow(src, 1)
    outRow = 2

    For r = 2 To last
        id = CleanText(CellByHeader(src, r, map, "Data Source ID"))
        argName = CleanText(CellByHeader(src, r, map, "Arg Name"))
        If Len(id) > 0 And Len(argName) > 0 Then
            dst.Cells(outRow, 1).Value = id
            dst.Cells(outRow, 2).Value = argName
            dst.Cells(outRow, 3).Value = CellByHeader(src, r, map, "Arg Value")
            dst.Cells(outRow, 4).Value = CellByHeader(src, r, map, "Arg Type")
            dst.Cells(outRow, 5).Value = CellByHeader(src, r, map, "Notes")
            outRow = outRow + 1
        End If
    Next r
End Sub

Private Sub BuildPlotsAndPlotArgs()
    Dim src As Worksheet
    Dim plots As Worksheet
    Dim args As Worksheet
    Dim map As Object
    Dim r As Long
    Dim last As Long
    Dim plotRow As Long
    Dim argRow As Long
    Dim plotId As String
    Dim plotFunction As String
    Dim usedArgs As Object

    Set src = ThisWorkbook.Worksheets(CHARTS_SHEET)
    Set plots = ThisWorkbook.Worksheets(TECH_PLOTS)
    Set args = ThisWorkbook.Worksheets(TECH_PLOT_ARGS)
    Set map = HeaderMap(src, 1)
    last = LastRow(src, 1)
    plotRow = 2
    argRow = 2

    For r = 2 To last
        If AsBoolean(CellByHeader(src, r, map, "Include")) Then
            plotId = CleanText(CellByHeader(src, r, map, "Plot ID"))
            plotFunction = CleanText(CellByHeader(src, r, map, "Plot Function"))

            If Len(plotId) = 0 Then AddValidation "Error", CHARTS_SHEET, r, "Plot ID is required."
            If Len(CleanText(CellByHeader(src, r, map, "Sector"))) = 0 Then AddValidation "Error", CHARTS_SHEET, r, "Sector is required."
            If Len(plotFunction) = 0 Then AddValidation "Error", CHARTS_SHEET, r, "Plot Function is required."
            If Len(CleanText(CellByHeader(src, r, map, "Data Source ID"))) = 0 Then AddValidation "Error", CHARTS_SHEET, r, "Data Source ID is required."
            If Len(CleanText(CellByHeader(src, r, map, "Output File"))) = 0 Then AddValidation "Error", CHARTS_SHEET, r, "Output File is required."

            plots.Cells(plotRow, 1).Value = plotId
            plots.Cells(plotRow, 2).Value = CellByHeader(src, r, map, "Sector")
            plots.Cells(plotRow, 3).Value = True
            plots.Cells(plotRow, 4).Value = plotFunction
            plots.Cells(plotRow, 5).Value = CellByHeader(src, r, map, "Data Source ID")
            plots.Cells(plotRow, 6).Value = CellByHeader(src, r, map, "Output File")
            plots.Cells(plotRow, 7).Value = CellByHeader(src, r, map, "Title")
            plots.Cells(plotRow, 8).Value = CellByHeader(src, r, map, "Subtitle")
            plots.Cells(plotRow, 9).Value = CellByHeader(src, r, map, "Sort Order")
            plots.Cells(plotRow, 10).Value = ""
            plotRow = plotRow + 1

            Set usedArgs = CreateObject("Scripting.Dictionary")
            AddPlotArg args, argRow, usedArgs, plotId, "x", CellByHeader(src, r, map, "X Field"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "x_freq", CellByHeader(src, r, map, "X Frequency"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "group", CellByHeader(src, r, map, "Group Field"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "y_col", CellByHeader(src, r, map, "Column Value"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "y_col_label", CellByHeader(src, r, map, "Column Axis Label"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "col_label", CellByHeader(src, r, map, "Column Legend Label"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "y_line", CellByHeader(src, r, map, "Line Value"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "y_line_label", CellByHeader(src, r, map, "Line Axis Label"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "line_label", CellByHeader(src, r, map, "Line Legend Label"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "col_position", CellByHeader(src, r, map, "Column Position"), "character", ""
            AddPlotArg args, argRow, usedArgs, plotId, "forecast", CellByHeader(src, r, map, "Forecast"), "logical", ""
            AddPlotArg args, argRow, usedArgs, plotId, "use_metadata_palette", CellByHeader(src, r, map, "Use Metadata Palette"), "logical", ""
            AddPlotArg args, argRow, usedArgs, plotId, "primary_min_breaks", CellByHeader(src, r, map, "Primary Min Breaks"), "integer", ""
            AddPlotArg args, argRow, usedArgs, plotId, "primary_max_breaks", CellByHeader(src, r, map, "Primary Max Breaks"), "integer", ""
            AddPlotArg args, argRow, usedArgs, plotId, "secondary_min_breaks", CellByHeader(src, r, map, "Secondary Min Breaks"), "integer", ""
            AddPlotArg args, argRow, usedArgs, plotId, "secondary_max_breaks", CellByHeader(src, r, map, "Secondary Max Breaks"), "integer", ""

            AddChartDefaults args, argRow, usedArgs, plotId, plotFunction
        End If
    Next r
End Sub

Private Sub AddChartDefaults(ByVal argsSheet As Worksheet, ByRef argRow As Long, ByVal usedArgs As Object, ByVal plotId As String, ByVal plotFunction As String)
    Dim src As Worksheet
    Dim map As Object
    Dim r As Long
    Dim last As Long
    Dim argName As String

    Set src = ThisWorkbook.Worksheets(CHART_DEFAULTS_SHEET)
    Set map = HeaderMap(src, 1)
    last = LastRow(src, 1)

    For r = 2 To last
        If CleanText(CellByHeader(src, r, map, "Plot Function")) = plotFunction Then
            argName = CleanText(CellByHeader(src, r, map, "Arg Name"))
            If Len(argName) > 0 And Not usedArgs.Exists(argName) Then
                AddPlotArg argsSheet, argRow, usedArgs, plotId, argName, CellByHeader(src, r, map, "Arg Value"), CellByHeader(src, r, map, "Arg Type"), CellByHeader(src, r, map, "Notes")
            End If
        End If
    Next r
End Sub

Private Sub AddPlotArg(ByVal ws As Worksheet, ByRef outRow As Long, ByVal usedArgs As Object, ByVal plotId As String, ByVal argName As String, ByVal argValue As Variant, ByVal argType As String, ByVal notes As String)
    If Len(CleanText(argName)) = 0 Then Exit Sub
    If IsEmptyValue(argValue) Then Exit Sub

    ws.Cells(outRow, 1).Value = plotId
    ws.Cells(outRow, 2).Value = argName
    ws.Cells(outRow, 3).Value = argValue
    ws.Cells(outRow, 4).Value = argType
    ws.Cells(outRow, 5).Value = notes
    usedArgs(argName) = True
    outRow = outRow + 1
End Sub

Private Sub CopySettingSheet(ByVal sourceSheetName As String, ByVal targetSheetName As String)
    Dim src As Worksheet
    Dim dst As Worksheet
    Dim r As Long
    Dim outRow As Long
    Dim last As Long

    Set src = ThisWorkbook.Worksheets(sourceSheetName)
    Set dst = ThisWorkbook.Worksheets(targetSheetName)
    last = LastRow(src, 1)
    outRow = 2

    For r = 2 To last
        If Len(CleanText(src.Cells(r, 1).Value)) > 0 Then
            dst.Cells(outRow, 1).Value = src.Cells(r, 1).Value
            dst.Cells(outRow, 2).Value = src.Cells(r, 2).Value
            dst.Cells(outRow, 3).Value = src.Cells(r, 3).Value
            dst.Cells(outRow, 4).Value = src.Cells(r, 4).Value
            outRow = outRow + 1
        End If
    Next r
End Sub

Private Sub ResetSheet(ByVal sheetName As String, ByVal headers As Variant)
    Dim ws As Worksheet
    Dim i As Long

    Set ws = EnsureSheet(sheetName)
    ws.Cells.Clear

    For i = LBound(headers) To UBound(headers)
        ws.Cells(1, i + 1).Value = headers(i)
    Next i

    ws.Rows(1).Font.Bold = True
End Sub

Private Function EnsureSheet(ByVal sheetName As String) As Worksheet
    On Error Resume Next
    Set EnsureSheet = ThisWorkbook.Worksheets(sheetName)
    On Error GoTo 0

    If EnsureSheet Is Nothing Then
        Set EnsureSheet = ThisWorkbook.Worksheets.Add(After:=ThisWorkbook.Worksheets(ThisWorkbook.Worksheets.Count))
        EnsureSheet.Name = sheetName
    End If
End Function

Private Function HeaderMap(ByVal ws As Worksheet, ByVal headerRow As Long) As Object
    Dim dict As Object
    Dim lastCol As Long
    Dim c As Long
    Dim key As String

    Set dict = CreateObject("Scripting.Dictionary")
    lastCol = ws.Cells(headerRow, ws.Columns.Count).End(xlToLeft).Column

    For c = 1 To lastCol
        key = CleanText(ws.Cells(headerRow, c).Value)
        If Len(key) > 0 Then dict(key) = c
    Next c

    Set HeaderMap = dict
End Function

Private Function CellByHeader(ByVal ws As Worksheet, ByVal rowNum As Long, ByVal map As Object, ByVal headerName As String) As Variant
    If map.Exists(headerName) Then
        CellByHeader = ws.Cells(rowNum, CLng(map(headerName))).Value
    Else
        CellByHeader = ""
    End If
End Function

Private Function LastRow(ByVal ws As Worksheet, ByVal colNum As Long) As Long
    LastRow = ws.Cells(ws.Rows.Count, colNum).End(xlUp).Row
End Function

Private Function CleanText(ByVal value As Variant) As String
    If IsError(value) Or IsNull(value) Or IsEmpty(value) Then
        CleanText = ""
    Else
        CleanText = Trim$(CStr(value))
    End If
End Function

Private Function IsEmptyValue(ByVal value As Variant) As Boolean
    If IsError(value) Or IsNull(value) Or IsEmpty(value) Then
        IsEmptyValue = True
    Else
        IsEmptyValue = (Len(Trim$(CStr(value))) = 0)
    End If
End Function

Private Function AsBoolean(ByVal value As Variant) As Boolean
    Dim text As String
    If VarType(value) = vbBoolean Then
        AsBoolean = CBool(value)
    Else
        text = LCase$(Trim$(CStr(value)))
        AsBoolean = (text = "true" Or text = "yes" Or text = "y" Or text = "1")
    End If
End Function

Private Sub AddValidation(ByVal level As String, ByVal sheetName As String, ByVal rowNum As Long, ByVal message As String)
    Dim ws As Worksheet
    Dim nextRow As Long

    Set ws = ThisWorkbook.Worksheets(VALIDATION_SHEET)
    nextRow = LastRow(ws, 1) + 1
    ws.Cells(nextRow, 1).Value = level
    ws.Cells(nextRow, 2).Value = sheetName
    ws.Cells(nextRow, 3).Value = rowNum
    ws.Cells(nextRow, 4).Value = message
End Sub

Private Sub HideTechnicalSheets()
    Dim sheetNames As Variant
    Dim i As Long

    sheetNames = TechnicalSheetNames()
    For i = LBound(sheetNames) To UBound(sheetNames)
        ThisWorkbook.Worksheets(CStr(sheetNames(i))).Visible = xlSheetHidden
    Next i
End Sub

Private Function TechnicalSheetNames() As Variant
    TechnicalSheetNames = Array(TECH_RELEASE, TECH_SECTOR, TECH_PLOTS, TECH_PLOT_ARGS, TECH_DATA_SOURCES, TECH_DATA_ARGS, TECH_RUN_CONTROL, TECH_PALETTES)
End Function

Private Function GetReleaseValue(ByVal settingName As String) As Variant
    Dim ws As Worksheet
    Dim r As Long
    Dim last As Long

    Set ws = ThisWorkbook.Worksheets(TECH_RELEASE)
    last = LastRow(ws, 1)

    For r = 2 To last
        If CleanText(ws.Cells(r, 1).Value) = settingName Then
            GetReleaseValue = ws.Cells(r, 2).Value
            Exit Function
        End If
    Next r

    GetReleaseValue = ""
End Function

Private Function ConfigRootPath() As String
    Dim p As String
    Dim marker As String
    Dim pos As Long

    p = ThisWorkbook.Path
    marker = "\config\"
    pos = InStr(1, LCase$(p), marker, vbTextCompare)

    If pos > 0 Then
        ConfigRootPath = Left$(p, pos + Len(marker) - 2)
    Else
        ConfigRootPath = p
    End If
End Function

Private Function CleanPathPart(ByVal value As String) As String
    Dim badChars As Variant
    Dim i As Long

    badChars = Array("\", "/", ":", "*", "?", """", "<", ">", "|")
    CleanPathPart = Trim$(value)

    For i = LBound(badChars) To UBound(badChars)
        CleanPathPart = Replace(CleanPathPart, CStr(badChars(i)), "_")
    Next i
End Function

Private Sub EnsureFolder(ByVal path As String)
    Dim fso As Object
    Dim parentPath As String

    Set fso = CreateObject("Scripting.FileSystemObject")
    If fso.FolderExists(path) Then Exit Sub

    parentPath = fso.GetParentFolderName(path)
    If Len(parentPath) > 0 And Not fso.FolderExists(parentPath) Then
        EnsureFolder parentPath
    End If

    fso.CreateFolder path
End Sub

Private Sub AddButton(ByVal ws As Worksheet, ByVal caption As String, ByVal macroName As String, ByVal leftPos As Double, ByVal topPos As Double, ByVal widthVal As Double, ByVal heightVal As Double)
    Dim btn As Button
    Set btn = ws.Buttons.Add(leftPos, topPos, widthVal, heightVal)
    btn.Caption = caption
    btn.OnAction = macroName
End Sub
