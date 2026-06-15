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

    AddButton ws, "Add Chart", "ShowChartWizard", 20, 250, 130, 28
    AddButton ws, "Add Data Source", "ShowDataSourceWizard", 160, 250, 140, 28
    AddButton ws, "Refresh Plot Functions", "RefreshPlotFunctionsFromR", 310, 250, 170, 28
    AddButton ws, "Refresh Data Functions", "RefreshDataFunctionsFromR", 490, 250, 170, 28
    AddButton ws, "Build R Config", "BuildRConfig", 670, 250, 130, 28
    AddButton ws, "Export Chart Config", "ExportChartConfigXlsx", 810, 250, 150, 28
    AddButton ws, "Delete Chart", "DeleteChartWizard", 20, 290, 130, 28
    AddButton ws, "Delete Data Source", "DeleteDataSourceWizard", 160, 290, 150, 28
    AddButton ws, "Refresh Plot List", "RefreshStartHerePlotList", 320, 290, 150, 28

    MsgBox "Buttons installed on START HERE.", vbInformation
End Sub

Public Sub ShowChartWizard()
    Dim sectorName As String
    Dim plotId As String
    Dim plotFunction As String
    Dim dataSourceId As String
    Dim outputFile As String
    Dim xField As String
    Dim groupField As String
    Dim columnValue As String
    Dim lineValue As String
    Dim sortOrder As Variant
    Dim fields As Collection
    Dim values(1 To 25) As Variant

    sectorName = PromptSelectFromList("Sector", SopiSectorValues())
    If Len(sectorName) = 0 Then Exit Sub

    plotId = PromptValue("Plot ID", "Enter a unique plot ID.", LCase$(Replace(sectorName, " ", "_")) & "_fig_1")
    If Len(plotId) = 0 Then Exit Sub

    RefreshPlotFunctionsFromR False
    plotFunction = PromptSelectFromList("Plot Function", GetListValues("Lists", "Plot Functions"))
    If Len(plotFunction) = 0 Then Exit Sub

    dataSourceId = PromptSelectFromList("Data Source ID", GetListValues(DATA_SOURCES_SHEET, "Data Source ID"))
    If Len(dataSourceId) = 0 Then Exit Sub

    Set fields = GetDataSourceFieldNames(dataSourceId)

    outputFile = PromptValue("Output File", "Enter SVG output filename.", plotId & ".svg")
    If Len(outputFile) = 0 Then Exit Sub

    xField = PromptSelectField("X Field", fields, FirstExistingField(fields, Array("year", "date", "month"), "year"), False)
    If Len(xField) = 0 Then Exit Sub

    groupField = PromptSelectField("Group Field", fields, FirstExistingField(fields, Array("group", "product", "market", "category"), "group"), True)
    columnValue = PromptSelectField("Column Value", fields, FirstExistingField(fields, Array("revenue", "export_value_nzd", "value"), "revenue"), True)
    lineValue = PromptSelectField("Line Value", fields, FirstExistingField(fields, Array("volume", "export_volume_tonnes"), "volume"), True)

    If Len(columnValue) = 0 And Len(lineValue) = 0 Then
        MsgBox "At least one value field is required.", vbExclamation
        Exit Sub
    End If

    sortOrder = BuilderNextSortOrder(sectorName)

    values(1) = True
    values(2) = plotId
    values(3) = sectorName
    values(4) = plotFunction
    values(5) = dataSourceId
    values(6) = outputFile
    values(7) = ""
    values(8) = ""
    values(9) = sortOrder
    values(10) = xField
    values(11) = "auto"
    values(12) = groupField
    values(13) = columnValue
    values(14) = "Export revenue (NZ$ million)"
    values(15) = "Revenue"
    values(16) = lineValue
    values(17) = "Export volume (tonnes)"
    values(18) = "Volume"
    values(19) = "dodge"
    values(20) = True
    values(21) = True
    values(22) = 4
    values(23) = 6
    values(24) = 4
    values(25) = 6

    If Not WriteOrReplaceRowValues(ThisWorkbook.Worksheets(CHARTS_SHEET), "Plot ID", plotId, values) Then Exit Sub
    RefreshStartHerePlotList

    MsgBox "Chart saved to the Charts sheet.", vbInformation
End Sub

Public Sub ShowDataSourceWizard()
    Dim dataSourceId As String
    Dim sourceType As String
    Dim sourceRef As String
    Dim sourceSheet As String
    Dim dataFunction As String
    Dim values(1 To 8) As Variant

    dataSourceId = PromptValue("Data Source ID", "Enter a unique data source ID.", "")
    If Len(dataSourceId) = 0 Then Exit Sub

    sourceType = LCase$(PromptSelectFromList("Source Type", CollectionFromArray(Array("excel", "function"))))
    If sourceType <> "excel" And sourceType <> "function" Then
        MsgBox "Source Type must be excel or function.", vbExclamation
        Exit Sub
    End If

    If sourceType = "excel" Then
        sourceRef = PromptValue("Source Ref", "Enter workbook path relative to project root.", "data/raw/manual_data.xlsx")
        If Len(sourceRef) = 0 Then Exit Sub

        sourceSheet = PromptSelectFromList("Sheet", GetWorkbookSheetNames(sourceRef))
        If Len(sourceSheet) = 0 Then Exit Sub
    Else
        RefreshDataFunctionsFromR False
        dataFunction = PromptSelectFromList("Data Function", GetListValues("Lists", "Data Functions"))
        If Len(dataFunction) = 0 Then Exit Sub
    End If

    values(1) = dataSourceId
    values(2) = sourceType
    values(3) = sourceRef
    values(4) = sourceSheet
    values(5) = ""
    values(6) = dataFunction
    values(7) = False
    values(8) = ""

    If Not WriteOrReplaceRowValues(ThisWorkbook.Worksheets(DATA_SOURCES_SHEET), "Data Source ID", dataSourceId, values) Then Exit Sub

    MsgBox "Data source saved to the Data Sources sheet.", vbInformation
End Sub

Public Sub DeleteChartWizard()
    Dim plotId As String
    Dim deletedCount As Long

    plotId = PromptSelectFromList("Plot ID to delete", GetListValues(CHARTS_SHEET, "Plot ID"))
    If Len(plotId) = 0 Then Exit Sub

    If MsgBox("Delete chart '" & plotId & "' from the Charts sheet?", vbQuestion + vbYesNo) <> vbYes Then Exit Sub

    deletedCount = DeleteRowsByHeaderValue(ThisWorkbook.Worksheets(CHARTS_SHEET), "Plot ID", plotId)
    RefreshStartHerePlotList
    MsgBox "Deleted " & deletedCount & " chart row(s). Click Build R Config and Export Chart Config to update the release config.", vbInformation
End Sub

Public Sub RefreshStartHerePlotList()
    Dim startWs As Worksheet
    Dim chartsWs As Worksheet
    Dim map As Object
    Dim sectors As Collection
    Dim sectorIndex As Long
    Dim sectorName As String
    Dim outRow As Long

    Set startWs = ThisWorkbook.Worksheets(START_SHEET)
    Set chartsWs = ThisWorkbook.Worksheets(CHARTS_SHEET)
    Set map = HeaderMap(chartsWs, 1)
    Set sectors = SopiSectorValues()

    ClearStartHerePlotList startWs

    outRow = 24
    startWs.Cells(outRow, 1).Value = "Current plots by sector"
    startWs.Cells(outRow, 1).Font.Bold = True
    startWs.Cells(outRow, 1).Font.Size = 13
    outRow = outRow + 2

    For sectorIndex = 1 To sectors.Count
        sectorName = CStr(sectors(sectorIndex))
        outRow = WriteSectorPlotList(startWs, chartsWs, map, sectorName, outRow)
    Next sectorIndex

    startWs.Columns("A:F").AutoFit
End Sub

Private Sub ClearStartHerePlotList(ByVal ws As Worksheet)
    ws.Range("A24:F500").Clear
End Sub

Private Function WriteSectorPlotList( _
    ByVal startWs As Worksheet, _
    ByVal chartsWs As Worksheet, _
    ByVal map As Object, _
    ByVal sectorName As String, _
    ByVal startRow As Long) As Long
    Dim r As Long
    Dim last As Long
    Dim outRow As Long
    Dim count As Long

    outRow = startRow
    last = LastRow(chartsWs, 1)

    For r = 2 To last
        If CleanText(CellByHeader(chartsWs, r, map, "Sector")) = sectorName Then
            count = count + 1
        End If
    Next r

    If count = 0 Then
        WriteSectorPlotList = outRow
        Exit Function
    End If

    startWs.Cells(outRow, 1).Value = sectorName
    startWs.Cells(outRow, 1).Font.Bold = True
    outRow = outRow + 1

    startWs.Cells(outRow, 1).Value = "Include"
    startWs.Cells(outRow, 2).Value = "Plot ID"
    startWs.Cells(outRow, 3).Value = "Plot Function"
    startWs.Cells(outRow, 4).Value = "Data Source"
    startWs.Cells(outRow, 5).Value = "Output File"
    startWs.Cells(outRow, 6).Value = "Sort Order"
    startWs.Range(startWs.Cells(outRow, 1), startWs.Cells(outRow, 6)).Font.Bold = True
    outRow = outRow + 1

    For r = 2 To last
        If CleanText(CellByHeader(chartsWs, r, map, "Sector")) = sectorName Then
            startWs.Cells(outRow, 1).Value = CellByHeader(chartsWs, r, map, "Include")
            startWs.Cells(outRow, 2).Value = CellByHeader(chartsWs, r, map, "Plot ID")
            startWs.Cells(outRow, 3).Value = CellByHeader(chartsWs, r, map, "Plot Function")
            startWs.Cells(outRow, 4).Value = CellByHeader(chartsWs, r, map, "Data Source ID")
            startWs.Cells(outRow, 5).Value = CellByHeader(chartsWs, r, map, "Output File")
            startWs.Cells(outRow, 6).Value = CellByHeader(chartsWs, r, map, "Sort Order")
            outRow = outRow + 1
        End If
    Next r

    WriteSectorPlotList = outRow + 1
End Function

Public Sub DeleteDataSourceWizard()
    Dim dataSourceId As String
    Dim chartCount As Long
    Dim deletedSources As Long
    Dim deletedArgs As Long
    Dim message As String

    dataSourceId = PromptSelectFromList("Data Source ID to delete", GetListValues(DATA_SOURCES_SHEET, "Data Source ID"))
    If Len(dataSourceId) = 0 Then Exit Sub

    chartCount = CountRowsByHeaderValue(ThisWorkbook.Worksheets(CHARTS_SHEET), "Data Source ID", dataSourceId)

    message = "Delete data source '" & dataSourceId & "' from the Data Sources sheet?"
    If chartCount > 0 Then
        message = message & vbCrLf & vbCrLf & chartCount & " chart row(s) still use this data source. Delete or update those charts too."
    End If

    If MsgBox(message, vbExclamation + vbYesNo) <> vbYes Then Exit Sub

    deletedSources = DeleteRowsByHeaderValue(ThisWorkbook.Worksheets(DATA_SOURCES_SHEET), "Data Source ID", dataSourceId)
    deletedArgs = DeleteRowsByHeaderValue(ThisWorkbook.Worksheets(DATA_ARGS_SHEET), "Data Source ID", dataSourceId)

    MsgBox "Deleted " & deletedSources & " data source row(s) and " & deletedArgs & " data argument row(s). Click Build R Config and Export Chart Config to update the release config.", vbInformation
End Sub

Public Sub RefreshDataFunctionsFromR(Optional ByVal showMessage As Boolean = True)
    Dim functions As Object
    Dim folderPath As String
    Dim ws As Worksheet
    Dim colNum As Long
    Dim fieldsColNum As Long
    Dim rowNum As Long
    Dim key As Variant
    Dim fields As Collection

    Set functions = CreateObject("Scripting.Dictionary")
    folderPath = ProjectRootPath() & "\R\data_functions"

    LoadFunctionNamesFromFolder folderPath, functions

    Set ws = EnsureSheet("Lists")
    colNum = EnsureHeaderColumn(ws, "Data Functions")
    fieldsColNum = EnsureHeaderColumn(ws, "Data Function Fields")
    ws.Range(ws.Cells(2, colNum), ws.Cells(ws.Rows.Count, colNum)).ClearContents
    ws.Range(ws.Cells(2, fieldsColNum), ws.Cells(ws.Rows.Count, fieldsColNum)).ClearContents

    rowNum = 2
    For Each key In functions.Keys
        ws.Cells(rowNum, colNum).Value = CStr(key)
        Set fields = GetDataFunctionFields(CStr(key))
        ws.Cells(rowNum, fieldsColNum).Value = JoinCollection(fields, ", ")
        rowNum = rowNum + 1
    Next key

    If showMessage Or functions.Count = 0 Then
        MsgBox "Loaded " & functions.Count & " data function(s) from " & folderPath, vbInformation
    End If
End Sub

Public Sub BuilderFillComboWithExcelSheets(ByVal combo As Object, ByVal sourceRef As String)
    Dim sheetNames As Collection
    Dim i As Long

    combo.Clear
    Set sheetNames = GetWorkbookSheetNames(sourceRef)

    For i = 1 To sheetNames.Count
        combo.AddItem CStr(sheetNames(i))
    Next i
End Sub

Public Sub BuilderFillComboWithDataFunctions(ByVal combo As Object)
    RefreshDataFunctionsFromR False
    BuilderFillComboFromColumn combo, "Lists", "Data Functions"
End Sub

Public Function BuilderSheetExists(ByVal sheetName As String) As Boolean
    Dim ws As Worksheet

    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets(sheetName)
    On Error GoTo 0

    BuilderSheetExists = Not ws Is Nothing
End Function

Public Function BuilderProjectRoot() As String
    BuilderProjectRoot = ProjectRootPath()
End Function

Public Function BuilderCleanText(ByVal value As Variant) As String
    BuilderCleanText = CleanText(value)
End Function

Public Function BuilderHeaderColumn(ByVal ws As Worksheet, ByVal headerName As String) As Long
    Dim map As Object
    Set map = HeaderMap(ws, 1)

    If map.Exists(headerName) Then
        BuilderHeaderColumn = CLng(map(headerName))
    Else
        BuilderHeaderColumn = 0
    End If
End Function

Public Function BuilderLastRow(ByVal ws As Worksheet, ByVal colNum As Long) As Long
    BuilderLastRow = LastRow(ws, colNum)
End Function

Public Sub BuilderFillComboFromColumn(ByVal combo As Object, ByVal sheetName As String, ByVal headerName As String, Optional ByVal includeBlank As Boolean = False)
    Dim ws As Worksheet
    Dim colNum As Long
    Dim r As Long
    Dim last As Long
    Dim value As String
    Dim seen As Object

    combo.Clear
    If includeBlank Then combo.AddItem ""
    If Not BuilderSheetExists(sheetName) Then Exit Sub

    Set ws = ThisWorkbook.Worksheets(sheetName)
    colNum = BuilderHeaderColumn(ws, headerName)
    If colNum = 0 Then Exit Sub

    Set seen = CreateObject("Scripting.Dictionary")
    last = LastRow(ws, colNum)

    For r = 2 To last
        value = CleanText(ws.Cells(r, colNum).Value)
        If Len(value) > 0 And Not seen.Exists(value) Then
            combo.AddItem value
            seen(value) = True
        End If
    Next r
End Sub

Public Sub BuilderFillComboFromArray(ByVal combo As Object, ByVal values As Variant, Optional ByVal includeBlank As Boolean = False)
    Dim i As Long

    combo.Clear
    If includeBlank Then combo.AddItem ""

    For i = LBound(values) To UBound(values)
        combo.AddItem CStr(values(i))
    Next i
End Sub

Public Sub RefreshPlotFunctionsFromR(Optional ByVal showMessage As Boolean = True)
    Dim functions As Object
    Dim folderPath As String
    Dim ws As Worksheet
    Dim colNum As Long
    Dim rowNum As Long
    Dim key As Variant

    Set functions = CreateObject("Scripting.Dictionary")
    folderPath = ProjectRootPath() & "\R\plot_functions"

    LoadFunctionNamesFromFolder folderPath, functions

    Set ws = EnsureSheet("Lists")
    colNum = EnsureHeaderColumn(ws, "Plot Functions")
    ws.Range(ws.Cells(2, colNum), ws.Cells(ws.Rows.Count, colNum)).ClearContents

    rowNum = 2
    For Each key In functions.Keys
        ws.Cells(rowNum, colNum).Value = CStr(key)
        rowNum = rowNum + 1
    Next key

    If showMessage Or functions.Count = 0 Then
        MsgBox "Loaded " & functions.Count & " plot function(s) from " & folderPath, vbInformation
    End If
End Sub

Private Sub LoadFunctionNamesFromFolder(ByVal folderPath As String, ByVal functions As Object)
    Dim fso As Object
    Dim folder As Object
    Dim file As Object

    Set fso = CreateObject("Scripting.FileSystemObject")
    If Not fso.FolderExists(folderPath) Then Exit Sub

    Set folder = fso.GetFolder(folderPath)
    For Each file In folder.Files
        If LCase$(fso.GetExtensionName(file.Name)) = "r" Then
            LoadFunctionNamesFromFile CStr(file.Path), functions
        End If
    Next file
End Sub

Private Sub LoadFunctionNamesFromFile(ByVal filePath As String, ByVal functions As Object)
    Dim fso As Object
    Dim stream As Object
    Dim lineText As String
    Dim functionName As String

    Set fso = CreateObject("Scripting.FileSystemObject")
    Set stream = fso.OpenTextFile(filePath, 1, False)

    Do Until stream.AtEndOfStream
        lineText = stream.ReadLine
        functionName = ExtractRFunctionName(lineText)
        If Len(functionName) > 0 Then
            If Not functions.Exists(functionName) Then functions.Add functionName, True
        End If
    Loop

    stream.Close
End Sub

Private Function ExtractRFunctionName(ByVal lineText As String) As String
    Dim text As String
    Dim assignPos As Long
    Dim namePart As String
    Dim rhs As String

    If lineText <> LTrim$(lineText) Then Exit Function

    text = Trim$(lineText)
    If Len(text) = 0 Or Left$(text, 1) = "#" Then Exit Function

    assignPos = InStr(1, text, "<-", vbTextCompare)
    If assignPos = 0 Then assignPos = InStr(1, text, "=", vbTextCompare)
    If assignPos = 0 Then Exit Function

    namePart = Trim$(Left$(text, assignPos - 1))
    rhs = Trim$(Mid$(text, assignPos + IIf(Mid$(text, assignPos, 2) = "<-", 2, 1)))

    If Not IsSimpleRName(namePart) Then Exit Function

    If Left$(LCase$(RemoveSpaces(rhs)), 9) = "function(" Then
        ExtractRFunctionName = namePart
    ElseIf IsPlotFunctionAlias(namePart) And IsSimpleRName(rhs) Then
        ExtractRFunctionName = namePart
    End If
End Function

Private Function RemoveSpaces(ByVal value As String) As String
    Dim text As String

    text = Replace(value, " ", "")
    text = Replace(text, vbTab, "")
    RemoveSpaces = text
End Function

Private Function IsSimpleRName(ByVal value As String) As Boolean
    Dim i As Long
    Dim ch As String

    value = Trim$(value)
    If Len(value) = 0 Then Exit Function

    ch = Mid$(value, 1, 1)
    If Not (ch Like "[A-Za-z.]") Then Exit Function

    For i = 2 To Len(value)
        ch = Mid$(value, i, 1)
        If Not (ch Like "[A-Za-z0-9._]") Then Exit Function
    Next i

    IsSimpleRName = True
End Function

Private Function IsPlotFunctionAlias(ByVal value As String) As Boolean
    Dim lowerValue As String

    lowerValue = LCase$(Trim$(value))
    IsPlotFunctionAlias = (Left$(lowerValue, 4) = "plot" Or Right$(lowerValue, 5) = "_plot")
End Function

Private Function GetListValues(ByVal sheetName As String, ByVal headerName As String) As Collection
    Dim values As Collection
    Dim ws As Worksheet
    Dim colNum As Long
    Dim rowNum As Long
    Dim last As Long
    Dim value As String

    Set values = New Collection
    If Not BuilderSheetExists(sheetName) Then
        Set GetListValues = values
        Exit Function
    End If

    Set ws = ThisWorkbook.Worksheets(sheetName)
    colNum = BuilderHeaderColumn(ws, headerName)
    If colNum = 0 Then
        Set GetListValues = values
        Exit Function
    End If

    last = LastRow(ws, colNum)
    For rowNum = 2 To last
        value = CleanText(ws.Cells(rowNum, colNum).Value)
        If Len(value) > 0 Then values.Add value
    Next rowNum

    Set GetListValues = values
End Function

Private Function CollectionFromArray(ByVal values As Variant) As Collection
    Dim result As Collection
    Dim i As Long

    Set result = New Collection
    For i = LBound(values) To UBound(values)
        result.Add CStr(values(i))
    Next i

    Set CollectionFromArray = result
End Function

Private Function SopiSectorValues() As Collection
    Set SopiSectorValues = CollectionFromArray(Array( _
        "Macro", _
        "Dairy", _
        "Meat and Wool", _
        "Forestry", _
        "Horticulture", _
        "Seafood", _
        "Arable", _
        "Other foods"))
End Function

Private Function GetWorkbookSheetNames(ByVal sourceRef As String) As Collection
    Dim names As Collection
    Dim wb As Workbook
    Dim fullPath As String
    Dim i As Long
    Dim oldScreenUpdating As Boolean

    Set names = New Collection
    fullPath = BuilderResolvePath(sourceRef)

    If Len(fullPath) = 0 Or Len(Dir(fullPath)) = 0 Then
        Set GetWorkbookSheetNames = names
        Exit Function
    End If

    On Error GoTo CleanFail
    oldScreenUpdating = Application.ScreenUpdating
    Application.ScreenUpdating = False

    Set wb = Workbooks.Open(Filename:=fullPath, UpdateLinks:=False, ReadOnly:=True, AddToMru:=False)
    For i = 1 To wb.Worksheets.Count
        names.Add wb.Worksheets(i).Name
    Next i

CleanExit:
    If Not wb Is Nothing Then wb.Close SaveChanges:=False
    Application.ScreenUpdating = oldScreenUpdating
    Set GetWorkbookSheetNames = names
    Exit Function

CleanFail:
    Resume CleanExit
End Function

Private Function PromptSelectFromList(ByVal title As String, ByVal values As Collection) As String
    Dim prompt As String
    Dim i As Long
    Dim answer As String
    Dim index As Long

    If values.Count = 0 Then
        PromptSelectFromList = PromptValue(title, "No values found. Type a value.", "")
        Exit Function
    End If

    prompt = "Select " & title & " by number:" & vbCrLf & vbCrLf
    For i = 1 To values.Count
        prompt = prompt & i & ". " & CStr(values(i)) & vbCrLf
    Next i

    answer = PromptValue(title, prompt, "1")
    If Len(answer) = 0 Then
        PromptSelectFromList = ""
    ElseIf IsNumeric(answer) Then
        index = CLng(answer)
        If index >= 1 And index <= values.Count Then
            PromptSelectFromList = CStr(values(index))
        Else
            PromptSelectFromList = ""
        End If
    Else
        PromptSelectFromList = answer
    End If
End Function

Private Function PromptSelectField(ByVal title As String, ByVal fields As Collection, ByVal defaultValue As String, ByVal allowBlank As Boolean) As String
    Dim prompt As String
    Dim i As Long
    Dim answer As String
    Dim index As Long

    If fields.Count = 0 Then
        PromptSelectField = PromptValue(title, "Enter " & title & ".", defaultValue)
        Exit Function
    End If

    prompt = "Select " & title & " by number, or type a column name:" & vbCrLf & vbCrLf
    If allowBlank Then prompt = prompt & "0. Leave blank" & vbCrLf

    For i = 1 To fields.Count
        prompt = prompt & i & ". " & CStr(fields(i)) & vbCrLf
    Next i

    answer = PromptValue(title, prompt, CStr(DefaultFieldIndex(fields, defaultValue, allowBlank)))
    If Len(answer) = 0 Then
        PromptSelectField = ""
    ElseIf IsNumeric(answer) Then
        index = CLng(answer)
        If allowBlank And index = 0 Then
            PromptSelectField = ""
        ElseIf index >= 1 And index <= fields.Count Then
            PromptSelectField = CStr(fields(index))
        Else
            PromptSelectField = ""
        End If
    Else
        PromptSelectField = answer
    End If
End Function

Private Function DefaultFieldIndex(ByVal fields As Collection, ByVal defaultValue As String, ByVal allowBlank As Boolean) As Long
    Dim i As Long

    If Len(defaultValue) = 0 And allowBlank Then
        DefaultFieldIndex = 0
        Exit Function
    End If

    For i = 1 To fields.Count
        If LCase$(CStr(fields(i))) = LCase$(defaultValue) Then
            DefaultFieldIndex = i
            Exit Function
        End If
    Next i

    If allowBlank Then
        DefaultFieldIndex = 0
    Else
        DefaultFieldIndex = 1
    End If
End Function

Private Function FirstExistingField(ByVal fields As Collection, ByVal candidates As Variant, ByVal fallback As String) As String
    Dim candidateIndex As Long
    Dim fieldIndex As Long

    For candidateIndex = LBound(candidates) To UBound(candidates)
        For fieldIndex = 1 To fields.Count
            If LCase$(CStr(fields(fieldIndex))) = LCase$(CStr(candidates(candidateIndex))) Then
                FirstExistingField = CStr(fields(fieldIndex))
                Exit Function
            End If
        Next fieldIndex
    Next candidateIndex

    FirstExistingField = fallback
End Function

Private Sub AddUniqueText(ByVal values As Collection, ByVal value As String)
    Dim i As Long
    Dim cleanValue As String

    cleanValue = CleanText(value)
    If Len(cleanValue) = 0 Then Exit Sub

    For i = 1 To values.Count
        If LCase$(CStr(values(i))) = LCase$(cleanValue) Then Exit Sub
    Next i

    values.Add cleanValue
End Sub

Private Function IsRoxygenLine(ByVal lineText As String) As Boolean
    IsRoxygenLine = (Left$(Trim$(lineText), 2) = "#'")
End Function

Private Function CleanRoxygenLine(ByVal lineText As String) As String
    Dim text As String

    text = Trim$(lineText)
    If Left$(text, 2) = "#'" Then text = Mid$(text, 3)
    CleanRoxygenLine = Trim$(text)
End Function

Private Sub AppendFieldsFromRoxygen(ByVal docs As Collection, ByVal fields As Collection)
    Dim i As Long
    Dim lineText As String
    Dim lowerText As String
    Dim payload As String
    Dim pos As Long

    For i = 1 To docs.Count
        lineText = CStr(docs(i))
        lowerText = LCase$(lineText)

        If Left$(lowerText, 12) = "@sopi_fields" Then
            payload = Trim$(Mid$(lineText, 13))
            AddDelimitedFields fields, payload
        ElseIf Left$(lowerText, 11) = "@sopi_field" Then
            payload = Trim$(Mid$(lineText, 12))
            AddDelimitedFields fields, payload
        ElseIf Left$(lowerText, 7) = "@return" Then
            pos = InStr(1, lowerText, "columns:", vbTextCompare)
            If pos > 0 Then
                payload = Mid$(lineText, pos + Len("columns:"))
                AddDelimitedFields fields, payload
            End If
        End If
    Next i
End Sub

Private Sub AddDelimitedFields(ByVal fields As Collection, ByVal payload As String)
    Dim cleanPayload As String
    Dim parts As Variant
    Dim i As Long

    cleanPayload = Replace(payload, ";", ",")
    parts = Split(cleanPayload, ",")

    For i = LBound(parts) To UBound(parts)
        AddUniqueText fields, CleanFieldToken(CStr(parts(i)))
    Next i
End Sub

Private Function CleanFieldToken(ByVal value As String) As String
    Dim text As String

    text = Trim$(value)
    text = Replace(text, "`", "")

    Do While Len(text) > 0 And InStr(1, " .:;)", Right$(text, 1), vbBinaryCompare) > 0
        text = Left$(text, Len(text) - 1)
    Loop

    Do While Len(text) > 0 And InStr(1, " (", Left$(text, 1), vbBinaryCompare) > 0
        text = Mid$(text, 2)
    Loop

    CleanFieldToken = Trim$(text)
End Function

Private Function JoinCollection(ByVal values As Collection, ByVal delimiter As String) As String
    Dim i As Long
    Dim result As String

    For i = 1 To values.Count
        If Len(result) > 0 Then result = result & delimiter
        result = result & CStr(values(i))
    Next i

    JoinCollection = result
End Function

Public Function BuilderNextSortOrder(ByVal sectorName As String) As Long
    Dim ws As Worksheet
    Dim map As Object
    Dim r As Long
    Dim last As Long
    Dim maxSort As Long

    Set ws = ThisWorkbook.Worksheets(CHARTS_SHEET)
    Set map = HeaderMap(ws, 1)
    last = LastRow(ws, 1)

    For r = 2 To last
        If CleanText(CellByHeader(ws, r, map, "Sector")) = sectorName Then
            If IsNumeric(CellByHeader(ws, r, map, "Sort Order")) Then
                If CLng(CellByHeader(ws, r, map, "Sort Order")) > maxSort Then
                    maxSort = CLng(CellByHeader(ws, r, map, "Sort Order"))
                End If
            End If
        End If
    Next r

    BuilderNextSortOrder = maxSort + 1
End Function

Public Function BuilderGetDataSourceInfo( _
    ByVal dataSourceId As String, _
    ByRef sourceType As String, _
    ByRef sourceRef As String, _
    ByRef sourceSheet As String, _
    ByRef sourceRange As String, _
    ByRef dataFunction As String) As Boolean
    Dim ws As Worksheet
    Dim map As Object
    Dim r As Long
    Dim last As Long

    Set ws = ThisWorkbook.Worksheets(DATA_SOURCES_SHEET)
    Set map = HeaderMap(ws, 1)
    last = LastRow(ws, 1)

    For r = 2 To last
        If CleanText(CellByHeader(ws, r, map, "Data Source ID")) = dataSourceId Then
            sourceType = CleanText(CellByHeader(ws, r, map, "Source Type"))
            sourceRef = CleanText(CellByHeader(ws, r, map, "Source Ref"))
            sourceSheet = CleanText(CellByHeader(ws, r, map, "Sheet"))
            sourceRange = CleanText(CellByHeader(ws, r, map, "Range"))
            dataFunction = CleanText(CellByHeader(ws, r, map, "Data Function"))
            BuilderGetDataSourceInfo = True
            Exit Function
        End If
    Next r

    BuilderGetDataSourceInfo = False
End Function

Public Function BuilderResolvePath(ByVal pathValue As String) As String
    If Len(CleanText(pathValue)) = 0 Then
        BuilderResolvePath = ""
    ElseIf Mid$(pathValue, 2, 2) = ":\" Or Left$(pathValue, 2) = "\\" Then
        BuilderResolvePath = pathValue
    Else
        BuilderResolvePath = ProjectRootPath() & "\" & pathValue
    End If
End Function

Private Function GetDataSourceFieldNames(ByVal dataSourceId As String) As Collection
    Dim sourceType As String
    Dim sourceRef As String
    Dim sourceSheet As String
    Dim sourceRange As String
    Dim dataFunction As String
    Dim fields As Collection

    Set fields = New Collection
    If Not BuilderGetDataSourceInfo(dataSourceId, sourceType, sourceRef, sourceSheet, sourceRange, dataFunction) Then
        Set GetDataSourceFieldNames = fields
        Exit Function
    End If

    If LCase$(sourceType) = "excel" Then
        Set fields = GetExcelHeaderFields(sourceRef, sourceSheet, sourceRange)
    ElseIf LCase$(sourceType) = "function" Then
        Set fields = GetDataFunctionFields(dataFunction)
    End If

    Set GetDataSourceFieldNames = fields
End Function

Private Function GetExcelHeaderFields(ByVal sourceRef As String, ByVal sourceSheet As String, ByVal sourceRange As String) As Collection
    Dim fields As Collection
    Dim wb As Workbook
    Dim ws As Worksheet
    Dim headerRange As Range
    Dim c As Range
    Dim fullPath As String
    Dim oldScreenUpdating As Boolean

    Set fields = New Collection
    fullPath = BuilderResolvePath(sourceRef)

    If Len(fullPath) = 0 Or Len(Dir(fullPath)) = 0 Or Len(sourceSheet) = 0 Then
        Set GetExcelHeaderFields = fields
        Exit Function
    End If

    On Error GoTo CleanFail
    oldScreenUpdating = Application.ScreenUpdating
    Application.ScreenUpdating = False

    Set wb = Workbooks.Open(Filename:=fullPath, UpdateLinks:=False, ReadOnly:=True, AddToMru:=False)
    Set ws = wb.Worksheets(sourceSheet)

    If Len(sourceRange) > 0 Then
        Set headerRange = ws.Range(sourceRange).Rows(1)
    Else
        Set headerRange = ws.UsedRange.Rows(1)
    End If

    For Each c In headerRange.Cells
        If Len(CleanText(c.Value)) > 0 Then AddUniqueText fields, CleanText(c.Value)
    Next c

CleanExit:
    If Not wb Is Nothing Then wb.Close SaveChanges:=False
    Application.ScreenUpdating = oldScreenUpdating
    Set GetExcelHeaderFields = fields
    Exit Function

CleanFail:
    Resume CleanExit
End Function

Private Function GetDataFunctionFields(ByVal dataFunction As String) As Collection
    Dim fields As Collection
    Dim filePath As String

    Set fields = New Collection
    filePath = FindRFunctionFile(ProjectRootPath() & "\R\data_functions", dataFunction)

    If Len(filePath) > 0 Then
        Set fields = LoadRoxygenFieldsFromFile(filePath, dataFunction)
    End If

    Set GetDataFunctionFields = fields
End Function

Private Function FindRFunctionFile(ByVal folderPath As String, ByVal functionName As String) As String
    Dim fso As Object
    Dim folder As Object
    Dim file As Object
    Dim functions As Object

    Set fso = CreateObject("Scripting.FileSystemObject")
    If Not fso.FolderExists(folderPath) Then Exit Function

    Set folder = fso.GetFolder(folderPath)
    For Each file In folder.Files
        If LCase$(fso.GetExtensionName(file.Name)) = "r" Then
            Set functions = CreateObject("Scripting.Dictionary")
            LoadFunctionNamesFromFile CStr(file.Path), functions
            If functions.Exists(functionName) Then
                FindRFunctionFile = CStr(file.Path)
                Exit Function
            End If
        End If
    Next file
End Function

Private Function LoadRoxygenFieldsFromFile(ByVal filePath As String, ByVal functionName As String) As Collection
    Dim fields As Collection
    Dim fso As Object
    Dim stream As Object
    Dim docs As Collection
    Dim lineText As String
    Dim functionFound As Boolean

    Set fields = New Collection
    Set docs = New Collection
    Set fso = CreateObject("Scripting.FileSystemObject")
    Set stream = fso.OpenTextFile(filePath, 1, False)

    Do Until stream.AtEndOfStream
        lineText = stream.ReadLine

        If IsRoxygenLine(lineText) Then
            docs.Add CleanRoxygenLine(lineText)
        ElseIf ExtractRFunctionName(lineText) = functionName Then
            AppendFieldsFromRoxygen docs, fields
            functionFound = True
            Exit Do
        ElseIf Len(Trim$(lineText)) > 0 Then
            Set docs = New Collection
        End If
    Loop

    stream.Close
    Set LoadRoxygenFieldsFromFile = fields
End Function

Public Sub BuilderFillCombosWithExcelHeaders(ByVal dataSourceId As String, ParamArray combos() As Variant)
    Dim sourceType As String
    Dim sourceRef As String
    Dim sourceSheet As String
    Dim sourceRange As String
    Dim dataFunction As String
    Dim wb As Workbook
    Dim ws As Worksheet
    Dim headerRange As Range
    Dim c As Range
    Dim i As Long
    Dim fullPath As String

    If Not BuilderGetDataSourceInfo(dataSourceId, sourceType, sourceRef, sourceSheet, sourceRange, dataFunction) Then
        MsgBox "Data source not found: " & dataSourceId, vbExclamation
        Exit Sub
    End If

    If LCase$(sourceType) <> "excel" Then
        MsgBox "Field selection is available for Excel data sources only.", vbInformation
        Exit Sub
    End If

    fullPath = BuilderResolvePath(sourceRef)
    If Len(Dir(fullPath)) = 0 Then
        MsgBox "Could not find source workbook: " & fullPath, vbExclamation
        Exit Sub
    End If

    Application.ScreenUpdating = False
    Set wb = Workbooks.Open(Filename:=fullPath, UpdateLinks:=False, ReadOnly:=True, AddToMru:=False)
    Set ws = wb.Worksheets(sourceSheet)

    If Len(sourceRange) > 0 Then
        Set headerRange = ws.Range(sourceRange).Rows(1)
    Else
        Set headerRange = ws.UsedRange.Rows(1)
    End If

    For i = LBound(combos) To UBound(combos)
        combos(i).Clear
        combos(i).AddItem ""
    Next i

    For Each c In headerRange.Cells
        If Len(CleanText(c.Value)) > 0 Then
            For i = LBound(combos) To UBound(combos)
                combos(i).AddItem CleanText(c.Value)
            Next i
        End If
    Next c

    wb.Close SaveChanges:=False
    Application.ScreenUpdating = True
End Sub

Public Sub BuilderSaveChartForm(ByVal form As Object)
    Dim values(1 To 25) As Variant

    values(1) = CBool(form.chkInclude.Value)
    values(2) = Trim$(form.txtPlotId.Value)
    values(3) = form.cboSector.Value
    values(4) = form.cboPlotFunction.Value
    values(5) = form.cboDataSourceId.Value
    values(6) = Trim$(form.txtOutputFile.Value)
    values(7) = Trim$(form.txtTitle.Value)
    values(8) = Trim$(form.txtSubtitle.Value)
    values(9) = form.txtSortOrder.Value
    values(10) = form.cboXField.Value
    values(11) = form.cboXFreq.Value
    values(12) = form.cboGroupField.Value
    values(13) = form.cboColumnValue.Value
    values(14) = Trim$(form.txtColumnAxisLabel.Value)
    values(15) = Trim$(form.txtColumnLegendLabel.Value)
    values(16) = form.cboLineValue.Value
    values(17) = Trim$(form.txtLineAxisLabel.Value)
    values(18) = Trim$(form.txtLineLegendLabel.Value)
    values(19) = form.cboColumnPosition.Value
    values(20) = CBool(form.chkForecast.Value)
    values(21) = CBool(form.chkUseMetadataPalette.Value)
    values(22) = form.txtPrimaryMinBreaks.Value
    values(23) = form.txtPrimaryMaxBreaks.Value
    values(24) = form.txtSecondaryMinBreaks.Value
    values(25) = form.txtSecondaryMaxBreaks.Value

    WriteRowValues ThisWorkbook.Worksheets(CHARTS_SHEET), values
End Sub

Public Sub BuilderSaveDataSourceForm(ByVal form As Object)
    Dim values(1 To 8) As Variant

    values(1) = Trim$(form.txtDataSourceId.Value)
    values(2) = form.cboSourceType.Value
    values(3) = Trim$(form.txtSourceRef.Value)
    values(4) = Trim$(form.txtSheet.Value)
    values(5) = Trim$(form.txtRange.Value)
    values(6) = Trim$(form.txtDataFunction.Value)
    values(7) = CBool(form.chkCache.Value)
    values(8) = Trim$(form.txtNotes.Value)

    WriteRowValues ThisWorkbook.Worksheets(DATA_SOURCES_SHEET), values
End Sub

Private Sub WriteRowValues(ByVal ws As Worksheet, ByVal values As Variant)
    Dim outRow As Long
    Dim i As Long

    outRow = LastRow(ws, 1) + 1
    For i = LBound(values) To UBound(values)
        ws.Cells(outRow, i).Value = values(i)
    Next i
End Sub

Private Function WriteOrReplaceRowValues(ByVal ws As Worksheet, ByVal keyHeader As String, ByVal keyValue As String, ByVal values As Variant) As Boolean
    Dim existingRow As Long

    existingRow = FindRowByHeaderValue(ws, keyHeader, keyValue)
    If existingRow > 0 Then
        If MsgBox(keyHeader & " '" & keyValue & "' already exists. Replace the existing row?", vbQuestion + vbYesNo) <> vbYes Then Exit Function
        WriteValuesToRow ws, existingRow, values
    Else
        WriteRowValues ws, values
    End If

    WriteOrReplaceRowValues = True
End Function

Private Sub WriteValuesToRow(ByVal ws As Worksheet, ByVal rowNum As Long, ByVal values As Variant)
    Dim i As Long

    For i = LBound(values) To UBound(values)
        ws.Cells(rowNum, i).Value = values(i)
    Next i
End Sub

Private Function FindRowByHeaderValue(ByVal ws As Worksheet, ByVal headerName As String, ByVal keyValue As String) As Long
    Dim colNum As Long
    Dim r As Long
    Dim last As Long

    colNum = BuilderHeaderColumn(ws, headerName)
    If colNum = 0 Then Exit Function

    last = LastRow(ws, colNum)
    For r = 2 To last
        If CleanText(ws.Cells(r, colNum).Value) = keyValue Then
            FindRowByHeaderValue = r
            Exit Function
        End If
    Next r
End Function

Private Function CountRowsByHeaderValue(ByVal ws As Worksheet, ByVal headerName As String, ByVal keyValue As String) As Long
    Dim colNum As Long
    Dim r As Long
    Dim last As Long
    Dim count As Long

    colNum = BuilderHeaderColumn(ws, headerName)
    If colNum = 0 Then Exit Function

    last = LastRow(ws, colNum)
    For r = 2 To last
        If CleanText(ws.Cells(r, colNum).Value) = keyValue Then count = count + 1
    Next r

    CountRowsByHeaderValue = count
End Function

Private Function DeleteRowsByHeaderValue(ByVal ws As Worksheet, ByVal headerName As String, ByVal keyValue As String) As Long
    Dim colNum As Long
    Dim r As Long
    Dim last As Long
    Dim deletedCount As Long

    colNum = BuilderHeaderColumn(ws, headerName)
    If colNum = 0 Then Exit Function

    last = LastRow(ws, colNum)
    For r = last To 2 Step -1
        If CleanText(ws.Cells(r, colNum).Value) = keyValue Then
            ws.Rows(r).Delete
            deletedCount = deletedCount + 1
        End If
    Next r

    DeleteRowsByHeaderValue = deletedCount
End Function

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
                AddPlotArg argsSheet, argRow, usedArgs, plotId, argName, _
                    CellByHeader(src, r, map, "Arg Value"), _
                    CellByHeader(src, r, map, "Arg Type"), _
                    CellByHeader(src, r, map, "Notes")
            End If
        End If
    Next r
End Sub

Private Sub AddPlotArg( _
    ByVal ws As Worksheet, _
    ByRef outRow As Long, _
    ByVal usedArgs As Object, _
    ByVal plotId As String, _
    ByVal argName As String, _
    ByVal argValue As Variant, _
    ByVal argType As String, _
    ByVal notes As String)
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

Private Function EnsureHeaderColumn(ByVal ws As Worksheet, ByVal headerName As String) As Long
    Dim colNum As Long

    colNum = BuilderHeaderColumn(ws, headerName)
    If colNum > 0 Then
        EnsureHeaderColumn = colNum
        Exit Function
    End If

    colNum = ws.Cells(1, ws.Columns.Count).End(xlToLeft).Column
    If Len(CleanText(ws.Cells(1, colNum).Value)) > 0 Then colNum = colNum + 1

    ws.Cells(1, colNum).Value = headerName
    ws.Cells(1, colNum).Font.Bold = True
    EnsureHeaderColumn = colNum
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

Private Function PromptValue(ByVal title As String, ByVal prompt As String, ByVal defaultValue As String) As String
    PromptValue = Trim$(InputBox(prompt, title, defaultValue))
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
    ConfigRootPath = ProjectRootPath() & "\config"
End Function

Private Function ProjectRootPath() As String
    Dim p As String
    Dim marker As String
    Dim pos As Long

    p = ThisWorkbook.Path
    marker = "\config\"
    pos = InStr(1, LCase$(p), marker, vbTextCompare)

    If pos > 0 Then
        ProjectRootPath = Left$(p, pos - 1)
    Else
        ProjectRootPath = p
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

Private Sub AddButton( _
    ByVal ws As Worksheet, _
    ByVal caption As String, _
    ByVal macroName As String, _
    ByVal leftPos As Double, _
    ByVal topPos As Double, _
    ByVal widthVal As Double, _
    ByVal heightVal As Double)
    Dim btn As Button
    Set btn = ws.Buttons.Add(leftPos, topPos, widthVal, heightVal)
    btn.Caption = caption
    btn.OnAction = macroName
End Sub
