VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmChartWizard
   Caption         =   "Add SOPI Chart"
   ClientHeight    =   8280
   ClientLeft      =   120
   ClientTop       =   465
   ClientWidth     =   8460
   StartUpPosition =   1  'CenterOwner
   Begin MSForms.Frame fraChart
      Caption         =   "Chart setup"
      Height          =   2580
      Left            =   180
      TabIndex        =   0
      Top             =   120
      Width           =   8100
      Begin MSForms.CheckBox chkInclude
         Caption         =   "Include chart"
         Height          =   240
         Left            =   180
         TabIndex        =   1
         Top             =   300
         Value           =   1  'Checked
         Width           =   1440
      End
      Begin MSForms.TextBox txtPlotId
         Height          =   240
         Left            =   1500
         TabIndex        =   2
         Top             =   660
         Width           =   2280
      End
      Begin MSForms.ComboBox cboSector
         Height          =   315
         Left            =   1500
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   1020
         Width           =   2280
      End
      Begin MSForms.ComboBox cboPlotFunction
         Height          =   315
         Left            =   1500
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   1440
         Width           =   2280
      End
      Begin MSForms.ComboBox cboDataSourceId
         Height          =   315
         Left            =   1500
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Top             =   1860
         Width           =   2280
      End
      Begin MSForms.CommandButton cmdLoadFields
         Caption         =   "Load Fields"
         Height          =   300
         Left            =   3900
         TabIndex        =   6
         Top             =   1860
         Width           =   1020
      End
      Begin MSForms.CommandButton cmdAddDataSource
         Caption         =   "Add Data Source"
         Height          =   300
         Left            =   5040
         TabIndex        =   7
         Top             =   1860
         Width           =   1380
      End
      Begin MSForms.TextBox txtOutputFile
         Height          =   240
         Left            =   5580
         TabIndex        =   8
         Top             =   660
         Width           =   2220
      End
      Begin MSForms.TextBox txtTitle
         Height          =   240
         Left            =   5580
         TabIndex        =   9
         Top             =   1020
         Width           =   2220
      End
      Begin MSForms.TextBox txtSubtitle
         Height          =   240
         Left            =   5580
         TabIndex        =   10
         Top             =   1440
         Width           =   2220
      End
      Begin MSForms.TextBox txtSortOrder
         Height          =   240
         Left            =   5580
         TabIndex        =   11
         Top             =   2220
         Width           =   840
      End
      Begin MSForms.Label lblPlotId
         Caption         =   "Plot ID"
         Height          =   240
         Left            =   180
         TabIndex        =   12
         Top             =   690
         Width           =   1140
      End
      Begin MSForms.Label lblSector
         Caption         =   "Sector"
         Height          =   240
         Left            =   180
         TabIndex        =   13
         Top             =   1080
         Width           =   1140
      End
      Begin MSForms.Label lblPlotFunction
         Caption         =   "Plot Function"
         Height          =   240
         Left            =   180
         TabIndex        =   14
         Top             =   1500
         Width           =   1140
      End
      Begin MSForms.Label lblDataSource
         Caption         =   "Data Source"
         Height          =   240
         Left            =   180
         TabIndex        =   15
         Top             =   1920
         Width           =   1140
      End
      Begin MSForms.Label lblOutputFile
         Caption         =   "Output File"
         Height          =   240
         Left            =   4260
         TabIndex        =   16
         Top             =   690
         Width           =   1140
      End
      Begin MSForms.Label lblTitle
         Caption         =   "Title"
         Height          =   240
         Left            =   4260
         TabIndex        =   17
         Top             =   1050
         Width           =   1140
      End
      Begin MSForms.Label lblSubtitle
         Caption         =   "Subtitle"
         Height          =   240
         Left            =   4260
         TabIndex        =   18
         Top             =   1470
         Width           =   1140
      End
      Begin MSForms.Label lblSortOrder
         Caption         =   "Sort Order"
         Height          =   240
         Left            =   4260
         TabIndex        =   19
         Top             =   2250
         Width           =   1140
      End
   End
   Begin MSForms.Frame fraFields
      Caption         =   "Fields and labels"
      Height          =   4080
      Left            =   180
      TabIndex        =   20
      Top             =   2820
      Width           =   8100
      Begin MSForms.ComboBox cboXField
         Height          =   315
         Left            =   1500
         TabIndex        =   21
         Top             =   360
         Width           =   2280
      End
      Begin MSForms.ComboBox cboXFreq
         Height          =   315
         Left            =   5580
         Style           =   2  'Dropdown List
         TabIndex        =   22
         Top             =   360
         Width           =   1440
      End
      Begin MSForms.ComboBox cboGroupField
         Height          =   315
         Left            =   1500
         TabIndex        =   23
         Top             =   840
         Width           =   2280
      End
      Begin MSForms.ComboBox cboColumnValue
         Height          =   315
         Left            =   1500
         TabIndex        =   24
         Top             =   1320
         Width           =   2280
      End
      Begin MSForms.TextBox txtColumnAxisLabel
         Height          =   240
         Left            =   5580
         TabIndex        =   25
         Top             =   1320
         Width           =   2220
      End
      Begin MSForms.TextBox txtColumnLegendLabel
         Height          =   240
         Left            =   5580
         TabIndex        =   26
         Top             =   1680
         Width           =   2220
      End
      Begin MSForms.ComboBox cboLineValue
         Height          =   315
         Left            =   1500
         TabIndex        =   27
         Top             =   2160
         Width           =   2280
      End
      Begin MSForms.TextBox txtLineAxisLabel
         Height          =   240
         Left            =   5580
         TabIndex        =   28
         Top             =   2160
         Width           =   2220
      End
      Begin MSForms.TextBox txtLineLegendLabel
         Height          =   240
         Left            =   5580
         TabIndex        =   29
         Top             =   2520
         Width           =   2220
      End
      Begin MSForms.ComboBox cboColumnPosition
         Height          =   315
         Left            =   1500
         Style           =   2  'Dropdown List
         TabIndex        =   30
         Top             =   3000
         Width           =   1440
      End
      Begin MSForms.CheckBox chkForecast
         Caption         =   "Forecast shading"
         Height          =   240
         Left            =   3300
         TabIndex        =   31
         Top             =   3060
         Value           =   1  'Checked
         Width           =   1560
      End
      Begin MSForms.CheckBox chkUseMetadataPalette
         Caption         =   "Use metadata palette"
         Height          =   240
         Left            =   5040
         TabIndex        =   32
         Top             =   3060
         Value           =   1  'Checked
         Width           =   1860
      End
      Begin MSForms.TextBox txtPrimaryMinBreaks
         Height          =   240
         Left            =   1500
         TabIndex        =   33
         Top             =   3540
         Width           =   480
      End
      Begin MSForms.TextBox txtPrimaryMaxBreaks
         Height          =   240
         Left            =   2580
         TabIndex        =   34
         Top             =   3540
         Width           =   480
      End
      Begin MSForms.TextBox txtSecondaryMinBreaks
         Height          =   240
         Left            =   4620
         TabIndex        =   35
         Top             =   3540
         Width           =   480
      End
      Begin MSForms.TextBox txtSecondaryMaxBreaks
         Height          =   240
         Left            =   5760
         TabIndex        =   36
         Top             =   3540
         Width           =   480
      End
      Begin MSForms.Label lblXField
         Caption         =   "X Field"
         Height          =   240
         Left            =   180
         TabIndex        =   37
         Top             =   420
         Width           =   1140
      End
      Begin MSForms.Label lblXFreq
         Caption         =   "X Frequency"
         Height          =   240
         Left            =   4260
         TabIndex        =   38
         Top             =   420
         Width           =   1140
      End
      Begin MSForms.Label lblGroupField
         Caption         =   "Group Field"
         Height          =   240
         Left            =   180
         TabIndex        =   39
         Top             =   900
         Width           =   1140
      End
      Begin MSForms.Label lblColumnValue
         Caption         =   "Column Value"
         Height          =   240
         Left            =   180
         TabIndex        =   40
         Top             =   1380
         Width           =   1140
      End
      Begin MSForms.Label lblColumnAxis
         Caption         =   "Column Axis Label"
         Height          =   240
         Left            =   4260
         TabIndex        =   41
         Top             =   1350
         Width           =   1320
      End
      Begin MSForms.Label lblColumnLegend
         Caption         =   "Column Legend"
         Height          =   240
         Left            =   4260
         TabIndex        =   42
         Top             =   1710
         Width           =   1320
      End
      Begin MSForms.Label lblLineValue
         Caption         =   "Line Value"
         Height          =   240
         Left            =   180
         TabIndex        =   43
         Top             =   2220
         Width           =   1140
      End
      Begin MSForms.Label lblLineAxis
         Caption         =   "Line Axis Label"
         Height          =   240
         Left            =   4260
         TabIndex        =   44
         Top             =   2190
         Width           =   1320
      End
      Begin MSForms.Label lblLineLegend
         Caption         =   "Line Legend"
         Height          =   240
         Left            =   4260
         TabIndex        =   45
         Top             =   2550
         Width           =   1320
      End
      Begin MSForms.Label lblColumnPosition
         Caption         =   "Column Position"
         Height          =   240
         Left            =   180
         TabIndex        =   46
         Top             =   3060
         Width           =   1260
      End
      Begin MSForms.Label lblPrimaryBreaks
         Caption         =   "Primary breaks min / max"
         Height          =   240
         Left            =   180
         TabIndex        =   47
         Top             =   3570
         Width           =   1320
      End
      Begin MSForms.Label lblSecondaryBreaks
         Caption         =   "Secondary breaks min / max"
         Height          =   240
         Left            =   3180
         TabIndex        =   48
         Top             =   3570
         Width           =   1440
      End
   End
   Begin MSForms.CommandButton cmdSave
      Caption         =   "Save Chart"
      Default         =   -1  'True
      Height          =   360
      Left            =   5520
      TabIndex        =   49
      Top             =   7200
      Width           =   1260
   End
   Begin MSForms.CommandButton cmdCancel
      Caption         =   "Cancel"
      Height          =   360
      Left            =   6960
      TabIndex        =   50
      Top             =   7200
      Width           =   1140
   End
End
Attribute VB_Name = "frmChartWizard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub UserForm_Initialize()
    BuilderFillComboFromColumn cboSector, "Sector Defaults", "Sector"
    BuilderFillComboFromArray cboPlotFunction, Array("generic_ts_plot", "plot_sopi", "plot_bar_ranked")
    BuilderFillComboFromColumn cboDataSourceId, "Data Sources", "Data Source ID"
    BuilderFillComboFromArray cboXFreq, Array("auto", "yearly", "quarterly", "monthly")
    BuilderFillComboFromArray cboColumnPosition, Array("dodge", "stacked")

    cboPlotFunction.Value = "generic_ts_plot"
    cboXFreq.Value = "auto"
    cboColumnPosition.Value = "dodge"
    txtColumnAxisLabel.Value = "Export revenue (NZ$ million)"
    txtColumnLegendLabel.Value = "Revenue"
    txtLineAxisLabel.Value = "Export volume (tonnes)"
    txtLineLegendLabel.Value = "Volume"
    txtPrimaryMinBreaks.Value = "4"
    txtPrimaryMaxBreaks.Value = "6"
    txtSecondaryMinBreaks.Value = "4"
    txtSecondaryMaxBreaks.Value = "6"
End Sub

Private Sub cboSector_Change()
    If Len(Trim$(cboSector.Value)) > 0 Then
        txtSortOrder.Value = CStr(BuilderNextSortOrder(cboSector.Value))
    End If
End Sub

Private Sub txtPlotId_Change()
    If Len(Trim$(txtOutputFile.Value)) = 0 And Len(Trim$(txtPlotId.Value)) > 0 Then
        txtOutputFile.Value = Trim$(txtPlotId.Value) & ".svg"
    End If
End Sub

Private Sub cmdAddDataSource_Click()
    frmDataSourceWizard.Show
    BuilderFillComboFromColumn cboDataSourceId, "Data Sources", "Data Source ID"
End Sub

Private Sub cmdLoadFields_Click()
    If Len(Trim$(cboDataSourceId.Value)) = 0 Then
        MsgBox "Select a data source first.", vbExclamation
        cboDataSourceId.SetFocus
        Exit Sub
    End If

    BuilderFillCombosWithExcelHeaders _
        cboDataSourceId.Value, _
        cboXField, _
        cboGroupField, _
        cboColumnValue, _
        cboLineValue
End Sub

Private Sub cmdSave_Click()
    If Not ValidateForm() Then Exit Sub

    BuilderSaveChartForm Me

    MsgBox "Chart added to the Charts sheet.", vbInformation
    Unload Me
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Function ValidateForm() As Boolean
    ValidateForm = False

    If Len(Trim$(txtPlotId.Value)) = 0 Then
        MsgBox "Plot ID is required.", vbExclamation
        txtPlotId.SetFocus
        Exit Function
    End If

    If Len(Trim$(cboSector.Value)) = 0 Then
        MsgBox "Sector is required.", vbExclamation
        cboSector.SetFocus
        Exit Function
    End If

    If Len(Trim$(cboPlotFunction.Value)) = 0 Then
        MsgBox "Plot function is required.", vbExclamation
        cboPlotFunction.SetFocus
        Exit Function
    End If

    If Len(Trim$(cboDataSourceId.Value)) = 0 Then
        MsgBox "Data source is required.", vbExclamation
        cboDataSourceId.SetFocus
        Exit Function
    End If

    If Len(Trim$(txtOutputFile.Value)) = 0 Then
        MsgBox "Output file is required.", vbExclamation
        txtOutputFile.SetFocus
        Exit Function
    End If

    If Len(Trim$(cboXField.Value)) = 0 Then
        MsgBox "X field is required.", vbExclamation
        cboXField.SetFocus
        Exit Function
    End If

    If Len(Trim$(cboColumnValue.Value)) = 0 And Len(Trim$(cboLineValue.Value)) = 0 Then
        MsgBox "Select at least one value field: Column Value or Line Value.", vbExclamation
        cboColumnValue.SetFocus
        Exit Function
    End If

    ValidateForm = True
End Function
