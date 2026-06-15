VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmDataSourceWizard
   Caption         =   "Add Data Source"
   ClientHeight    =   4320
   ClientLeft      =   120
   ClientTop       =   465
   ClientWidth     =   6600
   StartUpPosition =   1  'CenterOwner
   Begin MSForms.Frame fraMain
      Caption         =   "Data source details"
      Height          =   3420
      Left            =   180
      TabIndex        =   0
      Top             =   120
      Width           =   6240
      Begin MSForms.TextBox txtDataSourceId
         Height          =   240
         Left            =   1740
         TabIndex        =   1
         Top             =   360
         Width           =   3900
      End
      Begin MSForms.ComboBox cboSourceType
         Height          =   315
         Left            =   1740
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   720
         Width           =   1800
      End
      Begin MSForms.TextBox txtSourceRef
         Height          =   240
         Left            =   1740
         TabIndex        =   3
         Top             =   1140
         Width           =   3180
      End
      Begin MSForms.CommandButton cmdBrowse
         Caption         =   "Browse..."
         Height          =   300
         Left            =   5040
         TabIndex        =   4
         Top             =   1110
         Width           =   960
      End
      Begin MSForms.TextBox txtSheet
         Height          =   240
         Left            =   1740
         TabIndex        =   5
         Top             =   1560
         Width           =   3900
      End
      Begin MSForms.TextBox txtRange
         Height          =   240
         Left            =   1740
         TabIndex        =   6
         Top             =   1980
         Width           =   3900
      End
      Begin MSForms.TextBox txtDataFunction
         Height          =   240
         Left            =   1740
         TabIndex        =   7
         Top             =   2400
         Width           =   3900
      End
      Begin MSForms.CheckBox chkCache
         Caption         =   "Cache data"
         Height          =   240
         Left            =   1740
         TabIndex        =   8
         Top             =   2760
         Width           =   1800
      End
      Begin MSForms.TextBox txtNotes
         Height          =   240
         Left            =   1740
         TabIndex        =   9
         Top             =   3060
         Width           =   3900
      End
      Begin MSForms.Label lblDataSourceId
         Caption         =   "Data Source ID"
         Height          =   240
         Left            =   240
         TabIndex        =   10
         Top             =   390
         Width           =   1320
      End
      Begin MSForms.Label lblSourceType
         Caption         =   "Source Type"
         Height          =   240
         Left            =   240
         TabIndex        =   11
         Top             =   780
         Width           =   1320
      End
      Begin MSForms.Label lblSourceRef
         Caption         =   "Source Ref"
         Height          =   240
         Left            =   240
         TabIndex        =   12
         Top             =   1170
         Width           =   1320
      End
      Begin MSForms.Label lblSheet
         Caption         =   "Sheet"
         Height          =   240
         Left            =   240
         TabIndex        =   13
         Top             =   1590
         Width           =   1320
      End
      Begin MSForms.Label lblRange
         Caption         =   "Range"
         Height          =   240
         Left            =   240
         TabIndex        =   14
         Top             =   2010
         Width           =   1320
      End
      Begin MSForms.Label lblDataFunction
         Caption         =   "Data Function"
         Height          =   240
         Left            =   240
         TabIndex        =   15
         Top             =   2430
         Width           =   1320
      End
      Begin MSForms.Label lblNotes
         Caption         =   "Notes"
         Height          =   240
         Left            =   240
         TabIndex        =   16
         Top             =   3090
         Width           =   1320
      End
   End
   Begin MSForms.CommandButton cmdSave
      Caption         =   "Save Data Source"
      Default         =   -1  'True
      Height          =   360
      Left            =   3360
      TabIndex        =   17
      Top             =   3720
      Width           =   1500
   End
   Begin MSForms.CommandButton cmdCancel
      Caption         =   "Cancel"
      Height          =   360
      Left            =   5040
      TabIndex        =   18
      Top             =   3720
      Width           =   1140
   End
End
Attribute VB_Name = "frmDataSourceWizard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub UserForm_Initialize()
    BuilderFillComboFromArray cboSourceType, Array("excel", "function")
    cboSourceType.Value = "excel"
End Sub

Private Sub cboSourceType_Change()
    Dim isExcel As Boolean
    isExcel = (LCase$(cboSourceType.Value) = "excel")

    txtSourceRef.Enabled = isExcel
    txtSheet.Enabled = isExcel
    txtRange.Enabled = isExcel
    cmdBrowse.Enabled = isExcel
    txtDataFunction.Enabled = Not isExcel
End Sub

Private Sub cmdBrowse_Click()
    Dim selectedFile As Variant
    selectedFile = Application.GetOpenFilename("Excel files (*.xlsx;*.xlsm;*.xls),*.xlsx;*.xlsm;*.xls", , "Select data workbook")

    If selectedFile = False Then Exit Sub
    txtSourceRef.Value = MakeRelativePath(CStr(selectedFile))
End Sub

Private Sub cmdSave_Click()
    If Len(Trim$(txtDataSourceId.Value)) = 0 Then
        MsgBox "Data Source ID is required.", vbExclamation
        txtDataSourceId.SetFocus
        Exit Sub
    End If

    If Len(Trim$(cboSourceType.Value)) = 0 Then
        MsgBox "Source Type is required.", vbExclamation
        cboSourceType.SetFocus
        Exit Sub
    End If

    If LCase$(cboSourceType.Value) = "excel" Then
        If Len(Trim$(txtSourceRef.Value)) = 0 Or Len(Trim$(txtSheet.Value)) = 0 Then
            MsgBox "Excel data sources need Source Ref and Sheet.", vbExclamation
            Exit Sub
        End If
    ElseIf LCase$(cboSourceType.Value) = "function" Then
        If Len(Trim$(txtDataFunction.Value)) = 0 Then
            MsgBox "Function data sources need Data Function.", vbExclamation
            txtDataFunction.SetFocus
            Exit Sub
        End If
    End If

    BuilderSaveDataSourceForm Me

    MsgBox "Data source added.", vbInformation
    Unload Me
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Function MakeRelativePath(ByVal fullPath As String) As String
    Dim rootPath As String
    rootPath = BuilderProjectRoot()

    If LCase$(Left$(fullPath, Len(rootPath) + 1)) = LCase$(rootPath & "\") Then
        MakeRelativePath = Mid$(fullPath, Len(rootPath) + 2)
    Else
        MakeRelativePath = fullPath
    End If
End Function
