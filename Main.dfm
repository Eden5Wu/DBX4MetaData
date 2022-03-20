object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'dbExpress 4 MetaData Demo'
  ClientHeight = 665
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 23
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 82
    Height = 23
    Caption = 'Table box'
  end
  object Label2: TLabel
    Left = 8
    Top = 149
    Width = 102
    Height = 23
    Caption = 'Indexes box'
  end
  object Memo1: TMemo
    Left = 440
    Top = 0
    Width = 354
    Height = 665
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 233
    Width = 393
    Height = 232
    Caption = 'TDBXCommand + TDBXReader'
    TabOrder = 1
    object btnCRAutoInc: TButton
      Left = 199
      Top = 30
      Width = 177
      Height = 32
      Caption = 'Get AutoIncrement'
      TabOrder = 0
      OnClick = btnCRAutoIncClick
    end
    object btnCRPrimaryKeys: TButton
      Left = 16
      Top = 68
      Width = 177
      Height = 32
      Caption = 'Get PK no AutoInc'
      TabOrder = 1
      OnClick = btnCRPrimaryKeysClick
    end
    object btnCRForeignKey: TButton
      Left = 199
      Top = 68
      Width = 177
      Height = 32
      Caption = 'Get ForeignKeys'
      TabOrder = 2
      OnClick = btnCRForeignKeyClick
    end
    object btnCRForeignKeyColumns: TButton
      Left = 199
      Top = 106
      Width = 177
      Height = 32
      Caption = 'Get FK Columns'
      TabOrder = 3
      OnClick = btnCRForeignKeyColumnsClick
    end
    object btnCRPrimaryKeyColumns: TButton
      Left = 16
      Top = 30
      Width = 177
      Height = 32
      Caption = 'Get PK Columns'
      TabOrder = 4
      OnClick = btnCRPrimaryKeyColumnsClick
    end
  end
  object cbxTableName: TComboBox
    Left = 8
    Top = 34
    Width = 361
    Height = 31
    Style = csDropDownList
    TabOrder = 2
    OnCloseUp = cbxTableNameCloseUp
  end
  object Button5: TButton
    Left = 8
    Top = 78
    Width = 361
    Height = 61
    Caption = 'cn.GetFieldNames'
    TabOrder = 3
    OnClick = Button5Click
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 451
    Width = 393
    Height = 182
    Caption = 'TSQLDataSet + Cmd DbxMetaData'
    TabOrder = 4
    object btnDSAutoInc: TButton
      Left = 16
      Top = 31
      Width = 177
      Height = 62
      Caption = 'Get AutoIncrement'
      TabOrder = 0
      OnClick = btnDSAutoIncClick
    end
    object btnDSIndexes: TButton
      Left = 199
      Top = 31
      Width = 177
      Height = 62
      Caption = 'Get Indexes'
      TabOrder = 1
      OnClick = btnDSIndexesClick
    end
    object btnDSPrimaryKeys: TButton
      Left = 16
      Top = 99
      Width = 177
      Height = 62
      Caption = 'Get PrimaryKeys'
      TabOrder = 2
      OnClick = btnDSPrimaryKeysClick
    end
  end
  object cbxIndexes: TComboBox
    Left = 8
    Top = 178
    Width = 361
    Height = 31
    Style = csDropDownList
    TabOrder = 5
    OnCloseUp = cbxIndexesCloseUp
  end
  object cn: TSQLConnection
    ConnectionName = 'Devart SQL Server Direct'
    DriverName = 'DevartSQLServerDirect'
    GetDriverFunc = 'getSQLDriverSQLServerDirect'
    LibraryName = 'dbexpsda40.dll'
    LoginPrompt = False
    Params.Strings = (
      'BlobSize=-1'
      'SchemaOverride=%.dbo'
      'HostName=.\SQLEXPRESS'
      'DataBase=DBDEMOS'
      'DriverName=DevartSQLServerDirect'
      'User_Name='
      'Password='
      'LongStrings=True'
      'EnableBCD=True'
      'FetchAll=True'
      'UseUnicode=True'
      'IPVersion=IPv4')
    VendorLib = 'not used'
    Left = 36
    Top = 18
  end
end
