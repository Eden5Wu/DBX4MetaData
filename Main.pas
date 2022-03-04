unit Main;

interface

uses
  DBXCommon, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, DB, StdCtrls, SqlExpr, DBXMetaDataNames, DBXMSSQL;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    cn: TSQLConnection;
    GroupBox1: TGroupBox;
    cbxTableName: TComboBox;
    Label1: TLabel;
    btnCRAutoInc: TButton;
    Button5: TButton;
    GroupBox2: TGroupBox;
    btnDSAutoInc: TButton;
    btnCRPrimaryKeys: TButton;
    btnDSIndexes: TButton;
    btnDSPrimaryKeys: TButton;
    btnCRForeignKey: TButton;
    btnCRForeignKeyColumns: TButton;
    btnCRPrimaryKeyColumns: TButton;
    cbxIndexes: TComboBox;
    Label2: TLabel;
    procedure btnCRAutoIncClick(Sender: TObject);
    procedure btnDSAutoIncClick(Sender: TObject);
    procedure btnDSIndexesClick(Sender: TObject);
    procedure btnDSPrimaryKeysClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnCRPrimaryKeysClick(Sender: TObject);
    procedure btnCRForeignKeyClick(Sender: TObject);
    procedure btnCRForeignKeyColumnsClick(Sender: TObject);
    procedure btnCRPrimaryKeyColumnsClick(Sender: TObject);
    procedure cbxTableNameCloseUp(Sender: TObject);
    procedure cbxIndexesCloseUp(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

function DBXGetConnection(const AHostName, ADatabase, AUserName,
  APassword: string): TDBXConnection; overload;
function DBXGetConnection(const AParams: TStrings): TDBXConnection; overload;

implementation

uses
  DBXMetaDataProvider, DBXTypedTableStorage, DBExpress.MSSQL.Factory;

{$R *.dfm}

function DBXGetConnection(const AHostName, ADatabase, AUserName,
  APassword: string): TDBXConnection; overload;
// uses DBXDevartSQLServer
const DEVART_MSSQL_CONNECTION_STRING = ''
  +'%s=DevartSQLServer;%s=%s;%s=%s;%s=%s;%s=%s'
  +';BlobSize=-1;SchemaOverride=%%.dbo;LongStrings=True;EnableBCD=True'
  +';FetchAll=True;UseUnicode=True;IPVersion=IPv4'; // Devart can add Custom String ('Custom String=ApplicationName=A_NAME;WorkstationID=WID');
// uses DbxMsSql
const MSSQL_CONNECTION_STRING = ''
  +'%s=MSSQL;%s=%s;%s=%s;%s=%s;%s=%s'
  +';SchemaOverride=%%.dbo;BlobSize=-1;ErrorResourceFile=;LocaleCode=0000'
  +';IsolationLevel=ReadCommitted;OS Authentication=False;Prepare SQL=False'
  +';ConnectTimeout=60;Mars_Connection=False';
var
  LProperties: TDBXProperties;
begin
  LProperties := TDBXProperties.Create;
  try
    LProperties.SetProperties(Format(DEVART_MSSQL_CONNECTION_STRING,
                          [TDBXPropertyNames.DriverName,
                           TDBXPropertyNames.HostName, AHostName,
                           TDBXPropertyNames.Database, ADatabase,
                           TDBXPropertyNames.UserName, AUserName,
                           TDBXPropertyNames.Password, APassword]));
    Result := TDBXConnectionFactory.GetConnectionFactory.GetConnection(LProperties);
  finally
    FreeAndNil(LProperties);
  end;
end;

function DBXGetConnection(const AParams: TStrings): TDBXConnection; overload;
var
  LProperties: TDBXProperties;
begin
  LProperties := TDBXProperties.Create;
  try
    LProperties.MergeProperties(AParams);
    Result := TDBXConnectionFactory.GetConnectionFactory.GetConnection(LProperties);
  finally
    FreeAndNil(LProperties);
  end;
end;

procedure TForm2.btnCRAutoIncClick(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
  Dbx: TDBXConnection;
  Cmd: TDBXCommand;
  R: TDBXReader;
  LTempStr: string;
begin
  Memo1.Clear();
  p_Start:=GetTickCount();
  Dbx := DBXGetConnection(cn.Params);
  Cmd := Dbx.CreateCommand;
  Cmd.CommandType := TDBXCommandTypes.DbxMetaData;
  Cmd.Text := TDBXMetaDataCommands.GetColumns + ' ' + cbxTableName.Text;
  R := Cmd.ExecuteQuery;
  while R.Next do
  begin
    // Index performance is HIGH!
    if R.Value[TDBXColumnsIndex.IsAutoIncrement].AsBoolean then
      Memo1.Lines.Add(R.Value[TDBXColumnsColumns.ColumnName].AsString);
  end;
  R.Free;
  Cmd.Free;
  Dbx.Free;
  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
(*
  The unit DBXMetaDataNames has been provided to read metadata.
  The dbExpress class TDBXMetaDataCommands provides a set of constants
  to read various types of metadata.
  Set the Data.DBXCommon.TDBXCommand.CommandType property to
    Data.DBXCommon.TDBXCommandTypes.DBXMetadata and
    set Data.DBXCommon.TDBXCommand.Text to one of the constants
    in TDBXMetaDataCommands to acquire the designated metadata.
  Data.DBXCommon.TDBXCommand.ExecuteQuery returns a Data.DBXCommon.TDBXReader
  to access the metadata. The new classes in DBXMetaDataNames describe
  and provide access to this metadata's columns.
*)
end;

procedure TForm2.btnCRPrimaryKeyColumnsClick(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
begin
  p_Start:=GetTickCount();
  with TDBXMSSQLFactory.Create(cn.Params) do
  begin
    Memo1.Lines.CommaText := DBXFetchPrimaryKeyColumns(cbxTableName.Text);
    Free;
  end;
  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
end;

procedure TForm2.btnCRPrimaryKeysClick(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
begin
  p_Start:=GetTickCount();
  with TDBXMSSQLFactory.Create(cn.Params) do
  begin
    Memo1.Lines.CommaText := DBXFetchPrimaryKeyColumnsWithoutAutoInc(cbxTableName.Text);
    Free;
  end;
  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
end;

procedure TForm2.btnDSAutoIncClick(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
  LCmd: TSQLDataSet;
  A_TableName, ss: string;
begin
  Memo1.Clear;
  A_TableName := cbxTableName.Text;
  LCmd:= TSQLDataSet.Create(nil);
  LCmd.SQLConnection := cn;
  p_Start:=GetTickCount();
  try
    LCmd.DbxCommandType := TDBXCommandTypes.DbxMetaData;
    LCmd.CommandText := TDBXMetaDataCommands.GetColumns + ' ' + A_TableName;
    LCmd.Open;
    while not LCmd.Eof do
    begin
      if LCmd.Fields[TDBXColumnsIndex.IsAutoIncrement].AsBoolean then
        Memo1.Lines.Add(LCmd.Fields[TDBXColumnsIndex.ColumnName].AsString);
      LCmd.Next();
    end;
  Finally
    FreeAndNil(LCmd);
  end;

  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
end;

procedure TForm2.btnDSIndexesClick(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
  v_SDS: TSQLDataSet;
  A_TableName, ss: string;
begin
  Memo1.Clear;
  A_TableName := cbxTableName.Text;
  v_SDS:= TSQLDataSet.Create(nil);
  v_SDS.SQLConnection := cn;
  p_Start:=GetTickCount();
  try
    v_SDS.DbxCommandType := TDBXCommandTypes.DbxMetaData;
    v_SDS.CommandText := TDBXMetaDataCommands.GetIndexes + ' ' + A_TableName;
    v_SDS.Open;
    while not v_SDS.Eof do
    begin
      Memo1.Lines.Add(v_SDS.Fields[TDBXIndexesIndex.IndexName].AsString);
      if v_SDS.FieldByName(TDBXIndexesColumns.IsPrimary).AsBoolean then
        Memo1.Lines.Add(TDBXIndexesColumns.IsPrimary);
      v_SDS.Next();
    end;
  Finally
    FreeAndNil(v_SDS);
  end;

  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
end;

procedure TForm2.btnDSPrimaryKeysClick(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
  vIndexDS, vPrimaryDS: TSQLDataSet;
  A_TableName, ss: string;
begin
  Memo1.Clear;
  //A_TableName := Edit1.Text;
  vIndexDS:= TSQLDataSet.Create(nil);
  vPrimaryDS := TSQLDataSet.Create(nil);
  vIndexDS.SQLConnection := cn;
  vPrimaryDS.SQLConnection := cn;
  p_Start:=GetTickCount();

  vIndexDS.DbxCommandType := TDBXCommandTypes.DbxMetaData;
  vPrimaryDS.DbxCommandType := TDBXCommandTypes.DbxMetaData;
  vIndexDS.CommandText := TDBXMetaDataCommands.GetIndexes + ' ' + A_TableName;
  try
    vIndexDS.Open;
    while not vIndexDS.Eof do
    begin
      if vIndexDS.Fields[TDBXIndexesIndex.IsPrimary].AsBoolean then
      begin
        vPrimaryDS.Close();
        vPrimaryDS.CommandText := TDBXMetaDataCommands.GetIndexColumns + ' ' + A_TableName
          + ' ' + vIndexDS.Fields[TDBXIndexesIndex.IndexName].AsString;
        vPrimaryDS.Open();
        while not vPrimaryDS.Eof do
        begin
          Memo1.Lines.Add(vPrimaryDS.Fields[TDBXIndexColumnsIndex.ColumnName].AsString);
          vPrimaryDS.Next();
        end;
      end;
      vIndexDS.Next();
    end;
    if Memo1.Lines.Count = 0 then
      btnDSAutoIncClick(btnDSAutoInc);
  Finally
    FreeAndNil(vIndexDS);
    FreeAndNil(vPrimaryDS);
  end;

  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
end;

procedure TForm2.btnCRForeignKeyClick(Sender: TObject);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
begin
  Memo1.Clear();
  LCommand := cn.DBXConnection.CreateCommand;
  try
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetForeignKeys + ' ' + cbxTableName.Text;
    LReader := LCommand.ExecuteQuery;
    try
      while LReader.Next do
      begin
        Memo1.Lines.Add(LReader.Value[TDBXForeignKeysIndex.ForeignKeyName].AsString);
        btnCRForeignKeyColumnsClick(btnCRForeignKeyColumns);
//        try
//          ATableInfo.AddForeignKey(LForeignKeyInfo);
//          SetForeignKeyColumns(LForeignKeyInfo);
//        except
//          FreeAndNil(LForeignKeyInfo);
//          raise;
//        end;
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LCommand);
  end;
end;

procedure TForm2.btnCRForeignKeyColumnsClick(Sender: TObject);
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
begin
  if Memo1.Lines.Count = 0 then
    Exit;
  LCommand := cn.DBXConnection.CreateCommand;
  try
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetForeignKeyColumns + ' '
      + cbxTableName.Text + ' ' + Memo1.Lines.Strings[Memo1.Lines.Count-1];
    LReader := LCommand.ExecuteQuery;
    try
      while LReader.Next do
      begin
        Memo1.Lines.Add(LReader.Value[TDBXForeignKeyColumnsIndex.PrimaryTableName].AsString);
        Memo1.Lines.Add(LReader.Value[TDBXForeignKeyColumnsIndex.ColumnName].AsString);
        Memo1.Lines.Add(LReader.Value[TDBXForeignKeyColumnsIndex.PrimaryColumnName].AsString);
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LCommand);
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
begin
  p_Start:=GetTickCount();
  cn.GetFieldNames(cbxTableName.Text, Memo1.Lines);
  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
end;

procedure TForm2.cbxIndexesCloseUp(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
  LFactory: TDBXMSSQLFactory;
begin
  p_Start:=GetTickCount();
  LFactory := TDBXMSSQLFactory.Create(cn.Params);
  Memo1.Lines.CommaText := LFactory.DBXFetchIndexColumns(cbxTableName.Text, TComboBox(Sender).Text);
  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
  LFactory.Free;
end;

procedure TForm2.cbxTableNameCloseUp(Sender: TObject);
var
  p_Start, p_Cost : DWORD;
  LFactory: TDBXMSSQLFactory;
begin
  cbxIndexes.Items.Clear;
  p_Start:=GetTickCount();
  LFactory := TDBXMSSQLFactory.Create(cn.Params);
  cbxIndexes.Items.CommaText := LFactory.DBXFetchIndexes(TComboBox(Sender).Text);
  p_Cost:=GetTickCount()-p_Start;
  OutputDebugString(PWideChar('spend time: '+format('%0.3n',[p_Cost/1000])+'s'));
  LFactory.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  TDBXMSSQLFactory.FillConnectionParams(cn, '127.0.0.1', 'EdenDemos', 'SA_Eden', 'IL@veEden!', False);
  cn.Open;
  cn.GetTableNames(cbxTableName.Items);
end;

end.
