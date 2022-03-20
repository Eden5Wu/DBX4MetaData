unit EdenSqlExprHelper;

interface

uses
  DBXCommon, DBXMetaDataNames, SqlExpr, Classes;

type
  TSQLConnectionHelper = class helper for TSQLConnection
  public
    // Fetch Indexes
    function FetchIndexes(const ATableName: string): string;
    function FetchFKIndexes(const ATableName: string): string;
    function FetchFKFieldNames(const ATableName: string):string;

    // Fetch Fields
    function FetchAutoIncFieldNames(const ATableName: string): string;
    function FetchFieldNames(const ATableName: string;
      AAutoIncrement: Boolean = False): string;
    function FetchKeyFieldNames(const ATableName: string;
      AAutoIncrement: Boolean = False): string;
    function FetchIndexFieldNames(const ATableName, AIndexName: string): string;

    procedure FillConnectionParams(const AHost: string; const ADBName: string;
      const AUserName: string; const APassword: string);
  end;

implementation

uses
  DB, SqlConst, StrUtils, Types,
  DBXMetaDataProvider, DBXDataExpressMetaDataProvider;

procedure DelLastComma(var AText: String);
begin
  if RightStr(AText, 1) = ',' then
    AText := Copy(AText, 1, Length(AText) - 1);
end;

function DBXGetMetaProvider(const ADBXConnection: TDBXConnection): TDBXMetaDataProvider;
var
  Provider: TDBXDataExpressMetaDataProvider;
begin
  Provider := TDBXDataExpressMetaDataProvider.Create(ADBXConnection);
  try
    Provider.Open;
  except
    Provider.Free;
    raise;
  end;
  Result := Provider;
end;

{ TSQLConnectionHelper }

function TSQLConnectionHelper.FetchIndexes(const ATableName: string): string;
var
  LMetaCommand: TDBXCommand;
  LMetaReader: TDBXReader;
begin
  with DBXGetMetaProvider(Self.DBXConnection) do
  begin
    LMetaCommand := Self.DBXConnection.CreateCommand;
    LMetaCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LMetaCommand.Text := TDBXMetaDataCommands.GetIndexes + ' '
      + QuoteIdentifierIfNeeded(ATableName);
    LMetaReader := LMetaCommand.ExecuteQuery;
    while LMetaReader.Next do
    begin
      Result := Result
        + (LMetaReader.Value[TDBXIndexesIndex.IndexName].AsString)
        + ',';
    end;
    DelLastComma(Result);
    LMetaReader.Free;
    LMetaCommand.Free;
    Free;
  end;
end;

function TSQLConnectionHelper.FetchIndexFieldNames(const ATableName,
  AIndexName: string): string;
var
  LMetaCommand: TDBXCommand;
  LMetaReader: TDBXReader;
begin
  with DBXGetMetaProvider(Self.DBXConnection) do
  begin
    LMetaCommand := Self.DBXConnection.CreateCommand;
    LMetaCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LMetaCommand.Text := TDBXMetaDataCommands.GetIndexColumns + ' '
      + QuoteIdentifierIfNeeded(ATableName)
      + ' ' +  QuoteIdentifierIfNeeded(AIndexName);
    LMetaReader := LMetaCommand.ExecuteQuery;
    while LMetaReader.Next do
    begin
      Result := Result + LMetaReader.Value[TDBXIndexColumnsIndex.ColumnName].AsString + ',';
    end;
    DelLastComma(Result);
    LMetaReader.Free;
    LMetaCommand.Free;
    Free;
  end;
end;

function TSQLConnectionHelper.FetchAutoIncFieldNames(const ATableName
  : string): string;
var
  LMetaCommand: TDBXCommand;
  LMetaReader: TDBXReader;
  LSplit: string;
begin
  if ATableName = '' then
    Exit('');
  if Self.DBXConnection = nil then
    DatabaseError(sConnectionNameMissing);

  with DBXGetMetaProvider(Self.DBXConnection) do
  begin
    LMetaCommand := Self.DBXConnection.CreateCommand;
    LMetaCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LMetaCommand.Text := TDBXMetaDataCommands.GetColumns + ' '
      + QuoteIdentifierIfNeeded(ATableName);
    LMetaReader := LMetaCommand.ExecuteQuery;
    while LMetaReader.Next do
    begin
      if LMetaReader.Value[TDBXColumnsIndex.IsAutoIncrement].AsBoolean then
        Result := Result + LSplit +
          (LMetaReader.Value[TDBXColumnsIndex.ColumnName].AsString);

      if (LSplit = '') and (Result <> '') then
        LSplit := ',';
    end;
    DelLastComma(Result);
    LMetaReader.Free;
    LMetaCommand.Free;
    Free;
  end;
end;

function TSQLConnectionHelper.FetchFieldNames(const ATableName: string;
  AAutoIncrement: Boolean): string;
var
  LMetaCommand: TDBXCommand;
  LMetaReader: TDBXReader;
  LSplit: string;
begin
  if ATableName = '' then
    Exit('');
  if Self.DBXConnection = nil then
    DatabaseError(sConnectionNameMissing);
  LMetaCommand := Self.DBXConnection.CreateCommand;
  LMetaCommand.CommandType := TDBXCommandTypes.DbxMetaData;
  LMetaCommand.Text := TDBXMetaDataCommands.GetColumns + ' ' + ATableName;
  LMetaReader := LMetaCommand.ExecuteQuery;
  while LMetaReader.Next do
  begin
    if AAutoIncrement or
      (not LMetaReader.Value[TDBXColumnsIndex.IsAutoIncrement].AsBoolean) then
      Result := Result + LSplit +
        (LMetaReader.Value[TDBXColumnsIndex.ColumnName].AsString);

    if (LSplit = '') and (Result <> '') then
      LSplit := ',';
  end;
  DelLastComma(Result);
  LMetaReader.Free;
  LMetaCommand.Free;
end;

function TSQLConnectionHelper.FetchFKFieldNames(
  const ATableName: string): string;
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
  LFkIndex: string;
  LFkIndexes: TStringDynArray;
begin
  if ATableName = '' then
    Exit('');
  if Self.DBXConnection = nil then
    DatabaseError(sConnectionNameMissing);
  LFkIndexes := StrUtils.SplitString(Self.FetchFKIndexes(ATableName), ',');
  with DBXGetMetaProvider(Self.DBXConnection) do
  begin
    for LFkIndex in LFkIndexes do
    begin
      LCommand := Self.DBXConnection.CreateCommand;
      LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
      LCommand.Text := TDBXMetaDataCommands.GetForeignKeyColumns + ' '
        + QuoteIdentifierIfNeeded(ATableName) + ' ' + QuoteIdentifierIfNeeded(LFkIndex);
      LReader := LCommand.ExecuteQuery;
      while LReader.Next do
      begin
        Result := Result + LReader.Value[TDBXForeignKeyColumnsIndex.ColumnName].AsString + ','
      end;
      LReader.Free;
      LCommand.Free;
    end;
    DelLastComma(Result);
    Free;
  end;
end;

function TSQLConnectionHelper.FetchFKIndexes(const ATableName: string): string;
var
  LCommand: TDBXCommand;
  LReader: TDBXReader;
begin
  if ATableName = '' then
    Exit('');
  if Self.DBXConnection = nil then
    DatabaseError(sConnectionNameMissing);

  with DBXGetMetaProvider(Self.DBXConnection) do
  begin
    LCommand := Self.DBXConnection.CreateCommand;
    LCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LCommand.Text := TDBXMetaDataCommands.GetForeignKeys + ' ' + QuoteIdentifierIfNeeded(ATableName);
    LReader := LCommand.ExecuteQuery;
    while LReader.Next do
    begin
      Result := Result + LReader.Value[TDBXForeignKeysIndex.ForeignKeyName].AsString + ',';
    end;
    LReader.Free;
    LCommand.Free;
    DelLastComma(Result);
    Free;
  end;
end;

function TSQLConnectionHelper.FetchKeyFieldNames(const ATableName: string;
  AAutoIncrement: Boolean): string;
var
  LIndexCommand, LPrimaryCommand: TDBXCommand;
  LIndexReader, LPrimaryReader: TDBXReader;
  LPKIndexName: string;
  LAutoIncrementFields: TStringList;
begin
  if ATableName = '' then
    Exit('');
  if Self.DBXConnection = nil then
    DatabaseError(sConnectionNameMissing);

  with DBXGetMetaProvider(Self.DBXConnection) do
  begin
    LIndexCommand := Self.DBXConnection.CreateCommand;
    LIndexCommand.CommandType := TDBXCommandTypes.DbxMetaData;
    LIndexCommand.Text := TDBXMetaDataCommands.GetIndexes + ' ' + QuoteIdentifierIfNeeded(ATableName);
    LIndexReader := LIndexCommand.ExecuteQuery;
    while LIndexReader.Next do
    begin
      if LIndexReader.Value[TDBXIndexesIndex.IsPrimary].AsBoolean then
      begin
        LPKIndexName := LIndexReader.Value[TDBXIndexesIndex.IndexName].AsString;
        Break;
      end;
    end;
    LIndexReader.Free;
    LIndexCommand.Free;

    if LPKIndexName <> '' then
    begin
      LAutoIncrementFields := TStringList.Create;
      LAutoIncrementFields.CommaText := Self.FetchAutoIncFieldNames(ATableName);

      LPrimaryCommand := Self.DBXConnection.CreateCommand;
      LPrimaryCommand.CommandType := TDBXCommandTypes.DbxMetaData;
      LPrimaryCommand.Text := TDBXMetaDataCommands.GetIndexColumns + ' '
        + QuoteIdentifierIfNeeded(ATableName) + ' '
        + QuoteIdentifierIfNeeded(LPKIndexName);
      LPrimaryReader := LPrimaryCommand.ExecuteQuery();
      while LPrimaryReader.Next do
      begin
        if AAutoIncrement or
          (LAutoIncrementFields.IndexOf(LPrimaryReader.Value[TDBXIndexColumnsIndex.ColumnName].AsString) < 0) then
          Result := Result + LPrimaryReader.Value[TDBXIndexColumnsIndex.ColumnName].AsString + ',';
      end;
      LPrimaryReader.Free;
      LPrimaryCommand.Free;
      LAutoIncrementFields.Free;
      DelLastComma(Result);
    end;
    Free;
  end;
end;

procedure TSQLConnectionHelper.FillConnectionParams(const AHost, ADBName,
  AUserName, APassword: string);
begin
  {$REGION 'Devart DBX for MSSQL'}
  Self.GetDriverFunc := 'getSQLDriverSQLServerDirect';
  Self.DriverName := 'DevartSQLServerDirect';
  Self.LibraryName := 'dbexpsda40.dll';
  Self.Params.Clear;
  Self.Params.Add('User_Name='+AUserName);
  Self.Params.Add('Password='+APassword);
  Self.Params.Add('HostName='+AHost);
  Self.Params.Add('Database='+ADBName);
  Self.Params.Add('BlobSize=-1');
  Self.Params.Add('SchemaOverride=%.dbo');
  Self.Params.Add('LongStrings=True');
  Self.Params.Add('EnableBCD=True');
  Self.Params.Add('FetchAll=True');
  Self.Params.Add('UseUnicode=True');
  Self.Params.Add('IPVersion=IPv4');
  {$ENDREGION}
  Exit;
  {$REGION 'Default DBX for MSSQL'}
  Self.GetDriverFunc := 'getSQLDriverMSSQL';
  Self.DriverName := 'MSSQL';
  Self.LibraryName := 'dbxmss.dll';
  Self.VendorLib := 'sqlncli10.dll';
  Self.Params.Clear;
  Self.Params.Add('User_Name='+AUserName);
  Self.Params.Add('Password='+APassword);
  Self.Params.Add('HostName='+AHost);
  Self.Params.Add('Database='+ADBName);
  Self.Params.Add('BlobSize=-1');
  Self.Params.Add('SchemaOverride=%.dbo');
  Self.Params.Add('ErrorResourceFile=');
  Self.Params.Add('LocaleCode=0000');
  Self.Params.Add('IsolationLevel=ReadCommitted');
  Self.Params.Add('OS Authentication=' +
    IfThen(((AUserName='') and (APassword='')),'True','False'));
  Self.Params.Add('Prepare SQL=True');
  Self.Params.Add('ConnectTimeout=60');
  Self.Params.Add('Mars_Connection=True');
  {$ENDREGION}
end;

end.
