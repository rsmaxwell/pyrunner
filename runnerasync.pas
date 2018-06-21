unit RunnerAsync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, fpjson, jsonparser, StreamReader, RunnerInterfaces;



type
    MyRunnerAsync = class(TInterfacedObject, IMyRunnerObserved)
    private
        proc: TProcess;
        outputReader : MyReader;
        errorReader : MyReader;
        observers: TInterfaceList; 
        procedure WriteLn(line: AnsiString);
        function makeToken() : string;
    public
        constructor Create;
        procedure Read(var lines: TStrings);
        procedure Errors(var lines: TStrings);
        procedure attachObserver( observer : IMyRunnerObserver );
        procedure detachObserver( observer : IMyRunnerObserver );

        function CreateArray(field : AnsiString) : string;
        function ExtendArray( field : AnsiString; list : array of real ) : string;
        function RunPythonFunction( pythonFunction : AnsiString) : string;
        function GetField(field : AnsiString) : string; 
        function Close() : string;
    end;




implementation

const
    CarriageReturn: Byte = 13;
    LineFeed: Byte = 10;


// *****************************************************************************
// * Basic
// *****************************************************************************

constructor MyRunnerAsync.Create();
begin
    observers := TInterfaceList.Create;

    proc := TProcess.Create(nil);
    proc.Executable:= FindDefaultExecutablePath('python.exe');
    proc.Parameters.Add('server.py');
    proc.Options := proc.Options + [poUsePipes, poNoConsole];
    proc.Execute;

    outputReader := MyReader.Create(proc.Output, TMyRunnerOperation.stdout, observers);
    errorReader := MyReader.Create(proc.Stderr, TMyRunnerOperation.stderr, observers);
end;


procedure MyRunnerAsync.WriteLn(line: AnsiString);
var
    bytes: TBytes;
    i: integer;
    b: Byte;
begin
    bytes := BytesOf(line);

    for i := 0 to length(bytes) - 1 do
    begin
        b := bytes[i];
        proc.Input.WriteByte( b );
    end;

    proc.Input.WriteByte( CarriageReturn );
    proc.Input.WriteByte( LineFeed );
end;


procedure MyRunnerAsync.Read(var lines: TStrings);
begin
    outputReader.Read( lines );
end;


procedure MyRunnerAsync.Errors(var lines: TStrings);
begin
    errorReader.Read( lines );
end;

function MyRunnerAsync.Close(): string;
var
    command : string;
    jObject : TJSONObject;
    token : string;

begin
    jObject := TJSONObject.Create();
    jObject.Add('command', 'quit');

    token := makeToken();
    jObject.Add('token', token);

    command := jObject.AsJSON;
    WriteLn(command);

    Sleep(100);

    if not outputReader.Finished then
        outputReader.Terminate();

    if not errorReader.Finished then
        errorReader.Terminate();

    proc.Terminate(0);

    Close := token;
end;

function MyRunnerAsync.makeToken() : string;
var
    GUID: TGuid;
begin
    CreateGUID( GUID );
    makeToken := GUIDToString( GUID );
end;


// *****************************************************************************
// * Observers
// *****************************************************************************

procedure MyRunnerAsync.attachObserver( observer : IMyRunnerObserver );
begin
    if observers.IndexOf(observer) = -1 then
        observers.Add(IUnknown(observer));
end;

procedure MyRunnerAsync.detachObserver( observer : IMyRunnerObserver );
begin
    observers.Remove(observer);
end;

// *****************************************************************************
// * Helpers
// *****************************************************************************
function MyRunnerAsync.CreateArray( field : AnsiString ) : string;
var
    python : AnsiString;
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray;
    token : string;

begin
    // data["array"] = [] )

    python := 'data["' + field + '"] = []';

    jObject := TJSONObject.Create();
    jObject.Add('command', 'run');

    jArray := TJSONArray.Create();
    jArray.Add( python );
    jObject.Add('arguments', jArray);

    token := makeToken();
    jObject.Add('token', token);

    command := jObject.AsJSON;
    WriteLn(command);

    CreateArray := token;
end;

function MyRunnerAsync.ExtendArray( field : AnsiString; list : array of real ) : string;
var
    python : AnsiString;
    command : AnsiString;
    sep : string;
    jObject : TJSONObject;
    jArray : TJSONArray; 
    token : string;
    i : integer;
    value : real;

begin

    // data["array"].extend( (11,12,13) )

    python := 'data["' + field + '"].extend( (';
    sep := '';
    for i := 0 to Length(list) - 1 do
    begin
        value := list[i];
        python := python + sep + FloatToStr(value);
        sep := ', ';
    end;
    python := python + ') )';

    jObject := TJSONObject.Create();
    jObject.Add('command', 'run');

    jArray := TJSONArray.Create();
    jArray.Add( python );
    jObject.Add('arguments', jArray);

    token := makeToken();
    jObject.Add('token', token);

    command := jObject.AsJSON;
    WriteLn(command);

    ExtendArray := token;
end;


function MyRunnerAsync.RunPythonFunction( pythonFunction : AnsiString) : string;
var
    python : AnsiString;
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray;
    token : string;

begin

    // foobar()

    python := pythonFunction + '()';

    jObject := TJSONObject.Create();
    jObject.Add('command', 'run');

    jArray := TJSONArray.Create();
    jArray.Add( python );
    jObject.Add('arguments', jArray);

    token := makeToken();
    jObject.Add('token', token);

    command := jObject.AsJSON;
    WriteLn(command);

    RunPythonFunction := token
end;


function MyRunnerAsync.GetField( field : AnsiString ) : string;
var
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray; 
    token : string;

begin
    jObject := TJSONObject.Create();
    jObject.Add('command', 'get');

    jArray := TJSONArray.Create();
    jArray.Add( field );
    jObject.Add('arguments', jArray);

    token := makeToken();
    jObject.Add('token', token);

    command := jObject.AsJSON;
    WriteLn(command);

    GetField := token;
end;


end.


