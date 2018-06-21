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
    public
        constructor Create;
        procedure WriteLn(line: AnsiString);
        procedure Read(var lines: TStrings);
        procedure Errors(var lines: TStrings);
        procedure Close();
        procedure attachObserver( observer : IMyRunnerObserver );
        procedure detachObserver( observer : IMyRunnerObserver );

        procedure CreateArray(field : AnsiString);
        procedure ExtendArray( field : AnsiString; list : array of real );
        procedure RunPythonFunction( pythonFunction : AnsiString);
        procedure GetField(field : AnsiString);
    end;




implementation

const
    CarriageReturn: Byte = 13;
    LineFeed: Byte = 10;



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

procedure MyRunnerAsync.Close();
var
    command : string;
    jObject : TJSONObject;

begin
    jObject := TJSONObject.Create();
    jObject.Add('command', 'quit');

    command := jObject.AsJSON;
    WriteLn(command);

    Sleep(100);

    if not outputReader.Finished then
        outputReader.Terminate();

    if not errorReader.Finished then
        errorReader.Terminate();

    proc.Terminate(0);
end;

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
//* Helpers
// *****************************************************************************
procedure MyRunnerAsync.CreateArray( field : AnsiString );
var
    python : AnsiString;
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray;

begin
    // data["array"] = [] )

    python := 'data["' + field + '"] = []';

    jObject := TJSONObject.Create();
    jObject.Add('command', 'run');

    jArray := TJSONArray.Create();
    jArray.Add( python );
    jObject.Add('arguments', jArray);

    command := jObject.AsJSON;
    WriteLn(command);
end;

procedure MyRunnerAsync.ExtendArray( field : AnsiString; list : array of real );
var
    python : AnsiString;
    command : AnsiString;
    sep : string;
    jObject : TJSONObject;
    jArray : TJSONArray;
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

    command := jObject.AsJSON;
    WriteLn(command);
end;


procedure MyRunnerAsync.RunPythonFunction( pythonFunction : AnsiString);
var
    python : AnsiString;
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray;

begin

    // foobar()

    python := pythonFunction + '()';

    jObject := TJSONObject.Create();
    jObject.Add('command', 'run');

    jArray := TJSONArray.Create();
    jArray.Add( python );
    jObject.Add('arguments', jArray);

    command := jObject.AsJSON;
    WriteLn(command);
end;


procedure MyRunnerAsync.GetField( field : AnsiString );
var
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray;

begin
    jObject := TJSONObject.Create();
    jObject.Add('command', 'get');

    jArray := TJSONArray.Create();
    jArray.Add( field );
    jObject.Add('arguments', jArray);

    command := jObject.AsJSON;
    WriteLn(command);
end;


end.


