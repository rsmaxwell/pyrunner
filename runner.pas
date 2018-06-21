unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, jsonparser, RunnerAsync, RunnerInterfaces;



type
    MyRunner = class(TInterfacedObject, IMyRunnerObserver)
    private
        asyncClient: MyRunnerAsync;
        procedure notify( operation: TMyRunnerOperation );
    public
        constructor Create;
        procedure Close();
        procedure attachObserver( observer : IMyRunnerObserver );
        procedure detachObserver( observer : IMyRunnerObserver );

        procedure CreateArray(field : AnsiString);
        procedure ExtendArray( field : AnsiString; list : array of real );
        procedure RunPythonFunction( pythonFunction : AnsiString);
        procedure GetField(field : AnsiString);
    end;




implementation


// *****************************************************************************
//* Basic
// *****************************************************************************
constructor MyRunner.Create();
begin
    asyncClient := MyRunnerAsync.Create;
    asyncClient.attachObserver( self );
end;

procedure MyRunner.Close();
begin
    asyncClient.Close;
end;

procedure MyRunner.attachObserver( observer : IMyRunnerObserver );
begin
    asyncClient.attachObserver( observer );
end;

procedure MyRunner.detachObserver( observer : IMyRunnerObserver );
begin
    asyncClient.detachObserver( observer );
end;

// *****************************************************************************
//* Observer
// *****************************************************************************
procedure MyRunner.notify( operation: TMyRunnerOperation );
var
    opstring : string;
    lines : TStrings;
    line : AnsiString;
    i : integer;
begin
    WriteStr(opstring, operation);

    lines := TStringList.Create;

    if operation = TMyRunnerOperation.stdout then
        asyncClient.Read(lines)
    else
        asyncClient.Errors(lines);

    For i := 0 to lines.Count - 1 do
    begin
        line := lines[i];
        writeln(opstring + '   ' + line);
    end;
end;


// *****************************************************************************
//* Helpers
// *****************************************************************************
procedure MyRunner.CreateArray( field : AnsiString );
begin
    asyncClient.CreateArray( field );
end;

procedure MyRunner.ExtendArray( field : AnsiString; list : array of real );
begin
    asyncClient.ExtendArray( field, list );
end;


procedure MyRunner.RunPythonFunction( pythonFunction : AnsiString);
begin
    asyncClient.RunPythonFunction( pythonFunction );
end;


procedure MyRunner.GetField( field : AnsiString );
begin
    asyncClient.GetField( field );
end;
end.


