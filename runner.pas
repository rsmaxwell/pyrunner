unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, jsonparser, RunnerAsync, RunnerInterfaces, RunnerLogger;

type
    MyRunner = class(TInterfacedObject, IMyRunnerObserver)
    private
        asyncClient: MyRunnerAsync; 
        loggers: TInterfaceList;
        procedure notify( operation: TMyRunnerOperation );
    public
        constructor Create;
        procedure Close();
        procedure attachLogger( logger : IMyRunnerLogger );
        procedure detachLogger( logger : IMyRunnerLogger );

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
    loggers := TInterfaceList.Create;
end;

procedure MyRunner.Close();
begin
    asyncClient.Close;
end;

procedure MyRunner.attachLogger( logger : IMyRunnerLogger );
begin
    if loggers.IndexOf(logger) = -1 then
        loggers.Add(IUnknown(logger));
end;

procedure MyRunner.detachLogger( logger : IMyRunnerLogger );
begin
    loggers.Remove(logger);
end;

// *****************************************************************************
//* Observer
// *****************************************************************************
procedure MyRunner.notify( operation: TMyRunnerOperation );
var
    lines : TStrings;
    line : AnsiString;
    i : integer;
    logger : IUnknown;
begin 
    lines := TStringList.Create;

    if operation = TMyRunnerOperation.stderr then
    begin
        asyncClient.Errors(lines);

        For i := 0 to lines.Count - 1 do
        begin
            line := lines[i];
            writeln('stderr   ' + line);
        end;
    end
    else
    begin
        asyncClient.Read(lines);

        for i := 0 to loggers.Count - 1 do
        begin
            logger := loggers[i];
            with logger as IMyRunnerLogger do
                log( lines );
        end;
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


