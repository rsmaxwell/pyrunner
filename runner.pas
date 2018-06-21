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
        procedure attachLogger( logger : IMyRunnerLogger );
        procedure detachLogger( logger : IMyRunnerLogger );

        procedure CreateArray(field : AnsiString);
        procedure ExtendArray( field : AnsiString; list : array of real );
        procedure RunPythonFunction( pythonFunction : AnsiString);
        procedure GetField(field : AnsiString);
        procedure Close();
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
var
    token : string;
begin
    token := asyncClient.Close;
    writeln('MyRunner.Close: token = ' + token);
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
var
    token : string;
begin
    token := asyncClient.CreateArray( field );
    writeln('MyRunner.CreateArray: token = ' + token);
end;

procedure MyRunner.ExtendArray( field : AnsiString; list : array of real ); 
var
    token : string;
begin
    token := asyncClient.ExtendArray( field, list );
    writeln('MyRunner.ExtendArray: token = ' + token);
end;


procedure MyRunner.RunPythonFunction( pythonFunction : AnsiString);
var
    token : string;
begin
    token := asyncClient.RunPythonFunction( pythonFunction );
    writeln('MyRunner.RunPythonFunction: token = ' + token);
end;


procedure MyRunner.GetField( field : AnsiString ); 
var
    token : string;
begin
    token := asyncClient.GetField( field ); 
    writeln('MyRunner.GetField: token = ' + token);
end;

end.


