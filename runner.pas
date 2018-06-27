unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, jsonparser, typinfo, RunnerAsync, RunnerInterfaces, RunnerLogger;

type
    MyRunner = class(TInterfacedObject, IMyRunnerObserver)
    private
        loggers: TInterfaceList;
        procedure notify( operation: TMyRunnerOperation );
        procedure log( lines : TStrings );
        procedure log( line : AnsiString );

    public 
        asyncClient: MyRunnerAsync;

        constructor Create;
        procedure AttachLogger( logger : IMyRunnerLogger );
        procedure DetachLogger( logger : IMyRunnerLogger );

        procedure CreateArray( field : string);
        procedure ExtendArray( field : string; list : array of real );
        procedure RunPythonFunction( pythonFunction : string );
        procedure GetResult(var count : integer; var total : real );
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

procedure MyRunner.AttachLogger( logger : IMyRunnerLogger );
begin
    if loggers.IndexOf(logger) = -1 then
        loggers.Add(IUnknown(logger));
end;

procedure MyRunner.DetachLogger( logger : IMyRunnerLogger );
begin
    loggers.Remove(logger);
end;

procedure MyRunner.log( lines : TStrings );
var
    i : integer;
    logger : IUnknown;
begin
    for i := 0 to loggers.Count - 1 do
    begin
        logger := loggers[i];
        with logger as IMyRunnerLogger do
            log( lines );
    end;
end;

procedure MyRunner.log( line : AnsiString );
var
    lines : TStrings;
begin
    lines := TStringList.Create;
    lines.Add( line );
    log( lines );
end;


// *****************************************************************************
//* Observer
// *****************************************************************************
procedure MyRunner.notify( operation: TMyRunnerOperation );
var
    lines : TStrings;
    line : AnsiString;
    i : integer;

begin 
    lines := TStringList.Create;

    if operation = TMyRunnerOperation.stderr then
    begin
        asyncClient.Errors(lines);
        For i := 0 to lines.Count - 1 do
        begin
            line := lines[i];
            asyncClient.postResponseItem( line );
        end;
    end
    else if operation = TMyRunnerOperation.stdout then
    begin
        asyncClient.Read(lines);
        For i := 0 to lines.Count - 1 do
        begin
            line := lines[i];
            log( 'python: ' + line);
        end;
    end
    else // logger
    begin
        asyncClient.ReadLog(lines);
        For i := 0 to lines.Count - 1 do
        begin
            line := lines[i];
            log(line);
        end;
    end;
end;


// *****************************************************************************
//* Helpers
// *****************************************************************************
procedure MyRunner.CreateArray( field : string );
var
    token : string;

begin
    token := asyncClient.CreateArray( field );
    log('MyRunner.CreateArray: token: ' + token);

    asyncClient.WaitForResponse( token );
    log('MyRunner.CreateArray: exit');
end;


procedure MyRunner.ExtendArray( field : string; list : array of real );
var
    token : string;
begin
    token := asyncClient.ExtendArray( field, list );
    log('MyRunner.ExtendArray: token: ' + token);

    asyncClient.WaitForResponse( token );
    log('MyRunner.ExtendArray: exit');
end;


procedure MyRunner.RunPythonFunction( pythonFunction : string );
var
    token : string;
begin
    token := asyncClient.RunPythonFunction( pythonFunction );
    log('MyRunner.RunPythonFunction: token: ' + token);

    asyncClient.WaitForResponse( token );
    log('MyRunner.RunPythonFunction: exit' );
end;


procedure MyRunner.GetResult(var count : integer; var total : real );
var
    token : string;
    jObject : TJSONObject;

begin
    token := asyncClient.GetResult();
    log('MyRunner.GetResult: token: ' + token);

    jObject := asyncClient.WaitForResponse( token );
    asyncClient.HandleResponseGetResult(jObject, count, total);
    log('MyRunner.GetResult: exit');
end;




procedure MyRunner.Close();
var
    token : string;

begin
    token := asyncClient.Close;
    log('MyRunner.Close: token: ' + token);

    asyncClient.WaitForResponse( token );
    log('MyRunner.Close: exit');
end;

end.


