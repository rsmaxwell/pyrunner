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

        function CreateArray( field : AnsiString; var ErrorMessage : AnsiString ) : integer;
        function ExtendArray( field : AnsiString; list : array of real; var ErrorMessage : AnsiString ) : integer;
        function RunPythonFunction( pythonFunction : AnsiString; var ErrorMessage : AnsiString ) : integer;
        function GetResult(field : AnsiString; var count : integer; var total : real; var ErrorMessage : AnsiString ) : integer;
        function Close(var ErrorMessage : AnsiString ) : integer;
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
function MyRunner.CreateArray( field : AnsiString; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    jObject : TJSONObject; 
    code : integer;
begin
    token := asyncClient.CreateArray( field );
    log('MyRunner.CreateArray: entry: ' + token);

    code := asyncClient.WaitForResponse( token, ErrorMessage, jObject );
    log('MyRunner.CreateArray: exit(' + IntToStr(code) + '): ' + token);

    CreateArray := code;
end;


function MyRunner.ExtendArray( field : AnsiString; list : array of real; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.ExtendArray( field, list );
    log('MyRunner.ExtendArray: entry: ' + token);

    code := asyncClient.WaitForResponse( token, ErrorMessage, jObject );
    log('MyRunner.ExtendArray: exit(' + IntToStr(code) + '): ' + token);

    ExtendArray := code;
end;


function MyRunner.RunPythonFunction( pythonFunction : AnsiString; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.RunPythonFunction( pythonFunction );
    log('MyRunner.RunPythonFunction: entry: ' + token);

    code := asyncClient.WaitForResponse( token, ErrorMessage, jObject );
    log('MyRunner.RunPythonFunction: exit(' + IntToStr(code) + '): ' + token);

    RunPythonFunction := code;
end;


function MyRunner.GetResult(field : AnsiString; var count : integer; var total : real; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    jObject : TJSONObject;
    code : integer;

begin
    token := asyncClient.GetField( field );
    log('MyRunner.GetResult: entry: ' + token);

    code := asyncClient.WaitForResponse( token, ErrorMessage, jObject );

    if code = 0 then
        code := asyncClient.HandleResponseGetResult(jObject, count, total, ErrorMessage );


    log('MyRunner.GetResult: exit(' + IntToStr(code) + ')');
    GetResult := code;
end;




function MyRunner.Close( var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.Close;
    log('MyRunner.Close: entry: ' + token);

    code := asyncClient.WaitForResponse( token, ErrorMessage, jObject );

    log('MyRunner.Close: exit(' + IntToStr(code) + '): ' + token);
    Close := code;
end;

end.


