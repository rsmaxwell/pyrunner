unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, jsonparser, typinfo, RunnerAsync, RunnerInterfaces, RunnerLogger;

type
    MyRunner = class(TInterfacedObject, IMyRunnerObserver)
    private
        asyncClient: MyRunnerAsync; 
        loggers: TInterfaceList;
        procedure notify( operation: TMyRunnerOperation );
        function handleResponse( line: AnsiString; var message : AnsiString; var jObject : TJSONObject ) : integer;
        procedure log( lines : TStrings );
        procedure log( line : AnsiString );

    public
        constructor Create;
        procedure attachLogger( logger : IMyRunnerLogger );
        procedure detachLogger( logger : IMyRunnerLogger );

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

procedure MyRunner.attachLogger( logger : IMyRunnerLogger );
begin
    if loggers.IndexOf(logger) = -1 then
        loggers.Add(IUnknown(logger));
end;

procedure MyRunner.detachLogger( logger : IMyRunnerLogger );
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

function MyRunner.handleResponse( line: AnsiString; var message : AnsiString; var jObject : TJSONObject ) : integer;
var
    jData : TJSONData;
    jtype : TJSONtype;
    jTypeString : string;
    jStatus : TJSONString;
    jMessage : TJSONString;
    status : string;
    code : integer;

begin
    try
    begin
        jData := GetJSON( line );
        jtype := jData.JSONType();

        if  jtype = TJSONType.jtObject then
        begin
            jObject := jData as TJSONObject;

            jData := jObject.Find('status');
            if jData = Nil then
            begin
                code := -1;
                message := ' The "status" field is missing';
            end
            else
            begin
                jtype := jData.JSONType();
                if  jtype = TJSONType.jtString then
                begin
                    jStatus := jData as TJSONString;
                    status := jStatus.AsString;

                    if (status = 'ok') then
                        code := 0
                    else if (status = 'error') then
                        code := 1
                    else
                        code := 2
                end
                else
                begin
                    code := 3;
                    jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                    message := 'Error: unexpected status type. jType = ' + jTypeString;
                end;
            end;

            jData := jObject.Find('message');
            if jData <> Nil then
            begin
                jtype := jData.JSONType();
                if  jtype = TJSONType.jtString then
                begin
                    jMessage := jData as TJSONString;
                    message := jMessage.AsString;
                end
                else
                begin
                    code := 3;
                    jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                    message := 'Error: unexpected message type. jType = ' + jTypeString;
                end;
             end;


        end
        else
        begin
            code := 3;
            message := 'Error: unexpected response. jType = ' + jTypeString;
        end;
    end;

    Except
        on E: Exception do
        begin
            code := 4;
            message := 'Error: '+ E.ClassName + ': ' + E.Message;
        end;
    end;

    handleResponse := code;
end;



// *****************************************************************************
//* Helpers
// *****************************************************************************
function MyRunner.CreateArray( field : AnsiString; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject; 
    code : integer;
begin
    token := asyncClient.CreateArray( field );
    log('MyRunner.CreateArray: entry: ' + token);

    line := asyncClient.WaitForResponse( token );
    code := handleResponse( line, ErrorMessage, jObject );
    log('MyRunner.ExtendArray: exit(' + IntToStr(code) + '): ' + token);

    CreateArray := code;
end;


function MyRunner.ExtendArray( field : AnsiString; list : array of real; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.ExtendArray( field, list );
    log('MyRunner.ExtendArray: entry: ' + token);

    line := asyncClient.WaitForResponse( token );
    code := handleResponse( line, ErrorMessage, jObject );
    log('MyRunner.ExtendArray: exit(' + IntToStr(code) + '): ' + token);

    ExtendArray := code;
end;


function MyRunner.RunPythonFunction( pythonFunction : AnsiString; var ErrorMessage : AnsiString ) : integer;
var
    token : string; 
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.RunPythonFunction( pythonFunction );
    log('MyRunner.RunPythonFunction: entry: ' + token);

    line := asyncClient.WaitForResponse( token );
    code := handleResponse( line, ErrorMessage, jObject );
    log('MyRunner.RunPythonFunction: exit(' + IntToStr(code) + '): ' + token);

    RunPythonFunction := code;
end;


function MyRunner.GetResult(field : AnsiString; var count : integer; var total : real; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
    jData : TJSONData;
    jResult : TJSONObject;
    jtype : TJSONtype;
    jTypeString : string;
    jCount : TJSONIntegerNumber;
    jTotal : TJSONFloatNumber;

begin
    token := asyncClient.GetField( field );
    log('MyRunner.GetResult: entry: ' + token);

    line := asyncClient.WaitForResponse( token );
    code := handleResponse( line, ErrorMessage, jObject );

    if code = 0 then
    begin
        jData := jObject.Find('result');
        if jData = Nil then
        begin
            code := 1;
            ErrorMessage := 'The "result" field is missing';
        end
        else
        begin
            jtype := jData.JSONType();
            if  jtype = TJSONType.jtObject then
                jResult := jData as TJSONObject
            else
            begin
                code := 3;
                jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                ErrorMessage := 'Error: unexpected result type. jType = ' + jTypeString;
            end;
        end;
    end;

    if code = 0 then
    begin
        jData := jResult.Find('count');
        if jData = Nil then
        begin
            code := 1;
            ErrorMessage := 'Error: The "result.count" field is missing';
        end
        else
        begin
            jtype := jData.JSONType();
            if  jtype = TJSONType.jtNumber then
            begin
                jCount := jData as TJSONIntegerNumber;
                count := jCount.AsInteger;
            end
            else
            begin
                code := 3;
                jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                ErrorMessage := 'Error: Unexpected result.count type: jType = ' + jTypeString;
            end;
        end;
    end;

    if code = 0 then
    begin
        jData := jResult.Find('total');
        if jData = Nil then
        begin
            code := 1;
            ErrorMessage := 'Error: The "result.total" field is missing';
        end
        else
        begin
            jtype := jData.JSONType();
            if  jtype = TJSONType.jtNumber then
            begin
                jTotal := jData as TJSONFloatNumber;
                total := jTotal.AsFloat;
            end
            else
            begin
                code := 3;
                jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                ErrorMessage := 'Error: Unexpected result.total type: jType = ' + jTypeString;
            end;
        end;
    end;

    log('MyRunner.GetResult: exit(' + IntToStr(code) + '): ' + token);
    GetResult := code;
end;

function MyRunner.Close( var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.Close;
    log('MyRunner.Close: entry: ' + token);

    line := asyncClient.WaitForResponse( token );
    code := handleResponse( line, ErrorMessage, jObject );

    log('MyRunner.Close: exit(' + IntToStr(code) + '): ' + token);
    Close := code;
end;

end.


