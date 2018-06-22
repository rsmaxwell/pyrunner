unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fpjson, jsonparser, typinfo, RunnerAsync, RunnerInterfaces, RunnerLogger, ResponseItem;

type
    MyRunner = class(TInterfacedObject, IMyRunnerObserver)
    private
        asyncClient: MyRunnerAsync; 
        loggers: TInterfaceList;
        procedure notify( operation: TMyRunnerOperation );
        function handleResponse( line: AnsiString; var message : AnsiString; jObject : TJSONObject ) : integer;

    public
        constructor Create;
        procedure attachLogger( logger : IMyRunnerLogger );
        procedure detachLogger( logger : IMyRunnerLogger );

        function CreateArray( field : AnsiString; var ErrorMessage : AnsiString ) : integer;
        function ExtendArray( field : AnsiString; list : array of real; var ErrorMessage : AnsiString ) : integer;
        function RunPythonFunction( pythonFunction : AnsiString; var ErrorMessage : AnsiString ) : integer;
        function GetField(field : AnsiString; var ErrorMessage : AnsiString ) : integer;
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
            asyncClient.postResponseItem( line );
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

function MyRunner.handleResponse( line: AnsiString; var message : AnsiString; jObject : TJSONObject ) : integer;
var
    jData : TJSONData;
    jtype : TJSONtype;
    jTypeString : string;
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
                    status := jObject.Get('status');

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
                    message := jObject.Get('status')
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
    writeln('MyRunner.CreateArray: token = ' + token);

    line := asyncClient.WaitForResponse( token );
    CreateArray := handleResponse( line, ErrorMessage, jObject );
end;


function MyRunner.ExtendArray( field : AnsiString; list : array of real; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.ExtendArray( field, list );
    writeln('MyRunner.ExtendArray: token = ' + token);

    line := asyncClient.WaitForResponse( token );
    ExtendArray := handleResponse( line, ErrorMessage, jObject );
end;


function MyRunner.RunPythonFunction( pythonFunction : AnsiString; var ErrorMessage : AnsiString ) : integer;
var
    token : string; 
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.RunPythonFunction( pythonFunction );
    writeln('MyRunner.RunPythonFunction: token = ' + token);

    line := asyncClient.WaitForResponse( token );  
    RunPythonFunction := handleResponse( line, ErrorMessage, jObject );
end;


function MyRunner.GetField( field : AnsiString; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.GetField( field ); 
    writeln('MyRunner.GetField: token = ' + token);

    line := asyncClient.WaitForResponse( token ); 
    GetField := handleResponse( line, ErrorMessage, jObject );
end;

function MyRunner.Close( var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    line : AnsiString;
    jObject : TJSONObject;
    code : integer;
begin
    token := asyncClient.Close;
    writeln('MyRunner.Close: token = ' + token); 

    line := asyncClient.WaitForResponse( token );
    Close := handleResponse( line, ErrorMessage, jObject );
end;

end.


