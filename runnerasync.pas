unit RunnerAsync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, fpjson, jsonparser, typinfo, StreamReader, RunnerInterfaces, ResponseItem, gmap, gutil, Semaphores;


type
    CompareStrings = specialize TLess<AnsiString>;
    ResponseTreeMap = specialize TMap<AnsiString, MyResponseItem, CompareStrings>;

type
    MyRunnerAsync = class(TInterfacedObject, IMyRunnerObserved)
    private
        proc: TProcess;
        outputReader : MyReader;
        errorReader : MyReader;
        observers: TInterfaceList;
        response : ResponseTreeMap;

        linebuffer: TStrings;
        semaphore: TSemaphore;

        procedure WriteLn(line: AnsiString);
        function makeToken() : string;
        procedure log( line: AnsiString );
        procedure notifyObservers();

    public
        constructor Create;
        procedure Read(var lines: TStrings);
        procedure Errors(var lines: TStrings);
        procedure attachObserver( observer : IMyRunnerObserver );
        procedure detachObserver( observer : IMyRunnerObserver );
        
        procedure ReadLog(var buffer: TStrings);
        function WaitForResponse( token : string; var message : AnsiString; var jObject : TJSONObject ) : integer;

        procedure postResponseItem( line: AnsiString );

        function CreateArray(field : AnsiString) : string;
        function ExtendArray( field : AnsiString; list : array of real ) : string;
        function RunPythonFunction( pythonFunction : AnsiString) : string;
        function GetResult() : string;
        function Close() : string;

        function HandleResponseGetResult(jObject : TJSONObject; var count : integer; var total : real; var ErrorMessage : AnsiString ) : integer;
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
    response := ResponseTreeMap.create;
    linebuffer := TStringList.Create;
    semaphore := TSemaphore.Create(1);

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

function MyRunnerAsync.makeToken() : string;
var
    GUID: TGuid;
begin
    CreateGUID( GUID );
    makeToken := GUIDToString( GUID );
end;


function MyRunnerAsync.WaitForResponse( token : string; var message : AnsiString; var jObject : TJSONObject ) : integer;
var
    line: AnsiString;
    responseItem : MyResponseItem;
    jData : TJSONData;
    jtype : TJSONtype;
    jTypeString : string;
    jStatus : TJSONString;
    jMessage : TJSONString;
    status : string;
    code : integer;

begin
    responseItem := response[ token ];

    log( 'MyRunnerAsync.WaitForResponse: waiting for: ' + token );
    responseItem.semaphore.Wait();

    log( 'MyRunnerAsync.WaitForResponse: continuing: ' + token );
    response.delete( token );
    line := responseItem.line;
    responseItem.Destroy();

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

    WaitForResponse := code;
end;






procedure MyRunnerAsync.postResponseItem( line: AnsiString );
var
    jData : TJSONData;
    jtype : TJSONtype;
    jObject : TJSONObject;
    jToken : TJSONString;
    token: string;
    responseItem : MyResponseItem;
    jTypeString : string;

begin
    try
    begin
        log( 'MyRunnerAsync.postResponseItem: ' + line);

        jData := GetJSON( line );
        jtype := jData.JSONType();

        if  jtype = TJSONType.jtObject then
        begin
            jObject := jData as TJSONObject;

            jData := jObject.Find('token');
            if jData = Nil then
                log( 'MyRunnerAsync.postResponseItem: The "token" field is missing' )
            else
            begin
                jtype := jData.JSONType();
                if  jtype = TJSONType.jtString then
                begin
                    jToken := jData as TJSONString;
                    token := jToken.AsString;
                end
                else
                begin
                    jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                    log( 'MyRunnerAsync.postResponseItem: Error: unexpected token type. jType = ' + jTypeString );
                end;
            end;

            responseItem := response[ token ];
            responseItem.line := line;
            responseItem.semaphore.Post();
        end
        else
        begin
            log( 'MyRunnerAsync.postResponseItem: Error: unexpected response. jType = ' + jTypeString );
        end;
    end;

    Except
        on E: Exception do
        begin
            log( 'MyRunnerAsync.postResponseItem: Error: '+ E.ClassName + ': ' + E.Message );
        end;
    end;
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
// * Logging
// *****************************************************************************

procedure MyRunnerAsync.log( line: AnsiString );
begin
    semaphore.Wait();
    linebuffer.Add(line);
    semaphore.Post();

    notifyObservers();
end;

procedure MyRunnerAsync.ReadLog(var buffer: TStrings);
var
    temp: TStrings;
begin
    semaphore.Wait();
    temp := linebuffer;
    linebuffer := TStringList.Create;
    semaphore.Post();

    buffer := temp;
end;

procedure MyRunnerAsync.notifyObservers();
var
   observer: IUnknown;
   i: Integer;
begin
   for i := 0 to observers.Count - 1 do
   begin
       observer := observers[i];
       with observer as IMyRunnerObserver do
           notify( TMyRunnerOperation.logger );
   end;
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
    // data["array"] = []

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
    response[ token ] := MyResponseItem.Create();

    CreateArray := token
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
    response[ token ] := MyResponseItem.Create();

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
    response[ token ] := MyResponseItem.Create();

    RunPythonFunction := token
end;


function MyRunnerAsync.GetResult() : string;
var
    command : AnsiString;
    jObject : TJSONObject;
    jArray : TJSONArray; 
    token : string;

begin
    jObject := TJSONObject.Create();
    jObject.Add('command', 'get');

    jArray := TJSONArray.Create();
    jArray.Add( 'result' );
    jObject.Add('arguments', jArray);

    token := makeToken();
    jObject.Add('token', token);
    command := jObject.AsJSON;
    WriteLn(command);
    response[ token ] := MyResponseItem.Create();

    GetResult := token;
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
    response[ token ] := MyResponseItem.Create();

    Sleep(100);

    if not outputReader.Finished then
        outputReader.Terminate();

    if not errorReader.Finished then
        errorReader.Terminate();

    proc.Terminate(0);

    Close := token;
end;


// *****************************************************************************
// * HandleResponse helpers
// *****************************************************************************

function MyRunnerAsync.HandleResponseGetResult(jObject : TJSONObject; var count : integer; var total : real; var ErrorMessage : AnsiString ) : integer;
var
    token : string;
    code : integer;
    jData : TJSONData;
    jResult : TJSONObject;
    jtype : TJSONtype;
    jTypeString : string;
    jCount : TJSONIntegerNumber;
    jTotal : TJSONFloatNumber;

begin
    log('MyRunnerAsync.GetResultResponse: entry');

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

    log('MyRunnerAsync.GetResultResponse: exit(' + IntToStr(code) + ')');
    HandleResponseGetResult := code;
end;



end.


