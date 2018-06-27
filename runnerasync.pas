unit RunnerAsync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, fpjson, jsonparser, typinfo, StreamReader,
  RunnerInterfaces, ResponseItem, gmap, gutil, Semaphores, RunnerException;

const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

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
        function WaitForResponse( token : string ) : TJSONObject;

        procedure postResponseItem( line: AnsiString );

        function CreateArray(field : string) : string;
        function ExtendArray( field : string; list : array of real ) : string;
        function RunPythonFunction( pythonFunction : string) : string;
        function GetResult() : string;
        function Close() : string;

        procedure HandleResponseGetResult(jObject : TJSONObject; var count : integer; var total : real);
        procedure HandleResponseClose();
    end;




implementation

const
    CarriageReturn: Byte = 13;
    LineFeed: Byte = 10;


// *****************************************************************************
// * Basic
// *****************************************************************************

constructor MyRunnerAsync.Create();
var                       
    pythonProgramName : string;
    launcherProgramName : string;
    programName : string;
    programPath : string;

begin
    // *************************************************************************
    // * Find the python program path
    // *************************************************************************
    pythonProgramName := 'pythonw.exe';
    launcherProgramName := 'pyw.exe';

    programName := pythonProgramName;
    programPath := FindDefaultExecutablePath(programName);
    if length(programPath) = 0 then
    begin
        programName := launcherProgramName;
        programPath := FindDefaultExecutablePath(programName);
        if length(programPath) = 0 then
            raise MyRunnerException.Create('Could not find ' + pythonProgramName + ' or ' + launcherProgramName + ' on the PATH');
    end;

    // *************************************************************************
    // * Launch the python server
    // *************************************************************************
    observers := TInterfaceList.Create;
    response := ResponseTreeMap.create;
    linebuffer := TStringList.Create;
    semaphore := TSemaphore.Create(1);

    proc := TProcess.Create(nil);
    proc.Executable:= programPath;
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


function MyRunnerAsync.WaitForResponse( token : string ) : TJSONObject;
var
    line: AnsiString;
    responseItem : MyResponseItem;
    jObject : TJSONObject;
    jData : TJSONData;
    jtype : TJSONtype;
    jTypeString : string;
    jStatus : TJSONString;
    jMessage : TJSONString;
    status : string;
    message : AnsiString;

begin
    responseItem := response[ token ];

    log( 'MyRunnerAsync.WaitForResponse: waiting for: ' + token );
    responseItem.semaphore.Wait();

    log( 'MyRunnerAsync.WaitForResponse: continuing: ' + token );
    response.delete( token );
    line := responseItem.line;
    responseItem.Destroy();

    jData := GetJSON( line );
    jtype := jData.JSONType();

    if jtype <> TJSONType.jtObject then
    begin
        jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
        raise MyRunnerException.Create('Error: unexpected response. jType = ' + jTypeString);
    end;

    jObject := jData as TJSONObject;
    jData := jObject.Find('status');
    if jData = Nil then
        raise MyRunnerException.Create('The "status" field is missing');


    jtype := jData.JSONType();
    if jtype <> TJSONType.jtString then
    begin
        jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
        raise MyRunnerException.Create('Error: unexpected status type. jType = ' + jTypeString);
    end;

    jStatus := jData as TJSONString;
    status := jStatus.AsString;

    jData := jObject.Find('message');
    if jData <> Nil then
    begin
        jtype := jData.JSONType();
        if  jtype <> TJSONType.jtString then
        begin
            jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
            raise MyRunnerException.Create('Error: unexpected message type. jType = ' + jTypeString);
        end;

        jMessage := jData as TJSONString;
        message := jMessage.AsString;
    end;

    if status <> 'ok' then
        if length(message) > 0 then
            raise MyRunnerException.Create( status + ': ' + message )
        else
            raise MyRunnerException.Create( status + ': unexpected error' );

    WaitForResponse := jObject;
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
function MyRunnerAsync.CreateArray( field : string ) : string;
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

function MyRunnerAsync.ExtendArray( field : string; list : array of real ) : string;
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


function MyRunnerAsync.RunPythonFunction( pythonFunction : string) : string;
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

    Close := token;
end;


// *****************************************************************************
// * HandleResponse helpers
// *****************************************************************************

procedure MyRunnerAsync.HandleResponseGetResult(jObject : TJSONObject; var count : integer; var total : real );
var
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
        raise MyRunnerException.Create('The "result" field is missing')
    else
    begin
        jtype := jData.JSONType();
        if  jtype = TJSONType.jtObject then
            jResult := jData as TJSONObject
        else
        begin
            jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
            raise MyRunnerException.Create('Error: unexpected result type. jType = ' + jTypeString);
        end;

        jData := jResult.Find('count');
        if jData = Nil then
            raise MyRunnerException.Create('Error: The "result.count" field is missing')
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
                jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                raise MyRunnerException.Create('Error: Unexpected result.count type: jType = ' + jTypeString);
            end;
        end;

        jData := jResult.Find('total');
        if jData = Nil then
            raise MyRunnerException.Create('Error: The "result.total" field is missing')
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
                jTypeString := GetEnumName(TypeInfo(TJSONtype), Ord(jtype));
                raise MyRunnerException.Create('Error: Unexpected result.total type: jType = ' + jTypeString);
            end;
        end;
    end;

    log('MyRunnerAsync.GetResultResponse: exit');
end;




procedure MyRunnerAsync.HandleResponseClose(); begin
    if not outputReader.Finished then
        outputReader.Terminate();

    if not errorReader.Finished then
        errorReader.Terminate();

    proc.Terminate(0);

    observers.Free;
    response.Free;
    linebuffer.Free;
    semaphore.Free;
end;


end.


