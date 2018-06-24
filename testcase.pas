unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Math, DateUtils, Runner, RunnerLogger;

type
    MyTestCase = class(TInterfacedObject, IMyRunnerLogger)
    private
        client : MyRunner;
        procedure log( lines: TStrings );
    public
        procedure simpleTest();
        procedure standardTest();
        procedure asyncTest();
        procedure performanceTest();
    end;

implementation 

// *****************************************************************************
// * Simple Test.     !!! No error checking !!!
// *****************************************************************************
procedure MyTestCase.simpleTest();
var
    list : array of real;
    count : integer;
    total : real;
    ErrorMessage : AnsiString;

begin
    client := MyRunner.Create();
    // client.AttachLogger( self );

    client.CreateArray('array', ErrorMessage);

    SetLength(list, 2);
    list[0] := 123.456;
    list[1] := 456.789;
    client.ExtendArray('array', list, ErrorMessage);

    client.RunPythonFunction( 'foobar', ErrorMessage );

    client.GetResult('result', count, total, ErrorMessage);
    writeln( 'Results:' );
    writeln('     count = ' + IntToStr(count) );
    writeln('     total = ' + FloatToStr(total) );

    client.Close( ErrorMessage );
end;

// *****************************************************************************
// * Standard Test.   With error checking
// *****************************************************************************
procedure MyTestCase.standardTest();
var
    a : integer;
    b : integer;
    list : array of real;
    iterations : integer;
    size : integer;
    count : integer;
    total : real;
    rc : integer;
    rc2 : integer;
    ErrorMessage : AnsiString;
    functionName : string;

begin
    rc := 0;

    writeln('Startup' );
    client := MyRunner.Create();
    // client.AttachLogger( self );

    if rc = 0 then
    begin
        writeln('Create array' );
        rc := client.CreateArray('array', ErrorMessage);
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        iterations := 2;
        for a := 0 to iterations - 1 do
        begin
            size := 1000;
            SetLength(list, size);

            for b := 0  to size - 1 do
                list[b] := random();

            if rc = 0 then
            begin
                writeln('(' + IntToStr(a) + '):  Add ' + IntToStr(size) + ' items to array' );
                rc := client.ExtendArray('array', list, ErrorMessage);
                if rc <> 0 then
                begin
                    writeln( ErrorMessage );
                    Break;
                end;
            end;
        end;
    end;

    if rc = 0 then
    begin
        functionName := 'foobar';
        writeln('Run python function: ' + functionName);
        rc := client.RunPythonFunction( functionName, ErrorMessage );
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        writeln('Get result' );
        rc := client.GetResult('result', count, total, ErrorMessage);
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
       writeln( 'Results:' );
       writeln('     count = ' + IntToStr(count) );
       writeln('     total = ' + FloatToStr(total) );
    end;

    writeln('Close' );
    rc2 := client.Close( ErrorMessage );
    if rc2 <> 0 then
    begin
        rc := Max( rc, rc2);
        writeln( ErrorMessage );
    end;
end;


// *****************************************************************************
// * Asynchronous API Test.
// *****************************************************************************
procedure MyTestCase.asyncTest();
var
    a : integer;
    b : integer;
    list : array of real;
    iterations : integer;
    size : integer;
    count : integer;
    total : real;
    rc : integer;
    rc2 : integer;
    ErrorMessage : AnsiString;
    functionName : string;

    token : string;
    jObject : TJSONObject;

begin
    rc := 0;

    writeln('Startup' );
    client := MyRunner.Create();
    client.AttachLogger( self );

    if rc = 0 then
    begin
        writeln('Create array' );
        rc := client.CreateArray('array', ErrorMessage);
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        iterations := 2;
        for a := 0 to iterations - 1 do
        begin
            size := 1000;
            SetLength(list, size);

            for b := 0  to size - 1 do
                list[b] := random();

            if rc = 0 then
            begin
                writeln('(' + IntToStr(a) + '):  Add ' + IntToStr(size) + ' items to array' );
                rc := client.ExtendArray('array', list, ErrorMessage);
                if rc <> 0 then
                begin
                    writeln( ErrorMessage );
                    Break;
                end;
            end;
        end;
    end;

    if rc = 0 then
    begin
        functionName := 'foobar';
        writeln('Run python function: ' + functionName);
        token := client.asyncClient.RunPythonFunction( functionName );
        writeln('after MyAsyncRunner.RunPythonFunction: token: ' + token);

        // Do other stuff here ...

        // But make sure that eventually "WaitForResponse" is called, to clear the entry
        // in the "ResponseMap" ... (otherwise there will be a leak!)
        // "WaitForResponse" can be called on a different thread

        writeln('before MyAsyncRunner.WaitForResponse: token: ' + token);
        rc := client.asyncClient.WaitForResponse( token, ErrorMessage, jObject );
        writeln('after MyAsyncRunner.WaitForResponse: code: ' + IntToStr(rc));

        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        writeln('Get result' );
        rc := client.GetResult('result', count, total, ErrorMessage);
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
       writeln( 'Results:' );
       writeln('     count = ' + IntToStr(count) );
       writeln('     total = ' + FloatToStr(total) );
    end;

    writeln('Close' );
    rc2 := client.Close( ErrorMessage );
    if rc2 <> 0 then
    begin
        rc := Max( rc, rc2);
        writeln( ErrorMessage );
    end;
end;





// *****************************************************************************
// * Performance Test.   With error checking AND performance monitoring
// *****************************************************************************
procedure MyTestCase.performanceTest();
var
    a : integer;
    b : integer;
    list : array of real;
    iterations : integer;
    size : integer;
    count : integer;
    total : real;
    rc : integer;
    rc2 : integer;
    ErrorMessage : AnsiString;
    functionName : string;
    starttime : TDateTime;

begin
    rc := 0;

    writeln('Startup' );
    starttime := Now;
    client := MyRunner.Create();
    Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
    // client.AttachLogger( self );

    if rc = 0 then
    begin
        writeln('Create array' );
        starttime := Now;
        rc := client.CreateArray('array', ErrorMessage);
        Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        iterations := 1000;
        for a := 0 to iterations - 1 do
        begin
            size := 1000;
            SetLength(list, size);

            for b := 0  to size - 1 do
                list[b] := random();

            if rc = 0 then
            begin
                writeln('(' + IntToStr(a) + '):  Add ' + IntToStr(size) + ' items to array' );
                starttime := Now;
                rc := client.ExtendArray('array', list, ErrorMessage);
                Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
                if rc <> 0 then
                begin
                    writeln( ErrorMessage );
                    Break;
                end;
            end;
        end;
    end;

    if rc = 0 then
    begin
        functionName := 'foobar';
        writeln('Run python function: ' + functionName);
        starttime := Now;
        rc := client.RunPythonFunction( functionName, ErrorMessage );
        Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        writeln('Get result' );
        starttime := Now;
        rc := client.GetResult('result', count, total, ErrorMessage);
        Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
       writeln( 'Results:' );
       writeln('     count = ' + IntToStr(count) );
       writeln('     total = ' + FloatToStr(total) );
    end;

    writeln('Close' );
    starttime := Now;
    rc2 := client.Close( ErrorMessage );
    Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
    if rc2 <> 0 then
    begin
        rc := Max( rc, rc2);
        writeln( ErrorMessage );
    end;
end;


// *****************************************************************************
// * Logger
// *****************************************************************************
procedure MyTestCase.log( lines: TStrings );
var
    line : AnsiString;
    i : integer;
begin
    For i := 0 to lines.Count - 1 do
    begin
        line := lines[i];
        writeln( line );
    end;
end;


end.

