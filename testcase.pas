unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, DateUtils, Runner, RunnerLogger, RunnerException;

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
// * Simple Test     !!! No error handling !!!
// *****************************************************************************
procedure MyTestCase.simpleTest();
var
    list : array of real;
    count : integer;
    total : real;

begin
    client := MyRunner.Create();
    // client.AttachLogger( self );

    client.CreateArray('array');

    SetLength(list, 2);
    list[0] := 123.456;
    list[1] := 456.789;
    client.ExtendArray('array', list);

    client.RunPythonFunction('foobar');

    client.GetResult(count, total);
    writeln('Result:' );
    writeln('     count = ' + IntToStr(count) );
    writeln('     total = ' + FloatToStr(total) );

    client.Close();
end;

// *****************************************************************************
// * Standard Test
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
    functionName : string;

begin
    try
        writeln('Startup' );
        client := MyRunner.Create();
        client.AttachLogger( self );

        try
            writeln('Create array' );
            client.CreateArray('array');

            iterations := 2;
            for a := 0 to iterations - 1 do
            begin
                size := 1000;
                SetLength(list, size);

                for b := 0  to size - 1 do
                    list[b] := random();

                writeln('(' + IntToStr(a) + '):  Add ' + IntToStr(size) + ' items to array' );
                client.ExtendArray('array', list);
            end;

            functionName := 'foobar';
            writeln('Run python function: ' + functionName);
            client.RunPythonFunction( functionName );

            writeln('Get result' );
            client.GetResult(count, total);

            writeln( 'Results:' );
            writeln('     count = ' + IntToStr(count) );
            writeln('     total = ' + FloatToStr(total) );

        finally;
            writeln('Close' );
            client.Close();
        end;
    except
        on E: MyRunnerException do writeln( E.Message );
    end;
end;

// *****************************************************************************
// * Asynchronous API Test
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
    functionName : string;

    token : string;
    jObject : TJSONObject;

begin
    try
        writeln('Startup' );
        client := MyRunner.Create();
        client.AttachLogger( self );

        try
            writeln('Create array' );
            client.CreateArray('array');

            iterations := 2;
            for a := 0 to iterations - 1 do
            begin
                size := 1000;
                SetLength(list, size);

                for b := 0  to size - 1 do
                    list[b] := random();

                writeln('(' + IntToStr(a) + '):  Add ' + IntToStr(size) + ' items to array' );
                client.ExtendArray('array', list);
            end;

            functionName := 'foobar';
            writeln('Run python function: ' + functionName);
            token := client.asyncClient.RunPythonFunction( functionName );
            writeln('after MyAsyncRunner.RunPythonFunction: token: ' + token);

            // Do other stuff here ...

            // But make sure that eventually "WaitForResponse" is called, to clear the entry
            // in the "ResponseMap" ... (otherwise there will be a leak!)
            // "WaitForResponse" can be called on a different thread

            writeln('before MyAsyncRunner.WaitForResponse: token: ' + token);
            client.asyncClient.WaitForResponse(token);
            writeln('after MyAsyncRunner.WaitForResponse: exit');


            writeln('Get result' );
            token := client.asyncClient.GetResult();

            // Do other stuff here ...

            jObject := client.asyncClient.WaitForResponse(token);
            client.asyncClient.HandleResponseGetResult(jObject, count, total);

            writeln( 'Results:' );
            writeln('     count = ' + IntToStr(count) );
            writeln('     total = ' + FloatToStr(total) );

        finally;
            writeln('Close' );
            client.Close();
        end;
    except
        on E: MyRunnerException do writeln( E.Message );
    end;
end;





// *****************************************************************************
// * Performance Test        With performance monitoring
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
    functionName : string;
    starttime : TDateTime;

begin
    try
        writeln('Startup' );
        starttime := Now;
        client := MyRunner.Create();
        Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
        // client.AttachLogger( self );

        try
            writeln('Create array' );
            starttime := Now;
            client.CreateArray('array');
            Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));

            iterations := 1000;
            for a := 0 to iterations - 1 do
            begin
                size := 1000;
                SetLength(list, size);

                for b := 0  to size - 1 do
                    list[b] := random();

                writeln('(' + IntToStr(a) + '):  Add ' + IntToStr(size) + ' items to array' );
                starttime := Now;
                client.ExtendArray('array', list);
                Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));
            end;

            functionName := 'foobar';
            writeln('Run python function: ' + functionName);
            starttime := Now;
            client.RunPythonFunction(functionName);
            Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));

            writeln('Get result' );
            starttime := Now;
            client.GetResult(count, total);
            Writeln('milliseconds: ', MilliSecondsBetween(Now, starttime));

            writeln( 'Results:' );
            writeln('     count = ' + IntToStr(count) );
            writeln('     total = ' + FloatToStr(total) );

        finally;
            writeln('Close' );
            client.Close();
        end;
    except
        on E: MyRunnerException do writeln( E.Message );
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

