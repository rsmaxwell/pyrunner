unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Math, Runner, RunnerLogger;

type
    MyTestCase = class(TInterfacedObject, IMyRunnerLogger)
    private
        client : MyRunner;
        procedure log( lines: TStrings );
    public
        procedure simpleTest();
    end;

implementation


procedure MyTestCase.simpleTest();
var
    a : integer;
    b : integer;
    list : array of real;
    iterations : integer;
    size : integer;
    rc : integer;
    rc2 : integer;
    ErrorMessage : AnsiString;

begin
    rc := 0;

    client := MyRunner.Create();
    client.attachLogger( self );

    if rc = 0 then
    begin
        writeln('Create array' );
        rc := client.CreateArray('array', ErrorMessage);
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    iterations := 2;
    for a := 0  to iterations - 1 do
    begin
        size := 10;
        SetLength(list, size);

        for b := 0  to size - 1 do
            list[b] := random();

        if rc = 0 then
        begin
            writeln('Add items to array' );
            rc := client.ExtendArray('array', list, ErrorMessage);
            if rc <> 0 then
            begin
                writeln( ErrorMessage );
            end;
        end;
    end;

    if rc = 0 then
    begin
        writeln('Run python function' );
        rc := client.RunPythonFunction( 'foobar', ErrorMessage );
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
    end;

    if rc = 0 then
    begin
        writeln('Get result' );
        rc := client.GetField('result', ErrorMessage);
        if rc <> 0 then
        begin
            writeln( ErrorMessage );
        end;
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
//* Observer
// *****************************************************************************
procedure MyTestCase.log( lines: TStrings );
var
    line : AnsiString;
    i : integer;
begin
    For i := 0 to lines.Count - 1 do
    begin
        line := lines[i];
        writeln('stdout   ' + line);
    end;
end;


end.

