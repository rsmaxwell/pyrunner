unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Runner, RunnerLogger;

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

begin
    client := MyRunner.Create();
    client.attachLogger( self );

    writeln('Create array' );
    client.CreateArray('array');
    sleep(100);

    iterations := 2;
    for a := 0  to iterations - 1 do
    begin
        size := 10;
        SetLength(list, size);

        for b := 0  to size - 1 do
            list[b] := random();

        writeln('Add items to array' );
        client.ExtendArray('array', list);
        sleep(100);
    end;

    writeln('Run python function' );
    client.RunPythonFunction( 'foobar' );
    sleep(100);

    writeln('Get result' );
    client.GetField('result');
    sleep(100);

    writeln('Close' );
    client.Close();
    sleep(100);
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

