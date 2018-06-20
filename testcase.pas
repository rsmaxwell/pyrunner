unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, RunnerInterfaces, Runner;

type
    MyTestCase = class(TInterfacedObject, IMyRunnerObserver)
    private
        client : MyRunner;
        procedure notify( operation: TMyRunnerOperation );
    public
        procedure simpleTest();
        procedure stressTest();
    end;

implementation

procedure MyTestCase.simpleTest();
var
    command : string;

begin
    client := MyRunner.Create();
    client.attachObserver( self );

    command := '{ "command": "run", "arguments": [ "name", "fred" ] }';
    writeln('calling : ' + command );
    client.WriteLn(command);

    sleep(1000);

    command := '{ "command": "get", "arguments": [ "name" ] }';
    writeln('calling : ' + command );
    client.WriteLn(command);

    sleep(5000);
end;


procedure MyTestCase.stressTest();
var
    a : integer;
    b : integer;
    list : array of real;
    iterations : integer;
    size : integer;

begin
    client := MyRunner.Create();
    client.attachObserver( self );

    writeln('Create array' );
    client.CreateArray('array');
    sleep(100);

    iterations := 100;
    for a := 0  to iterations - 1 do
    begin
        size := 1000;
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
end;

procedure MyTestCase.notify( operation: TMyRunnerOperation );
var
    opstring : string;
    lines : TStrings;
    line : AnsiString;
    i : integer;
begin
    WriteStr(opstring, operation);

    lines := TStringList.Create;

    if operation = TMyRunnerOperation.stdout then
        client.Read(lines)
    else
        client.Errors(lines);

    For i := 0 to lines.Count - 1 do
    begin
        line := lines[i];
        writeln(opstring + '   ' + line);
    end;
end;

end.

