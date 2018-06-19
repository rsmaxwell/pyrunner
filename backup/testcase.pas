unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RunnerInterfaces, Runner;

type
    MyTestCase = class(TInterfacedObject, IMyRunnerObserver)
    private
        client : MyRunner;
        procedure notify( operation: TMyRunnerOperation );
    public
        procedure simpleTest();
    end;

implementation

procedure MyTestCase.simpleTest();
var
    command : AnsiString;

begin
    client := MyRunner.Create();
    client.attachObserver( self );

    command := '{ "command": "set", "arguments": [ "name", "fred" ] }';
    writeln('calling : ' + command );
    client.WriteLn(command);

    sleep(1000);

    command := '{ "command": "get", "arguments": [ "name" ] }';
    writeln('calling : ' + command );
    client.WriteLn(command);

    sleep(5000);
end;

procedure MyTestCase.notify( operation: TMyRunnerOperation );
var
    opstring : string;
    lines : TStrings;
    line : AnsiString;
    i : integer;
begin
    WriteStr(opstring, operation);
    writeln('---[ ' + opstring + ']----------------------');

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

