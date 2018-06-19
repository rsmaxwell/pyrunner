unit Runner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, StreamReader, RunnerInterfaces;



type
    MyRunner = class(TInterfacedObject, IMyRunnerObserved)
    private
        proc: TProcess;
        outputReader : MyReader;
        errorReader : MyReader;
        observers: TInterfaceList;
    public
        constructor Create;
        procedure WriteLn(line: AnsiString);
        procedure Read(var lines: TStrings);
        procedure Errors(var lines: TStrings);
        procedure Close();
        procedure attachObserver( observer : IMyRunnerObserver );
        procedure detachObserver( observer : IMyRunnerObserver );
    end;




implementation

const
    CarriageReturn: Byte = 13;
    LineFeed: Byte = 10;



constructor MyRunner.Create();
begin
    proc := TProcess.Create(nil);
    proc.Executable:= FindDefaultExecutablePath('python.exe');;
    proc.Parameters.Add('server.py');
    proc.Options := proc.Options + [poUsePipes, poNoConsole];
    proc.Execute;

    outputReader := MyReader.Create(proc.Output, TMyRunnerOperation.stdout, observers);
    errorReader := MyReader.Create(proc.Stderr, TMyRunnerOperation.stderr, observers);
end;


procedure MyRunner.WriteLn(line: AnsiString);
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


procedure MyRunner.Read(var lines: TStrings);
begin
    outputReader.Read( lines );
end;


procedure MyRunner.Errors(var lines: TStrings);
begin
    errorReader.Read( lines );
end;

procedure MyRunner.Close();
begin
    WriteLn('{ "command": "quit" }');

    Sleep(100);

    if not outputReader.Finished then
        outputReader.Terminate();

    if not errorReader.Finished then
        errorReader.Terminate();

    proc.Terminate(0);
end;

procedure MyRunner.attachObserver( observer : IMyRunnerObserver );
begin
    if observers.IndexOf(observer) = -1 then
        observers.Add(IUnknown(observer));
end;

procedure MyRunner.detachObserver( observer : IMyRunnerObserver );
begin
    observers.Remove(observer);
end;


end.


