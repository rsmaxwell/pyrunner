unit StreamReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamIO, RunnerInterfaces, Semaphores;

type
    MyReader = class(TThread)
    private
        stream: TStream;
        linebuffer: TStrings;
        temp: TStrings;
        line: string;
        observers : TInterfaceList;
        operation: TMyRunnerOperation;
        semaphore: TSemaphore;

        procedure ReadInternal;
        procedure Update();
        procedure notifyObservers();
    protected
         procedure Execute; override;
    public
        constructor Create(s : TStream; op: TMyRunnerOperation; var obs : TInterfaceList);
        procedure Read(var buffer: TStrings);
    end;


implementation


constructor MyReader.Create(s : TStream; op: TMyRunnerOperation; var obs : TInterfaceList);
begin
    inherited Create(False);
    FreeOnTerminate := True;
    stream := s;
    linebuffer := TStringList.Create;
    observers := obs;
    operation := op;

    semaphore := TSemaphore.Create(1);
end;

procedure MyReader.Read(var buffer: TStrings);
begin
    ReadInternal;
    buffer := temp;
end;

procedure MyReader.ReadInternal();
begin
    semaphore.Wait();
    temp := linebuffer;
    linebuffer := TStringList.Create;
    semaphore.Post();
end;

procedure MyReader.Update();
begin
    semaphore.Wait();
    linebuffer.Add(line);
    semaphore.Post();

    notifyObservers();
end;

procedure MyReader.Execute; var
    f: Text;
begin

    while (not Terminated) do
    begin
        AssignStream(f, stream);
        Reset(f);
        while not EOF(f) do begin
          ReadLn(f, line);

          Update;
        end;
    end;
end; 

procedure MyReader.notifyObservers();
var
    observer: IUnknown;
    i: Integer;
begin
    for i := 0 to observers.Count - 1 do
    begin
        observer := observers[i];
        with observer as IMyRunnerObserver do
            notify( operation );
    end;
end;

end.


