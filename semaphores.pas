unit Semaphores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSemaphore }

  TSemaphore = class
  private
    FEvent: PRTLEvent;
    FCount: Integer;
  public
    constructor Create(ACount: Integer);
    destructor Destroy;
    procedure Wait;
    procedure Post;
    property Count: Integer read FCount;
  end;


implementation

{ TSemaphore }

constructor TSemaphore.Create(ACount: Integer);
begin
  FCount:= ACount;
  FEvent:= RTLEventCreate;
end;

destructor TSemaphore.Destroy;
begin
  RTLeventdestroy(FEvent);
  inherited Destroy;
end;

procedure TSemaphore.Wait;
begin
  if InterLockedDecrement(FCount) < 0 then
    RTLeventWaitFor(FEvent);
end;

procedure TSemaphore.Post;
begin
  if InterLockedIncrement(FCount) <= 0 then
    RTLeventSetEvent(FEvent);
end;

end.


