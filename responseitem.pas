unit ResponseItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Semaphores;

type
    MyResponseItem = class
    private

    public 
        semaphore: TSemaphore;
        line: AnsiString;
        constructor Create();
        destructor Destroy; override;
    end;

implementation

// *****************************************************************************
//* Basic
// *****************************************************************************
constructor MyResponseItem.Create();
begin
    semaphore := TSemaphore.Create(0);
end;


destructor MyResponseItem.Destroy;
begin
    semaphore.Destroy();
    inherited Destroy;
end;


end.

