unit IRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    TMyRunnerObservedOperation = (
        ooChange,
        ooFree,
        ooAddItem,
        ooDeleteItem,
        ooCustom
    );
end.

type
    IMyRunnerObserved = interface(TInterfacedObject)
    public
        procedure attachObserver( observer : TObject );
        procedure detachObserver( observer : TObject );
        procedure notifyObservers( obj : TObject; operation : TMyRunnerObservedOperation; data : Pointer );
    end;

    IMyRunnerObserver = interface(TInterfacedObject)
    public;
        procedure observedChanged( obj : TObject; operation : TMyRunnerObservedOperation; data : Pointer );
    end;


implementation

end.

