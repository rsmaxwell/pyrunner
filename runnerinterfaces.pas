unit RunnerInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SMyRunnerObserver = '{3ca1a046-ae3d-4cd1-979f-9777f3b9ac53}';


  type
    TMyRunnerOperation = (
        stdout,
        stderr,
        logger
    );

    IMyRunnerObserver = interface
        [SMyRunnerObserver]
        procedure notify( operation: TMyRunnerOperation );
    end;


    IMyRunnerObserved = interface
        procedure attachObserver( observer : IMyRunnerObserver );
        procedure detachObserver( observer : IMyRunnerObserver );
    end;


    implementation

end.



