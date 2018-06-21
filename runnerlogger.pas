unit RunnerLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SMyRunnerLogger = '{e01cc103-16ad-4fb7-9608-a80a7e7387ee}';

type
    IMyRunnerlogger = interface
        [SMyRunnerLogger]
        procedure log( lines : TStrings );
    end;


implementation

end.

