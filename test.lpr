program client;

{$mode objfpc}{$H+}

uses Classes, SysUtils, TestCase, RunnerLogger, ResponseItem;

var
    test : MyTestCase;

begin
    test := MyTestCase.Create();
    test.asyncTest();
end.

