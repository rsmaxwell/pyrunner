program client;

{$mode objfpc}{$H+}

uses Classes, SysUtils, TestCase, RunnerLogger;

var
    test : MyTestCase;

begin
    test := MyTestCase.Create();
    test.simpleTest();
end.

