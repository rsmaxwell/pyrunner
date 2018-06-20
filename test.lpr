program client;

{$mode objfpc}{$H+}

uses Classes, SysUtils, TestCase, Semaphores;

var
    test : MyTestCase;

begin
    test := MyTestCase.Create();
    test.stressTest();
end.

