program client;

{$mode objfpc}{$H+}

uses Classes, SysUtils, TestCase;

var
    test : MyTestCase;

begin
    test := MyTestCase.Create();
    test.stressTest();
end.

