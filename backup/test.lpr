program client;

{$mode objfpc}{$H+}

uses Classes, SysUtils, TestCase;

{$R *.res}


var
    test : MyTestCase;

begin
    test := MyTestCase.Create();
    test.simpleTest();
end.

