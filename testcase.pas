unit TestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Runner;

type
    MyTestCase = class
    private
        client : MyRunner;
    public
        procedure simpleTest();
    end;

implementation


procedure MyTestCase.simpleTest();
var
    a : integer;
    b : integer;
    list : array of real;
    iterations : integer;
    size : integer;

begin
    client := MyRunner.Create();

    writeln('Create array' );
    client.CreateArray('array');
    sleep(100);

    iterations := 2;
    for a := 0  to iterations - 1 do
    begin
        size := 10;
        SetLength(list, size);

        for b := 0  to size - 1 do
            list[b] := random();

        writeln('Add items to array' );
        client.ExtendArray('array', list);
        sleep(100);
    end;

    writeln('Run python function' );
    client.RunPythonFunction( 'foobar' );
    sleep(100);

    writeln('Get result' );
    client.GetField('result');
    sleep(100);
end;



end.

