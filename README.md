# pyrunner

This pascal project provides an API to calls into a runing python program to set state, run functions and retrieve results.

![pyrunner](images/pyrunner.png "pyrunner")

## [Runner](runner.pas)

The [Runner](runner.pas) class provides an API to call into python and return with results. The API call will not return until the python call has completed and results are available. The corresponding async API is called and then the call waits until a response arrives.

## [RunnerAsync](runnerasync.pas)

When [RunnerAsync](runnerasync.pas) is created, a separate python process is started with reader threads attached to the stdout and stderr streams.

The [RunnerAsync](runnerasync.pas) class also provides an API which builds and writes a command to the inputStream of the python process then immediately returns.

When the reader threads read from the python output streams, it wakes up the blocked API call with the response.

# Getting Started

This project currently only works on Windows

Make sure [python3](https://www.python.org/downloads/) is available on the system path.

Get [Lazarus](https://www.lazarus-ide.org), point it at [test.lpi](test.lpi) and select Run or F9 

# [SimpleExample](testcase.pas)

```pascal
    client := MyRunner.Create();
    // client.AttachLogger( self );

    client.CreateArray('array', ErrorMessage);

    SetLength(list, 2);
    list[0] := 123.456;
    list[1] := 456.789;
    client.ExtendArray('array', list, ErrorMessage);

    client.RunPythonFunction( 'foobar', ErrorMessage );

    client.GetResult('result', count, total, ErrorMessage);
    writeln( 'Results:' );
    writeln('     count = ' + IntToStr(count) );
    writeln('     total = ' + FloatToStr(total) );

    client.Close( ErrorMessage );
```


# API - MyRunner

## Create();

Create must be called first to initialise the Runner system. A new process is started running python [server.py](server.py). [Reader](streamreader.pas) threads are attached to the process output streams

## AttachLogger( logger : [IMyRunnerLogger](runnerlogger.pas) );

Specifies a class containing a callback where [Runner](runner.pas) will send log records including standard output from the python process

## CreateArray( field : AnsiString; var ErrorMessage : AnsiString ) : integer;

Creates an empty array of the given name as a field of __data__ in the python process. 

For example if the __field__ name was __array__ the following command would be run on the python process:

```python
data["array"] = []
```

Returns:
  * '0' on success.
  * 'non-zero' on failure and __ErrorMessage__ will be set to describe the problem. 


## ExtendArray( field : AnsiString; list : array of real; var ErrorMessage : AnsiString ) : integer;

Adds a list of numbers to an array in the python process. 

For example if the __field__ name was __array__ and the list contained __123.456__ and __456.789__ the following command would be run on the python process:

```python
data["array"].extend( (123.456, 456.789) )
```

Returns:
  * '0' on success.
  * 'non-zero' on failure and __ErrorMessage__ will be set to describe the problem. 


## RunPythonFunction( pythonFunction : AnsiString) : string;

Runs the named function in the python process. 

For example if the __pythonFunction__ name was __foobar__ the following command would be run on the python process:

```python
foobar()
```

Returns:
  * '0' on success.
  * 'non-zero' on failure and __ErrorMessage__ will be set to describe the problem. 


## GetResult(field : AnsiString; var count : integer; var total : real; var ErrorMessage : AnsiString ) : integer;

Gets __result__ field from the map __data__ from the python process, and extracts the integer __count__ and the float __total__


Returns:
  * '0' on success and __count__ and __total__ will be set 
  * 'non-zero' on failure and __ErrorMessage__ will be set to describe the problem. 


## Close( var ErrorMessage : AnsiString ) : integer;

Cleans up resources used by the [Runner](runner.pas). 

The python process is sent a __quit__ command to terminate it.

Returns:
  * '0' on success
  * 'non-zero' on failure and __ErrorMessage__ will be set to describe the problem. 



