# pyrunner

This pascal project starts python program and implements IPC to the allow calls to the runing python program to set state, run functions and retrieve results.

![pyrunner](images/pyrunner.png "pyrunner")

## [Runner](runner.pas)

The [Runner](runner.pas) class provides an API to call into python and return with results. The API call will not return until the python call has completed and results are available. The corresponding async API is called and then the call waits until a response arrives.

## [RunnerAsync](runnerasync.pas)

When [RunnerAsync](runnerasync.pas) is created, a separate python process is started with reader threads attached to the stdout and stderr streams.

The [RunnerAsync](runnerasync.pas) class also provides an API which builds and writes a command to the inputStream of the python process then immediately returns.

When the reader threads read from the python output streams, it wakes up the blocked API call with the response.

# Getting Started


