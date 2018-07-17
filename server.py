#!/usr/bin/python3

from __future__ import print_function
import sys
import json
import time
import binascii
from datetime import datetime

data = {}


def okResponse():
        response = {}
        response['status'] = "ok"
        response['token'] = token
        eprint(json.dumps(response))

def errorResponse(message):
        response = {}
        response['status'] = "error"
        response['token'] = token
        response['message'] = message
        eprint(json.dumps(response))


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)
    sys.stderr.flush()


def foobar():
    print("FooBar")
    sys.stdout.flush()

    total = 0.0
    for value in data['array']:
        total = total + value

    data['result'] = {}
    data['result']['count'] = len(data['array'])
    data['result']['total'] = total



def quit( *args ):
    print("Quit")
    sys.stdout.flush()
    okResponse()
    sys.exit()





def run( *args ):
    print("Run")
    sys.stdout.flush()

    arguments = []
    if 'arguments' in parsed_json:
        arguments = parsed_json['arguments']

    if len(arguments) != 1:
        errorResponse("Expected 1 argument, found " + str(len(arguments)))
        return

    python = arguments[0]

    try:
        exec( python )
        okResponse()
    except Exception as e:
        errorResponse(str(e))
        print( sys.exc_info()[0] )
        sys.stdout.flush()





def get( *args ):
    print("Get")
    sys.stdout.flush()

    arguments = json.loads('[]')
    if 'arguments' in parsed_json:
        arguments = parsed_json['arguments']

    if len(arguments) != 1:
        errorResponse("Expected 1 argument, found " + str(len(arguments)))
        return

    field = arguments[0]

    response = {}
    if field in data:
        response['status'] = 'ok'
        response['token'] = token
        response['result'] = data[ field ]
    else:
        response['status'] = 'error'
        response['token'] = token
        response['message'] = "field '" + field + "' not found"

    eprint(json.dumps(response))



# https://stackoverflow.com/questions/956867/how-to-get-string-objects-instead-of-unicode-from-json/33571117#33571117
def json_load_byteified(file_handle):
    return _byteify(
        json.load(file_handle, object_hook=_byteify),
        ignore_dicts=True
    )

def json_loads_byteified(json_text):
    return _byteify(
        json.loads(json_text, object_hook=_byteify),
        ignore_dicts=True
    )

def _byteify(data, ignore_dicts = False):
    # if this is a unicode string, return its string representation
    if isinstance(data, unicode):
        return data.encode('utf-8')
    # if this is a list of values, return list of byteified values
    if isinstance(data, list):
        return [ _byteify(item, ignore_dicts=True) for item in data ]
    # if this is a dictionary, return dictionary of byteified keys and values
    # but only if we haven't already byteified it
    if isinstance(data, dict) and not ignore_dicts:
        return {
            _byteify(key, ignore_dicts=True): _byteify(value, ignore_dicts=True)
            for key, value in data.iteritems()
        }
    # if it's anything else, return it in its original form
    return data













if sys.version_info[0] >= 3:
    get_input = input
else:
    get_input = raw_input




commands = {'quit': 'quit()', 'run': 'run()', 'get': 'get()'}

while True:

    token = "?????"

    json_string = get_input()
    debug = (json_string[:200] + '...') if len(json_string) > 200 else json_string
    print('input = ', debug)
    sys.stdout.flush()

    try:
        parsed_json = json.loads(json_string)
        #parsed_json = json_loads_byteified(json_string)
    except Exception as e:
        errorResponse("Failed to parse input as json:" + str(e))
        print('Failed to parse input as json')
        print('json_string: ', json_string)
        print( sys.exc_info()[0] )
        sys.stdout.flush()
        continue

    if 'token' not in parsed_json:
        errorResponse("No 'token' field in input")
        print("No 'token' field in input")
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        sys.stdout.flush()
        continue

    token = parsed_json['token']
    print('token = ', token)
    sys.stdout.flush()

    if 'command' not in parsed_json:
        errorResponse("No 'command' field in input")
        print("No 'command' field in input")
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        sys.stdout.flush()
        continue

    command_string = parsed_json['command']
    print('command_string = ', command_string)
    sys.stdout.flush()
    command = commands.get(command_string)
    print('command = ', command)
    sys.stdout.flush()

    if command is None:
        errorResponse("Unexpected command: " + command_string)
        print("Unexpected command: " + command_string)
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        sys.stdout.flush()
        continue

    try:
        eval( command )

    except Exception as e:
        errorResponse(str(e))
        print("Caught exception: " + str(e))
        print( sys.exc_info()[0] )
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        sys.stdout.flush()
        continue








