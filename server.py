#!/usr/bin/python3

from __future__ import print_function
import sys
import json
import time
import binascii


data = json.loads('{}')


def okResponse():
        response = {}
        response['status'] = "ok"
        eprint(json.dumps(response))

def errorResponse(message):
        response = {}
        response['status'] = "error"
        response['message'] = message
        eprint(json.dumps(response))


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)



def foobar():
    print("FooBar")

    total = 0.0
    for value in data['array']:
        total = total + value

    data['result'] = {}
    data['result']['count'] = len(data['array'])
    data['result']['total'] = total

    okResponse()




def quit( *args ):
    print("Quit")
    sys.exit()





def run( *args ):
    print("Run")

    arguments = json.loads('[]')
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
        errorResponse("Caught exception: " + str(e))
        print( sys.exc_info()[0] )





def get( *args ):
    print("Get")

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
        response['value'] = data[ field ]
    else:
        response['status'] = 'error'
        response['message'] = "field '" + field + "' not found"

    eprint(json.dumps(response))



commands = {'quit': 'quit()', 'run': 'run()', 'get': 'get()'}

while True:
    json_string = input()

    try:
        parsed_json = json.loads(json_string)
    except Exception as e:
        errorResponse("Failed to parse input as json:" + str(e))
        print('Failed to parse input as json')
        print('json_string: ', json_string)
        print( sys.exc_info()[0] )
        continue

    if 'command' not in parsed_json:
        errorResponse("No 'command' field in input")
        print("No 'command' field in input")
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        continue

    command_string = parsed_json['command']
    command = commands.get(command_string)

    if command is None:
        errorResponse("Unexpected command: " + command_string)
        print("Unexpected command: " + command_string)
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        continue

    try:
        eval( command )

    except Exception as e:
        errorResponse("Caught exception: " + str(e))
        print("Caught exception: " + str(e))
        print( sys.exc_info()[0] )
        print(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        continue








