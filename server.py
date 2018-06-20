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
        json.dump(response, sys.stdout)
        print()

def errorResponse(message):
        response = {}
        response['status'] = "error"
        response['message'] = message
        json.dump(response, sys.stdout)
        print()

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)



def foobar():
    eprint("FooBar")

    total = 0.0
    for value in data['array']:
        total = total + value

    data['result'] = {}
    data['result']['count'] = len(data['array'])
    data['result']['total'] = total

    okResponse()




def quit( *args ):
    eprint("Quit")
    sys.exit()





def run( *args ):
    eprint("Run")

    arguments = json.loads('[]')
    if 'arguments' in parsed_json:
        arguments = parsed_json['arguments']

    if len(arguments) != 1:
        errorResponse("Expected 1 argument, found " + str(len(arguments)))
        return

    python = arguments[0]

    try:
        exec( python )
        print("{ \"status\": \"ok\" }")
    except Exception as e:
        errorResponse("Caught exception: " + str(e))
        eprint( sys.exc_info()[0] )





def get( *args ):
    eprint("Get")

    arguments = json.loads('[]')
    if 'arguments' in parsed_json:
        arguments = parsed_json['arguments']

    if len(arguments) != 1:
        errorResponse("Expected 1 argument, found " + str(len(arguments)))
        return

    field = arguments[0]

    returnvalue = json.loads('{}')
    if field in data:
        returnvalue['status'] = 'ok'
        returnvalue['value'] = data[ field ]
    else:
        returnvalue['status'] = 'error'
        returnvalue['message'] = "field '" + field + "' not found"

    print(json.dumps(returnvalue))



commands = {'quit': 'quit()', 'run': 'run()', 'get': 'get()'}

while True:
    json_string = input()

    try:
        parsed_json = json.loads(json_string)
    except Exception as e:
        errorResponse("Failed to parse input as json:" + str(e))
        eprint('Failed to parse input as json')
        eprint('json_string: ', json_string)
        eprint( sys.exc_info()[0] )
        continue

    if 'command' not in parsed_json:
        errorResponse("No 'command' field in input")
        eprint("No 'command' field in input")
        eprint(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        continue

    command_string = parsed_json['command']
    command = commands.get(command_string)

    if command is None:
        errorResponse("Unexpected command: " + command_string)
        eprint("Unexpected command: " + command_string)
        eprint(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        continue

    try:
        eval( command )

    except Exception as e:
        errorResponse("Caught exception: " + str(e))
        eprint("Caught exception: " + str(e))
        eprint( sys.exc_info()[0] )
        eprint(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        continue








