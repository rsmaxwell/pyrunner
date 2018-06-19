#!/usr/bin/python3

from __future__ import print_function
import sys
import json
import time
import binascii


data = json.loads('{}')



def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def error( *args ):
    eprint("Error")
    print("{ \"status\": \"error\", \"message\": \"unexpected command\" }")

def quit( *args ):
    eprint("Quit")
    sys.exit()

def set( *args ):
    eprint("Set")
    if len(args) != 2:
        print("{ \"status\": \"error\", \"message\": \"expected 2 arguments, found " + str(len(args)) + "\" }")
        return

    key = args[0]
    value = args[1]

    data[ key ] = value
    print("{ \"status\": \"ok\" }")

def get( *args ):
    eprint("Get")
    if len(args) != 1:
        print("{ \"status\": \"error\", \"message\": \"expected 1 argument, found " + str(len(args)) + "\" }")
        return

    key = args[0]

    returnvalue = json.loads('{}')

    if key in data:
        returnvalue['status'] = 'ok'
        returnvalue['value'] = data[ key ]
    else:
        returnvalue['status'] = 'error'
        returnvalue['message'] = "field '" + key + "' not found"

    print(json.dumps(returnvalue))


def run( *args ):
    eprint("Run")
    print("{ \"status\": \"ok\" }")





commands = {'quit': 'quit', 'set': 'set', 'get': 'get', 'run': 'run'}

while True:
    json_string = input()

    try:
        parsed_json = json.loads(json_string)
    except ValueError:
        eprint('Failed to parse input as json')
        eprint('json_string: ', json_string)
        print("{ \"status\": \"error\", \"message\": \"Failed to parse input as json\" }")
        continue

    arguments = json.loads('[]')
    if 'arguments' in parsed_json:
        arguments = parsed_json['arguments']

    if 'command' not in parsed_json:
        eprint("No 'command' field in input")
        eprint(json.dumps(parsed_json, sort_keys=True, indent=4, separators=(',', ': ')))
        print("{ \"status\": \"error\", \"message\": \"No 'command' field in input\" }")
        continue

    command_string = parsed_json['command']
    command = commands.get(command_string, 'error')

    command_and_arguments = command + '('
    seperator = ''
    for argument in arguments:
        command_and_arguments = command_and_arguments + seperator + '"' + argument + '"'
        seperator = ', '

    command_and_arguments = command_and_arguments + ')'
    eprint("command_and_arguments = " + command_and_arguments)

    eval( command_and_arguments )









