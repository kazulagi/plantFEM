#!/usr/bin/env python
# -*- coding:utf-8 -*-


import argparse
import json
import os
import time
from http.server import BaseHTTPRequestHandler, HTTPServer

class MyHandler(BaseHTTPRequestHandler):
    
    def do_POST(self):
        try:
            # get JSON file 
            content_len=int(self.headers.get('content-length'))
            requestBody = json.loads(self.rfile.read(content_len).decode('utf-8'))

            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            #responseBody = json.dumps(response)

            # save jsonfile as input.json
            f = open("input.json","w")

            json_content =str(requestBody)
            json_content = json_content.replace(",",",\n")
            json_content = json_content.replace("{","{\n")
            json_content = json_content.replace("}","\n}")
            json_content = json_content.replace("'",'"')
            f.write( json_content)
            f.close()

            #run simulation :: input.json => output.json
            os.system("./plantfem run")

            # open output.json and send it as responce
            with open("output.json") as f:
                data = json.load(f)
            responseBody = json.dumps(data)
            self.wfile.write(responseBody.encode('utf-8'))

        except Exception as e:
            print("An error occured")
            print("The information of error is as following")
            print(type(e))
            print(e.args)
            print(e)
            response = { 'status' : 500,
                         'msg' : 'An error occured' }
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            responseBody = json.dumps(response)

            self.wfile.write(responseBody.encode('utf-8'))

def importargs():
    parser = argparse.ArgumentParser("This is the simple server")

    parser.add_argument('--host', '-H', required=False, default='localhost')
    parser.add_argument('--port', '-P', required=False, type=int, default=8080)

    args = parser.parse_args()

    return args.host, args.port

def run(server_class=HTTPServer, handler_class=MyHandler, server_name='localhost', port=8080):

    server = server_class((server_name, port), handler_class)
    server.serve_forever()

def main():
    host, port = importargs()
    run(server_name=host, port=port)

if __name__ == '__main__':
    main()

#import json
#import os
#
## convert server.json into server.f90
#
#server_json = open('server.json', 'r')
#server = json.load(server_json)
#
#server_f90 = open('server.f90','w')
#
#server_f90.write('program main\n')
#server_f90.write("  ")
#server_f90.write('use plantFEM\n')
## declear variables
#server_f90.write("  ")
#server_f90.write("type(")
#server_f90.write(str(server["object"]["type"]) )
#server_f90.write(") :: ")
#server_f90.write(str(server["object"]["name"]))
#server_f90.write("\n")
#
## Operations
#if "create" in str(server["object"]["exec"]):
#    server_f90.write("  ")
#    server_f90.write("call "+str(server["object"]["name"])+"%create(")
#    # arguments
#    if "meshtype" in str(server["object"]["exec"]["create"]):
#        server_f90.write('meshtype='+"'"+str(server["object"]["exec"]["create"]["meshtype"])+"'")
#    server_f90.write(")\n")
#
## Operations
#if "msh" in str(server["object"]["exec"]):
#    server_f90.write("  ")
#    server_f90.write("call "+str(server["object"]["name"])+"%msh(")
#    # arguments
#    if "name" in str(server["object"]["exec"]["msh"]):
#        server_f90.write('name='+"'"+str(server["object"]["exec"]["msh"]["name"])+"'")
#    server_f90.write(")\n")
#
#    #server_f90.write("call "+str(server["object"]["name"])+"%msh(")
#    #if "name" in str(server["object"]["exec"]["msh"]):
#    #    server_f90.write('name='+"'"+str(server["object"]["exec"]["msh"]["name"])+"'")
#    #server_f90.write(")")
#server_f90.write('end program main\n')
#server_f90.close()
#os.system("./plantfem run")
#