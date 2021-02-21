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
