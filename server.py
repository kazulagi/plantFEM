from wsgiref.simple_server import make_server
import json
import requests
import os

def app(environ, start_response):
    status = '200 OK'
    headers = [
      ('Content-type', 'application/json; charset=utf-8'),
      ('Access-Control-Allow-Origin', '*'),
    ]
    start_response(status, headers)
    
    # 事前にlocalhost:3000に何かしらのサーバー立ててください。
    # localhost 3000番にhttpGET
    url= 'http://localhost:3000/post'
    req = requests.get(url)
    print(req.text)
    
    jsonfile=str(req.text)
    
    # bodyをinput.jsonに改名
    f=open("input.json",'w')
    f.write(str(jsonfile))
    f.close()

    # input.json >> 計算の実行 >> output.json
    os.system("./plantfem run")

    # 出力の読み込み
    f=open("output.json",'r')
    jsonfile=f.read()
    f.close()

    # 計算結果ファイルの返却
    return [jsonfile.encode("utf-8")]

with make_server('', 3005, app) as httpd:
    print("Serving on port 3005...")
    httpd.serve_forever()