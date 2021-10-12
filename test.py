import json

json_open=open('test.json', 'r')
indata=json.load(json_open)
for i in range(10):
    print(indata["key"][i])