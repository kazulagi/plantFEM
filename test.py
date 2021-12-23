import subprocess
import json

subprocess.call("plantfem build", shell=True)
ret = subprocess.call("./server.out", shell=True)

soy = json.load(ret)


