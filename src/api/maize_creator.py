from typing import List

from fastapi import FastAPI, File, UploadFile
from fastapi.responses import FileResponse
from fastapi.responses import HTMLResponse
import shutil
import uuid
from pathlib import Path
from datetime import datetime
import os

app = FastAPI()

@app.get("/downloadfile")
async def get_file(filename: str):
    '''任意ファイルのダウンロード'''
    current = Path()
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response

@app.post("/uploadfile/")
async def create_upload_files(
    files: List[UploadFile] = File(description="Multiple files as UploadFile"),
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.json'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        os.system("./server.out "+filename)
    content = """
<body>
Download 3-D maize plant from here!<br>
<br>
<a href="/">Main page</a>
<br>
<br>
Your file is<br>
<br>
<br>
"""+filename+".vtk"+ """<br>
<br>
<br>
</body>
    """    
    return HTMLResponse(content=content)
    

#UPLOAD_DIR = "./"

#@app.post("/uploadfile/")
#async def upload_file(file: UploadFile = File(description="Multiple files as UploadFile")):
#    if file:
#        filename = file.filename
#        fileobj = file.file
#        upload_dir = open(os.path.join(UPLOAD_DIR, filename),'wb+')
#        shutil.copyfileobj(fileobj, upload_dir)
#        upload_dir.close()
#        return {"アップロードファイル名": filename}
#    return {"Error": "アップロードファイルが見つかりません。"}

@app.get("/")
async def main():
    content = """
<body>

(1) If you want an example of the *.json file, please type maize.json and edit it!<br>
<form action="/downloadfile/" method="get">
<input name="filename" type="text" value="maize.json">
<input type="submit">
</form>
<br>
<br>
<br>
(2) Upload your edited maize.json!<br>
<br>
<form action="/uploadfile/" enctype="multipart/form-data" method="post">
<input name="files" type="file" multiple>
<input type="submit">
</form>
<br>
<br>
(3) Download your 3-D maize ! <br>
If you want an example of the *.json file, please type maize.json<br>
<form action="/downloadfile/" method="get">
<input name="filename" type="text" value="maize.json">
<input type="submit">
</form>
<br>
<br>
<br>
(4) Open the *.vtk file in paraview(https://www.paraview.org/).

</body>
    """
    return HTMLResponse(content=content)