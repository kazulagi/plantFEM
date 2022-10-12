import uvicorn
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

@app.get("/")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>Pricing example · Bootstrap</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>


<body>
    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>
    </div>
</body>
</html>
    """
    return HTMLResponse(content=content)



@app.get("/maize_creator/downloadfile")
async def get_file(filename: str):
    current = Path()
    if not filename.endswith(".json"):
        if not filename.endswith(".vtk"):
            return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response

@app.post("/maize_creator/uploadfile/")
async def create_upload_files(files: List[UploadFile] = File(...),
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.json'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
        os.system("./server_maize_creator.out "+filename)
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>Pricing example · Bootstrap</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>
    </div>


Download your 3-D maize ! <br>
<form class="row g-3" action="/maize_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input name="filename" t    ype="text"  class="form-control" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>



</body>
</html>
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

@app.get("/maize_creator")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>Pricing example · Bootstrap</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>



<body>


    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>
    </div>

<div class="container-fluid">
    
    If you want an example of the *.json file, please type maize.json and edit it! <br>
    <form class="row g-3" action="/maize_creator/downloadfile/" method="get">
        <div class="col-auto">
            <input name="filename" type="text" value="maize.json"  class="form-control">
        </div>
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Get template file">
        </div>
    </form>


 If you have an *.json file for maize, please upload from here!<br>
<form class="row g-3" action="/maize_creator/uploadfile/" enctype="multipart/form-data" method="post">
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-primary mb-2" value="Create">
    </div>
</form>

</body>
</html>
    """
    return HTMLResponse(content=content)


@app.get("/soybean_creator/downloadfile")
async def get_file(filename: str):
    current = Path()
    if not filename.endswith(".json"):
        if not filename.endswith(".vtk"):
            return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response

@app.post("/soybean_creator/uploadfile")
async def create_upload_files(files: List[UploadFile] = File(...),
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.json'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
        print("./server_soybean_creator.out "+filename)
        os.system("./server_soybean_creator.out "+filename)
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>Pricing example · Bootstrap</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>
    </div>


Download your 3-D soybean ! <br>
<form class="row g-3" action="/soybean_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input name="filename" t    ype="text"  class="form-control" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>



</body>
</html>
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

@app.get("/soybean_creator")
async def main():
    
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>Pricing example · Bootstrap</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>



<body>


    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/  soybean_creator">Soybean</a>
      </nav>
    </div>

<div class="container-fluid">
    
    If you want an example of the *.json file, please type soybean.json and edit it! <br>
    <form class="row g-3" action="/soybean_creator/downloadfile/" method="get">
        <div class="col-auto">
            <input name="filename" type="text" value="soybean.json"  class="form-control">
        </div>
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Get template file">
        </div>
    </form>


 If you have an *.json file for soybean, please upload from here!<br>
<form class="row g-3" action="/soybean_creator/uploadfile/" enctype="multipart/form-data" method="post">
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-primary mb-2" value="Create">
    </div>
</form>

</body>
</html>
    """
    return HTMLResponse(content=content)


#if __name__ == “__main__":
#    uvicorn.run(“main:app", port=8000)
