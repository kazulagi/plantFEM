from http.client import HTTPResponse
import uvicorn
from typing import List

from fastapi import FastAPI, File, UploadFile
from fastapi.responses import FileResponse
from fastapi.responses import HTMLResponse
import subprocess
import shutil
import uuid
from pathlib import Path
from datetime import datetime
import os
import json

app = FastAPI()

#### TOP PAGE ####



@app.get("/")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>


<body>
    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs@agri</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/agri">Agriculture</a>
      </nav>
      
    </div>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs@civil</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/civil">Civil engineering</a>
      </nav>
    </div>
    

</body>
</html>
    """
    return HTMLResponse(content=content)

@app.get("/agri")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>


<body>
    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Pre-processing</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>
    </div>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Solvers</h5>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/static_analysis">Static analysis</a>
      </nav>


      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/modal_analysis">modal analysis</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/dynamic_analysis">Dynamic analysis</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/fluid_analysis">Fluid analysis</a>
      </nav>


    </div>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal"></h5>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>

    </div>

</body>
</html>
    """
    return HTMLResponse(content=content)


@app.get("/civil")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>


<body>
    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Pre-processing</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/embankment_creator">Embankment</a>
      </nav>

    </div>


    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Solvers</h5>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/static_analysis">Static analysis</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/modal_analysis">modal analysis</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/dynamic_analysis">Dynamic analysis</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/fluid_analysis">Fluid analysis</a>
      </nav>


    </div>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal"></h5>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>

    </div>
</body>
</html>
    """
    return HTMLResponse(content=content)

#### JSON Editor #####




###### MAIZE ######


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


@app.get("/maize_creator/download_new_json/")
async def download_new_json(Mainstem_Length: str, Mainstem_BottomWidth: str, 
    Mainstem_TopWidth: str,Mainstem_Node: str,Mainstem_ms_angle_ave: str,Mainstem_ms_angle_sig: str,
    Ear_1_Position:str,Ear_2_Position:str, Tassel_exists: str):
    current = Path()
    filename = "maize.json"
    file_path = current / filename
    
    with open(file_path, 'r') as fcc_file:
        fcc_data = json.load(fcc_file)
        fcc_data["Mainstem"]["Length"] = float(Mainstem_Length)
        fcc_data["Mainstem"]["BottomWidth"] = float(Mainstem_BottomWidth)
        fcc_data["Mainstem"]["TopWidth"] = float(Mainstem_TopWidth) 
        fcc_data["Mainstem"]["Node"]     = int(Mainstem_Node) 
        fcc_data["Mainstem"]["ms_anlge_ave"] = float(Mainstem_ms_angle_ave) 
        fcc_data["Mainstem"]["ms_angle_sig"] = float(Mainstem_ms_angle_sig)
        fcc_data["Panicle_1_"]["From"] = int(Mainstem_Node)
        
        if int(Tassel_exists)==0:
            # no panicle
            fcc_data.pop("Panicle_1_")

        # as for leaf
        # default == number of leaf == 12, node 1 ~ 12
        default_leaf_num=12
        for i in range(int(Mainstem_Node)):
            if i > default_leaf_num:
                # add
                fcc_data["Leaf_"+str(i+1)+"_" ] = fcc_data["Leaf_"+str(default_leaf_num)+"_" ] 
                fcc_data["Leaf_"+str(i+1)+"_" ]["From"] = i+1
                
        if default_leaf_num > int(Mainstem_Node) :
            for i in range(int(Mainstem_Node)+1 ,default_leaf_num+1):
                fcc_data.pop("Leaf_"+str(i)+"_")

        if int(Ear_1_Position)==0 :
            #echo
            fcc_data.pop("Ear_1_")
        else:
            fcc_data["Ear_1_"]["From"] = int(Ear_1_Position)
        
        if int(Ear_2_Position)==0 :
            fcc_data.pop("Ear_2_")
        else:
            fcc_data["Ear_2_"]["From"] = int(Ear_2_Position)

    new_filename = "download_"+str(uuid.uuid4())+"_maize.json"
    f = open(new_filename,"w")
    f.write((json.dumps(fcc_data, indent=4)))
    f.close()

    file_path = current / new_filename
    response = FileResponse(
        path=file_path,
        filename=f"{new_filename}"
        )
    
    return response

@app.get("/maize_creator/createjsonfile")
async def get_json_form():
    content = """
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>


<h2>Online Maize editor </h2><br>
<form class="form-group" action="/maize_creator/download_new_json" method="get">

    <div class="col-auto">
        Stem length (m)
        <input name="Mainstem_Length" type="text"  class="form-control" value="1.800">
    </div>

    <div class="col-auto">
        Diameter at bottom (m)
        <input name="Mainstem_BottomWidth" type="text"  class="form-control" value="0.030">
    </div>
    <div class="col-auto">
        Diameter at top (m)
        <input name="Mainstem_TopWidth" type="text"  class="form-control" value="0.007">
    </div>
    
    <div class="col-auto">
        Number of node in main stem 
        <input name="Mainstem_Node" type="text"  class="form-control" value="13">
    </div>

    <div class="col-auto">
        Average of internode angles (deg.)
        <input name="Mainstem_ms_angle_ave" type="text"  class="form-control" value="0.0">
    </div>
    <div class="col-auto">
        Variance of internode angles (deg.)
        <input name="Mainstem_ms_angle_sig" type="text"  class="form-control" value="2.0">
    </div>
    <div class="col-auto">
        Node ID where ear#1 is located (if no primary ear is present, set 0)
        <input name="Ear_1_Position" type="text"  class="form-control" value="5">
    </div>
    <div class="col-auto">
        Node ID where ear#2 is located (if no secondary ear is present, set 0)
        <input name="Ear_2_Position" type="text"  class="form-control" value="4">
    </div>
    <div class="col-auto">
        Is there a tassel? (if no tassel is present, set 0, otherwise, set 1)
        <input name="Tassel_exists" type="text"  class="form-control" value="1">
    </div>
    

    <div class="col-auto">
        Download from here!
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>

</form>
    """
    return HTMLResponse(content=content)

#    current = Path()
#    if not filename.endswith(".json"):
#        if not filename.endswith(".vtk"):
#            return {"status": "error"}
#    file_path = current / filename
#    
#    response = FileResponse(
#        path=file_path,
#        filename=f"download_{filename}"
#        )
#    
#    return response

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
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>


Download your 3-D maize ! <br>
<form class="row g-3" action="/maize_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>



</body>
</html>
    """    
    return HTMLResponse(content=content)

@app.get("/maize_creator")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
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

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
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

    If you want to create a *.json file, please type the name and click the botton! <br>
    <form class="row g-3" action="/maize_creator/createjsonfile/" method="get">
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Create .json file">
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


###### SOYBEAN ######

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
async def create_upload_files(files: List[UploadFile] = File(...)
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
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>


      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>


Download your 3-D soybean ! <br>
<form class="row g-3" action="/soybean_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>



</body>
</html>
    """    
    return HTMLResponse(content=content)
    

@app.get("/soybean_creator")
async def main():
    
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
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

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
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



################################################
######### Civil Engineering ####################
################################################


###### Bridge ######


@app.get("/bridge_creator/downloadfile")
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


@app.get("/bridge_creator/download_new_json/")
async def download_new_json(Width: str, NumPiers_x: str, NumPiers_y: str,
    Length: str, Height: str, PierThickness: str, Divisions_x: str,
    Divisions_y: str,Divisions_z: str,GirderWidth,
    GirderThickness:str,
    GirderEdgeHeight:str,
    GirderEdgeThickness:str):

    current = Path()
    filename = "bridge.json"
    file_path = current / filename
    
    with open(file_path, 'r') as fcc_file:
        fcc_data = json.load(fcc_file)
        fcc_data["NumPiers_x"] = int(NumPiers_x)
        fcc_data["NumPiers_y"] = int(NumPiers_y)
        fcc_data["Width"] = float(Width)
        fcc_data["Length"] = float(Length)
        fcc_data["Height"] = float(Height)
        fcc_data["PierThickness"] = float(PierThickness)
        fcc_data["Divisions_x"]     = int(Divisions_x)
        fcc_data["Divisions_y"]     = int(Divisions_y)
        fcc_data["Divisions_z"]     = int(Divisions_z) 
        fcc_data["GirderWidth"]     = float(GirderWidth)
        fcc_data["GirderThickness"]     = float(GirderThickness)
        fcc_data["GirderEdgeHeight"]     = float(GirderEdgeHeight) 
        fcc_data["GirderEdgeThickness"]     = float(GirderEdgeThickness) 
        fcc_data.pop("NumMiddlePier")
        fcc_data.pop("MiddlePierHeights")
        
    new_filename = "download_"+str(uuid.uuid4())+"_bridge.json"
    f = open(new_filename,"w")
    f.write((json.dumps(fcc_data, indent=4)))
    f.close()

    file_path = current / new_filename
    response = FileResponse(
        path=file_path,
        filename=f"{new_filename}"
        )
    
    return response

@app.get("/bridge_creator/createjsonfile")
async def get_json_form():
    content = """
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/embankment_creator">Embankment</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>


<h2>Online bridge editor </h2><br>
<form class="form-group" action="/bridge_creator/download_new_json" method="get">

    <div class="col-auto">
        Number of piers for Length-direction
        <input name="NumPiers_x" type="text"  class="form-control" value="2">
    </div>


    <div class="col-auto">
        Number of piers for width-direction
        <input name="NumPiers_y" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Width of bridge (m)
        <input name="Width" type="text"  class="form-control" value="10.00">
    </div>
    
    <div class="col-auto">
        Length of bridge (m)
        <input name="Length" type="text"  class="form-control" value="35.00">
    </div>


    <div class="col-auto">
        Height of bridge (m)
        <input name="Height" type="text"  class="form-control" value="12.00">
    </div>

    <div class="col-auto">
        Thickness of piers (m)
        <input name="PierThickness" type="text"  class="form-control" value="1.00">
    </div>

    <div class="col-auto">
        Mesh divisions for length-direction 
        <input name="Divisions_x" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Mesh divisions for width-direction 
        <input name="Divisions_y" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Mesh divisions for height-direction 
        <input name="Divisions_z" type="text"  class="form-control" value="3">
    </div>    


    <div class="col-auto">
        Width of girder (m) (In case of no girder, please set 0.00)
        <input name="GirderWidth" type="text"  class="form-control" value="11.000">
    </div>    

    <div class="col-auto">
        Thickness of girder (m) (In case of no girder, please set 0.00)
        <input name="GirderThickness" type="text"  class="form-control" value="0.3000">
    </div>    

    <div class="col-auto">
        Height of edge in girder (m) (In case of no girder, please set 0.00)
        <input name="GirderEdgeHeight" type="text"  class="form-control" value="1.000">
    </div>    

    <div class="col-auto">
        Thickness of edge in girder (m) (In case of no girder, please set 0.00)
        <input name="GirderEdgeThickness" type="text"  class="form-control" value="0.25000">
    </div>    

    <div class="col-auto">
        Download from here!
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>

</form>
    """
    return HTMLResponse(content=content)

#    current = Path()
#    if not filename.endswith(".json"):
#        if not filename.endswith(".vtk"):
#            return {"status": "error"}
#    file_path = current / filename
#    
#    response = FileResponse(
#        path=file_path,
#        filename=f"download_{filename}"
#        )
#    
#    return response

@app.post("/bridge_creator/uploadfile/")
async def create_upload_files(files: List[UploadFile] = File(...),
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.json'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
        #os.system("./server_bridge_creator.out "+filename)
        sts = subprocess.Popen("./server_bridge_creator.out " + filename , shell=True)
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/embankment_creator">Embankment</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>

Download your 3-D bridge ! <br>
<form class="row g-3" action="/bridge_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>



</body>
</html>
    """    
    return HTMLResponse(content=content)

@app.get("/bridge_creator")
async def main():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>



<body>


    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/embankment_creator">Embankment</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>

<div class="container-fluid">
    
    If you want an example of the *.json file, please type bridge.json and edit it! <br>
    <form class="row g-3" action="/bridge_creator/downloadfile/" method="get">
        <div class="col-auto">
            <input name="filename" type="text" value="bridge.json"  class="form-control">
        </div>
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Get template file">
        </div>
    </form>

    If you want to create a *.json file, please type the name and click the botton! <br>
    <form class="row g-3" action="/bridge_creator/createjsonfile/" method="get">
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Create .json file">
        </div>
    </form>

 If you have an *.json file for bridge, please upload from here!<br>
<form class="row g-3" action="/bridge_creator/uploadfile/" enctype="multipart/form-data" method="post">
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



@app.get("/view_graph")
async def view_graph():
    content = """
    <body>

  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

 <style>
   /* 線の色の変更 */
   .epoch .category1 .line {
     stroke: #dc3545;
   }

   .epoch .category2 .line {
     stroke: #17a2b8;
   }
</style>
<div class="container">
    <h1 class="text-center">現在の気温・湿度</h1>
    <div class="row">
        <div class="mx-auto">
            <p>
                気温：<img width="40" height="20" class="bg-danger">
                湿度：<img width="40" height="20" class="bg-info">
            </p>
        </div>
        <div id="myChart" class="epoch" style="width: 100%; height: 200px">
        </div>
    </div>
    <div class="row mt-3">
        <div class="col-12">
            <form>
                <div class="form-group">
                    <label for="temperature">気温（－１５℃～３５℃）</label>
                    <input id="temperature" type="range" class="form-control-range" min="-15" max="35" value="20">
                </div>
                <div class="form-group">
                    <label for="humidity">湿度（０％～１００％）</label>
                    <input id="humidity" type="range" class="form-control-range" min="0" max="100">
                </div>
            </form>
        </div>
    </div>
</div>
<script src="https://code.jquery.com/jquery-3.2.1.slim.min.js"
        integrity="sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN"
        crossorigin="anonymous"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js"
        integrity="sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl"
        crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.10.6/moment.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.10.6/locale/ja.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.3.13/d3.js" charset="utf-8"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/epoch/0.8.4/js/epoch.min.js"></script>

<script>

    // データ範囲 左右別
    var leftRange = [-20, 40];
    var rightRange = [-5, 105];
    // 初期データ
    var data = [
            {
                label: "layer1",
                range: leftRange,
                values: [],
            },
            {
                label: "layer2",
                range: rightRange,
                values: [],
            }
        ]
    ;
    // 初期化
    let chart = $('#myChart').epoch({
        type: 'time.line',                         //グラフの種類
        data: data,                                  //初期値
        axes: ['bottom', 'left', 'right'],       //利用軸の選択
        fps: 24,                                     //フレームレート
        range: {                                     //軸の範囲
            left: leftRange,
            right: rightRange
        },
        queueSize: 1,   // キューサイズ ※push時、キューからあふれたデータは破棄される
        windowSize: 20, // 表示から見切れるまでいくつデータを表示させるか

        // 目盛りの設定。 timeは間隔秒数、他は目盛りの数
        ticks: {time: 5, right: 5, left: 5},
        // 目盛りの書式
        tickFormats: {
            bottom: function (d) {
                return moment(d * 1000).format('HH:mm:ss');
            },
            left: function (d) {
                return (d).toFixed(1) + " ℃";
            },
            right: function (d) {
                return (d).toFixed(0) + " %";
            }

        }
    });

    // リアルタイム表示処理
    setInterval(function () {
        chart.push(
            [
                {time: Date.now() / 1000, y: $("#temperature")[0].value,},
                {time: Date.now() / 1000, y: $("#humidity")[0].value,},
            ],
        );
    }, 1000);

</script>

</body>
</html>
    """
    return HTMLResponse(content=content)


#### Solvers ####
#### Solvers ####
#### Solvers ####
#### Solvers ####


@app.get("/modal_analysis")
async def modal_analysis_info():
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>



<body>


    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/embankment_creator">Embankment</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>

<div class="container-fluid">
    
    
<form class="form-group" action="/upload_vtk_file/" enctype="multipart/form-data" method="post">   
    [1] First, please upload VTK file and get access token. 
    
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-outline-success" value="Create access token">
    </div>

</form>


<form class="form-group" action="/modal_analysis/uploadfile/" enctype="multipart/form-data" method="get">
    [2] Second, please upload conditions and access token. 
    
    <div class="col-auto">
        Set Young's modulus (m) <br>
        <input name="YoungModulus" class="form-control" type="text" value="10000000.00">
    </div>

    <div class="col-auto">
        Set Poisson's ratio (m) <br>
        <input name="PoissonRatio" class="form-control" type="text" value="0.300">
    </div>

    <div class="col-auto">
        Set density (t/m^3) <br>
        <input name="Density" class="form-control" type="text" value="1.800">
    </div>

    <div class="col-auto">
        Set ground level (m) <br>
        <input name="ground_level"  class="form-control" type="text" value="0.100">
    </div>

    <div class="col-auto">
        Set VTK file access token <br>
        <input name="filename"  class="form-control" type="text" value="">
    </div>


    <div class="col-auto">
        This process can take from a few minutes to several hours.
        <input type="submit"  class="btn btn-outline-success" value="Run modal analysis"/>
    </div>


</form>



</body>
</html>
    """
    return HTMLResponse(content=content)


@app.post("/upload_vtk_file")
async def create_upload_files(files: List[UploadFile] = File(...)
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.vtk'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
        
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>

    </div>





VTK file is registered! <br>
Here is the token. Please copy&paste this token in a solver. <br>
<form class="row g-3" action="/soybean_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value="""+filename+ """ id="vtk_token">
    </div>
    <a class="btn btn-outline-success" onclick="copy_to_clipboard()" >Copy access token</a>
</form>



<script>
function copy_to_clipboard() {
  // Get the text field
  var copyText = document.getElementById("vtk_token");

  // Select the text field
  copyText.select();
  copyText.setSelectionRange(0, 99999); // For mobile devices

   // Copy the text inside the text field
  navigator.clipboard.writeText(copyText.value);

  // Alert the copied text
  alert("Copied the text: " + copyText.value);
}
</script>


</body>
</html>
    """    
    return HTMLResponse(content=content)


@app.get("/downloadfile")
async def get_file(filename: str):
    current = Path()
    if not filename.endswith(".json"):
        if not filename.endswith(".vtk"):
            if not filename.endswith(".freq"):
                return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response


@app.get("/modal_analysis/uploadfile/")
async def create_modal_analysis_upload_files(YoungModulus: str, PoissonRatio:str, Density: str,
    ground_level: str, filename: str):
    
    #filename = 'uploaded_'+str(uuid.uuid4()) +'.vtk'
    #f = open(filename, 'wb+')
    #print(type(files[0].file) )
    #fileobj = files[0].file
    #shutil.copyfileobj(fileobj, f)
    #f.close()

    f=open(filename + ".condition","w")
    f.write(YoungModulus + "\n")
    f.write(PoissonRatio + "\n")
    f.write(Density + "\n")
    f.write(ground_level + "\n")
    f.close()
    os.system("./server_modal_analysis.out "+filename)
    content = """
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>

<body>



    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>


Download results of modal analysis ! <br>
- Eigen frequency (1st-3rd)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value="""+filename+".freq"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>

- Mode shape (1st)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=mode_1_"""+filename+"_mode_0001.vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


- Mode shape (2nd)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=mode_1_"""+filename+"_mode_0002.vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


- Mode shape (3rd)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=mode_1_"""+filename+"_mode_0003.vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


</body>
</html>
    """    
    return HTMLResponse(content=content)
