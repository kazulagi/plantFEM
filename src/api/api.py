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

content_head = """
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantFEM-webAPI</title>
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
  </head>
"""

content_src ="""
  <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.15.0/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
  <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
"""

content_header_civil_gb_tp="""
<div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/culvert_creator">Culvert</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/civil">Solver</a>
      </nav>

      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>
"""

content_header_agri_1="""
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
"""

content_header_civil_1="""
<div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Pre-processing</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/bridge_creator">Bridge</a>
        <a class="btn btn btn-primary" href="/dam_creator">Dam</a>
        <a class="btn btn btn-primary" href="/culvert_creator">Culvert</a>
      </nav>
    </div>
"""

content_header_agri_crop="""
<div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Pre-processing</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-primary" href="/maize_creator">Maize</a>
        <a class="btn btn btn-primary" href="/soybean_creator">Soybean</a>
      </nav>
    </div>
"""

content_header_solver_1="""
<div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Solvers</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/static_analysis">Static analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-success" href="/modal_analysis">modal analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>Dynamic analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>Fluid analysis</a>
      </nav>
    </div>
"""

content_header_solver_agri="""
    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">Solvers</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>Static analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>modal analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>Dynamic analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>Fluid analysis</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn-outline-dark" disabled>Growth simulation</a>
      </nav>
    </div>
"""


content_header_top_page="""
<div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal"></h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>
"""

content_header_agri_crop_and_top_page="""
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
"""

content_header_top_page_and_go_back="""
    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 border-bottom shadow-sm">
      <h5 class="my-0 mr-md-auto font-weight-normal">plantFEM APIs</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="#" onclick="history.back(-1);return false;">Go back</a>
      </nav>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/">Top page</a>
      </nav>
    </div>
"""

@app.get("/")
async def main():
    content = """
<html>
  """+content_head+"""

<body>
    """+content_src+"""

     plantFEM API 
     <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-c-circle" viewBox="0 0 16 16">
      <path d="M1 8a7 7 0 1 0 14 0A7 7 0 0 0 1 8Zm15 0A8 8 0 1 1 0 8a8 8 0 0 1 16 0ZM8.146 4.992c-1.212 0-1.927.92-1.927 2.502v1.06c0 1.571.703 2.462 1.927 2.462.979 0 1.641-.586 1.729-1.418h1.295v.093c-.1 1.448-1.354 2.467-3.03 2.467-2.091 0-3.269-1.336-3.269-3.603V7.482c0-2.261 1.201-3.638 3.27-3.638 1.681 0 2.935 1.054 3.029 2.572v.088H9.875c-.088-.879-.768-1.512-1.729-1.512Z"/>
    </svg>2022
    Haruka Tomobe <br>
     v2022.10.13.1  THIS API IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED OR IMPLIED. 

    

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
  """+content_head+"""
<body>
    """+content_src+"""
    """+content_header_agri_crop+"""
    """+content_header_solver_agri+"""
    """+content_header_top_page+"""
</body>
</html>
    """
    return HTMLResponse(content=content)



@app.get("/civil")
async def main():
    content = """
<html>
  """+content_head+"""


<body>
    """+content_src+"""
    """+content_header_civil_1+"""
    """+content_header_solver_1+"""
    """+content_header_top_page+"""
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
          if not filename.endswith(".stl"):
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


maize_creator_createjsonfile_form="""

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

@app.get("/maize_creator/createjsonfile")
async def get_json_form():
    content = """
  """+content_head+"""

<body>
    """+content_src+"""
    """+content_header_agri_1+"""
    """+maize_creator_createjsonfile_form+"""
    """
    return HTMLResponse(content=content)


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
  """+content_head+"""

<body>
    """+content_header_agri_1+"""

Download your 3-D maize ! <br>
<form class="row g-3" action="/maize_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk"+ """>
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
  """+content_head+"""



<body>


    """+content_src+"""
    """+content_header_agri_1+"""

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
        <input name="files"  class="form-control" type="file" multiple required>
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
          if not filename.endswith(".stl"):
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
  """+content_head+"""

<body>



    """+content_header_agri_1+"""


Download your 3-D soybean ! <br>
<form class="row g-3" action="/soybean_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk"+ """>
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
  """+content_head+"""



<body>


    """+content_src+"""
    """+content_header_agri_crop_and_top_page+"""

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
        <input name="files"  class="form-control" type="file" multiple required>
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
          if not filename.endswith(".stl"):
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
    Divisions_y: str,Divisions_z: str,GirderWidth: str,
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
  """+content_head+"""

<body>

    """+content_header_civil_gb_tp+"""


<h2>Online bridge editor </h2><br>
<form class="form-group" action="/bridge_creator/download_new_json" method="get">

    <div class="col-auto">
        Number of piers for width-direction
        <input name="NumPiers_x" type="text"  class="form-control" value="2">
    </div>


    <div class="col-auto">
        Number of piers for length-direction
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
        Mesh divisions for length-direction ( < 6 is STRONGLY recommended)
        <input name="Divisions_x" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Mesh divisions for width-direction ( < 6 is STRONGLY recommended)
        <input name="Divisions_y" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Mesh divisions for height-direction ( < 6 is STRONGLY recommended)
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
  """+content_head+"""

<body>



    """+content_header_civil_gb_tp+"""

<form class="row g-3" action="/bridge_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Download 3-D bridge as .vtk">
    </div>
</form>

<form class="row g-3" action="/bridge_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk_000001.stl"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Download 3-D bridge as .stl">
    </div>
</form>

<iframe id="vs_iframe" src="https://www.viewstl.com/?embedded" style="border:0;margin:0;width:100%;height:100%;"></iframe>


</body>
</html>
    """    
    return HTMLResponse(content=content)

@app.get("/bridge_creator")
async def main():
    content = """
<html>
  """+content_head+"""



<body>


    """+content_src+"""
    """+content_header_civil_gb_tp+"""

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
        <input name="files"  class="form-control" type="file" multiple required>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-primary mb-2" value="Create">
    </div>
</form>

</body>
</html>
    """
    return HTMLResponse(content=content)


###### Dam ######


@app.get("/dam_creator/downloadfile")
async def get_file(filename: str):
    current = Path()
    if not filename.endswith(".json"):
        if not filename.endswith(".vtk"):
          if not filename.endswith(".stl"):
            return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response


@app.get("/dam_creator/download_new_json/")
async def download_new_json(height: str, width: str, length: str,
    depth: str, margin: str, top: str, angle_up: str,
    angle_down: str,division_v: str,division_h: str,
    refine_level_x:str,
    refine_level_y:str,
    refine_level_z:str):

    current = Path()
    filename = "dam.json"
    file_path = current / filename
    
    with open(file_path, 'r') as fcc_file:
        fcc_data = json.load(fcc_file)
        fcc_data["height"] = float(height)
        fcc_data["width"] = float(width)
        fcc_data["length"] = float(length)
        fcc_data["depth"] = float(depth)
        fcc_data["margin"] = float(margin)
        fcc_data["top"] = float(top)
        fcc_data["angles"][0]     = float(angle_up)
        fcc_data["angles"][1]     = float(angle_up)
        fcc_data["division_v"]     = int(division_v)
        fcc_data["division_h"]     = int(division_h)
        fcc_data["refine_level"][0]     = int(refine_level_x)
        fcc_data["refine_level"][1]     = int(refine_level_y)
        fcc_data["refine_level"][2]     = int(refine_level_z)
        
    new_filename = "download_"+str(uuid.uuid4())+"_dam.json"
    f = open(new_filename,"w")
    f.write((json.dumps(fcc_data, indent=4)))
    f.close()

    file_path = current / new_filename
    response = FileResponse(
        path=file_path,
        filename=f"{new_filename}"
        )
    return response

@app.get("/dam_creator/createjsonfile")
async def get_json_form():
    content = """
  """+content_head+"""

<body>

    """+content_header_civil_gb_tp+"""


<h2>Online dam editor </h2><br>
<form class="form-group" action="/dam_creator/download_new_json" method="get">

    <div class="col-auto">
        Height (m)
        <input name="height" type="text"  class="form-control" value="2.200">
    </div>


    <div class="col-auto">
        Width (m)
        <input name="width" type="text"  class="form-control" value="11.00">
    </div>

    <div class="col-auto">
        Length (m)
        <input name="length" type="text"  class="form-control" value="117.00">
    </div>
    
    <div class="col-auto">
        Depth of ground (m)
        <input name="depth" type="text"  class="form-control" value="30.00">
    </div>


    <div class="col-auto">
        Margin (m)
        <input name="margin" type="text"  class="form-control" value="50.00">
    </div>

    <div class="col-auto">
        Width of top (m)
        <input name="top" type="text"  class="form-control" value="3.00">
    </div>

    <div class="col-auto">
        Angles of upstream side (deg.)
        <input name="angle_up" type="text"  class="form-control" value="20.00">
    </div>

    <div class="col-auto">
        Angles of downstream side (deg.)
        <input name="angle_down" type="text"  class="form-control" value="20.00">
    </div>

    <div class="col-auto">
        Mesh division for vertial direction  ( < 6 is STRONGLY recommended)
        <input name="division_v" type="text"  class="form-control" value="4">
    </div>

    <div class="col-auto">
        Mesh division for horizontal direction ( < 6 is STRONGLY recommended)
        <input name="division_h" type="text"  class="form-control" value="4">
    </div>

    <div class="col-auto">
        Mesh refinement level for x-direction 
        <input name="refine_level_x" type="text"  class="form-control" value="3">
    </div>
    <div class="col-auto">
        Mesh refinement level for y-direction 
        <input name="refine_level_y" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Mesh refinement level for z-direction 
        <input name="refine_level_z" type="text"  class="form-control" value="3">
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

@app.post("/dam_creator/uploadfile/")
async def create_upload_files(files: List[UploadFile] = File(...),
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.json'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
        #os.system("./server_dam_creator.out "+filename)
        os.system("./server_dam_creator.out " + filename )
    content = """
<html>
  """+content_head+"""

<body>



    """+content_header_civil_gb_tp+"""

Download your 3-D bridge ! <br>
<form class="row g-3" action="/dam_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


<form class="row g-3" action="/dam_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk_000001.stl"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Download 3-D bridge as .stl">
    </div>
</form>

<iframe id="vs_iframe" src="https://www.viewstl.com/?embedded" style="border:0;margin:0;width:100%;height:100%;"></iframe>


</body>
</html>
    """    
    return HTMLResponse(content=content)

@app.get("/dam_creator")
async def main():
    content = """
<html>
  """+content_head+"""



<body>


    """+content_src+"""
    """+content_header_civil_gb_tp+"""

<div class="container-fluid">
    
    If you want an example of the *.json file, please type bridge.json and edit it! <br>
    <form class="row g-3" action="/dam_creator/downloadfile/" method="get">
        <div class="col-auto">
            <input name="filename" type="text" value="dam.json"  class="form-control">
        </div>
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Get template file">
        </div>
    </form>

    If you want to create a *.json file, please type the name and click the botton! <br>
    <form class="row g-3" action="/dam_creator/createjsonfile/" method="get">
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Create .json file">
        </div>
    </form>

 If you have an *.json file for bridge, please upload from here!<br>
<form class="row g-3" action="/dam_creator/uploadfile/" enctype="multipart/form-data" method="post">
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple required>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-primary mb-2" value="Create">
    </div>
</form>

</body>
</html>
    """
    return HTMLResponse(content=content)


##########################


### Box-culvert ####


###### Dam ######


@app.get("/culvert_creator/downloadfile")
async def get_file(filename: str):
    current = Path()
    if not filename.endswith(".json"):
        if not filename.endswith(".vtk"):
          if not filename.endswith(".stl"):
            return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response


@app.get("/culvert_creator/download_new_json/")
async def download_new_json(Height: str, Width: str, Length: str,
    Top_thickness: str, Side_thickness: str, Bottom_thickness: str,
    Edge_thickness:str, Divisions_1: str,Divisions_2: str,Divisions_3: str,
    Cut_angles_1: str,Cut_angles_2: str):

    current = Path()
    filename = "culvert.json"
    file_path = current / filename
    
    with open(file_path, 'r') as fcc_file:
        fcc_data = json.load(fcc_file)
        fcc_data["Height"] = float(Height)
        fcc_data["Width"] = float(Width)
        fcc_data["Length"] = float(Length)
        fcc_data["Top_thickness"] = float(Top_thickness)
        fcc_data["Side_thickness"] = float(Side_thickness)
        fcc_data["Edge_thickness"] = float(Edge_thickness)
        fcc_data["Bottom_thickness"] = float(Bottom_thickness)
        fcc_data["Divisions"][0]     = int(Divisions_1)
        fcc_data["Divisions"][1]     = int(Divisions_2) 
        fcc_data["Divisions"][2]     = int(Divisions_3)
        fcc_data["Cut_angles"][0]     = float(Cut_angles_1)
        fcc_data["Cut_angles"][1]     = float(Cut_angles_2)
        
    new_filename = "download_"+str(uuid.uuid4())+"_dam.json"
    f = open(new_filename,"w")
    f.write((json.dumps(fcc_data, indent=4)))
    f.close()

    file_path = current / new_filename
    response = FileResponse(
        path=file_path,
        filename=f"{new_filename}"
        )
    return response

@app.get("/culvert_creator/createjsonfile")
async def get_json_form():
    content = """
  """+content_head+"""

<body>

    """+content_header_civil_gb_tp+"""


<h2>Online culvert editor </h2><br>
<form class="form-group" action="/culvert_creator/download_new_json" method="get">
    (https://www.zenkoku-box.jp/about/standards.html)
    <div class="col-auto">
        Height, H0 (m)
        <input name="Height" type="text"  class="form-control" value="0.860">
    </div>


    <div class="col-auto">
        Width (B0) (m)
        <input name="Width" type="text"  class="form-control" value="0.860">
    </div>

    <div class="col-auto">
        Length, L (m)
        <input name="Length" type="text"  class="form-control" value="2.000">
    </div>
    
    <div class="col-auto">
        Top thickness, T1  (m)
        <input name="Top_thickness" type="text"  class="form-control" value="0.130">
    </div>


    <div class="col-auto">
        Bottom thickness. T2 (m)
        <input name="Bottom_thickness" type="text"  class="form-control" value="0.130">
    </div>

    <div class="col-auto">
        Side thickness, T3 (m)
        <input name="Side_thickness" type="text"  class="form-control" value="0.130">
    </div>

    <div class="col-auto">
        Edge thickness, C (m)
        <input name="Edge_thickness" type="text"  class="form-control" value="0.100">
    </div>

    <div class="col-auto">
        Mesh division for Length direction 
        <input name="Divisions_1" type="text"  class="form-control" value="2">
    </div>

    <div class="col-auto">
        Mesh division for Width direction 
        <input name="Divisions_2" type="text"  class="form-control" value="2">
    </div>

    <div class="col-auto">
        Mesh division for Height direction 
        <input name="Divisions_3" type="text"  class="form-control" value="2">
    </div>


    <div class="col-auto">
        Cut-off angle (1) (deg.)
        <input name="Cut_angles_1" type="text"  class="form-control" value="0.00">
    </div>

    <div class="col-auto">
        Cut-off angle (2) (deg.)
        <input name="Cut_angles_2" type="text"  class="form-control" value="0.00">
    </div>
    
    <div class="col-auto">
        Download from here!
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>

</form>
    """
    return HTMLResponse(content=content)

@app.post("/culvert_creator/uploadfile/")
async def create_upload_files(files: List[UploadFile] = File(...),
    ):
    for file in files:
        filename = 'uploaded_'+str(uuid.uuid4()) +'.json'
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
        #os.system("./server_culvert_creator.out "+filename)
        os.system("./server_culvert_creator.out " + filename )
    content = """
<html>
  """+content_head+"""

<body>



    """+content_header_civil_gb_tp+"""

Download your 3-D bridge ! <br>
<form class="row g-3" action="/culvert_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk"+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


<form class="row g-3" action="/culvert_creator/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+".vtk_000001.stl"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Download 3-D bridge as .stl">
    </div>
</form>

<iframe id="vs_iframe" src="https://www.viewstl.com/?embedded" style="border:0;margin:0;width:100%;height:100%;"></iframe>


</body>
</html>
    """    
    return HTMLResponse(content=content)

@app.get("/culvert_creator")
async def main():
    content = """
<html>
  """+content_head+"""



<body>


    """+content_src+"""
    """+content_header_civil_gb_tp+"""

<div class="container-fluid">
    
    If you want an example of the *.json file, please type bridge.json and edit it! <br>
    <form class="row g-3" action="/culvert_creator/downloadfile/" method="get">
        <div class="col-auto">
            <input name="filename" type="text" value="culvert.json"  class="form-control">
        </div>
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Get template file">
        </div>
    </form>

    If you want to create a *.json file, please type the name and click the botton! <br>
    <form class="row g-3" action="/culvert_creator/createjsonfile/" method="get">
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Create .json file">
        </div>
    </form>

 If you have an *.json file for bridge, please upload from here!<br>
<form class="row g-3" action="/culvert_creator/uploadfile/" enctype="multipart/form-data" method="post">
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple required>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-primary mb-2" value="Create">
    </div>
</form>

</body>
</html>
    """
    return HTMLResponse(content=content)





###################################################
###################################################
@app.get("/view_graph")
async def view_graph():
    content = """
    <body>

  """+content_head+"""

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



#### Static analysis ####
#### Static analysis ####
#### Static analysis ####
#### Static analysis ####

@app.get("/static_analysis")
async def static_analysis_info():
    content = """
<html>
  """+content_head+"""
<body>
    """+content_src+"""
    """+content_header_civil_gb_tp+"""

<div class="container-fluid">
    
    
<form class="form-group" action="/upload_vtk_file/" enctype="multipart/form-data" method="post">   
    [1] First, please upload VTK file and get access token. 
    
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple required>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-outline-success" value="Create access token">
    </div>

</form>


<form class="form-group" action="/static_analysis/uploadfile/" enctype="multipart/form-data" method="get">
    [2] Second, please upload conditions and access token. 
    
    <div class="col-auto">
        Set Young's modulus (kPa) <br>
        <input name="YoungModulus" class="form-control" type="text" value="10000000.00">
    </div>

    <div class="col-auto">
        Set Poisson's ratio <br>
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
        Fixed boundary (x-min) (m) <br>
        <input name="fix_boundary_xmin"  class="form-control" type="text" value="-100000.0">
    </div>

    <div class="col-auto">
        Fixed boundary (x-max) (m) <br>
        <input name="fix_boundary_xmax"  class="form-control" type="text" value="100000.0">
    </div>

    <div class="col-auto">
        Fixed boundary (y-min) (m) <br>
        <input name="fix_boundary_ymin"  class="form-control" type="text" value="-100000.0">
    </div>

    <div class="col-auto">
        Fixed boundary (y-max) (m) <br>
        <input name="fix_boundary_ymax"  class="form-control" type="text" value="100000.0">
    </div>


    <div class="col-auto">
        Set VTK file access token <br>
        <input name="filename"  class="form-control" type="text" value="" required>
    </div>


    <div class="col-auto">
        This process can take from a few minutes to several hours.
        <input type="submit"  class="btn btn-outline-success" value="Run static analysis"/>
    </div>


</form>



</body>
</html>
    """
    return HTMLResponse(content=content)


@app.get("/static_analysis/uploadfile/")
async def create_modal_analysis_upload_files(YoungModulus: str, PoissonRatio:str, Density: str,
    ground_level: str, fix_boundary_xmin:str, fix_boundary_xmax:str,
    fix_boundary_ymin:str, fix_boundary_ymax:str,filename: str):
    

    f=open(filename + ".condition","w")
    f.write(YoungModulus + "\n")
    f.write(PoissonRatio + "\n")
    f.write(Density + "\n")
    f.write(ground_level + "\n")
    f.write(fix_boundary_xmin + "\n")
    f.write(fix_boundary_xmax + "\n")
    f.write(fix_boundary_ymin + "\n")
    f.write(fix_boundary_ymax + "\n")
    
    f.close()
    #os.system("./server_static_analysis.out "+filename)
    Popen_obj = subprocess.Popen(["./server_static_analysis.out",filename])
    content = """
<html>
  """+content_head+"""

<body>



"""+content_header_top_page_and_go_back+"""

Download results of static analysis ! <br>

- Deformation & mean stress
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_I1_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>

- Deformation & s(1,1)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_11_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


- Deformation & s(2,2)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_22_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


- Deformation & s(3,3)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_33_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>



- Deformation & s(1,2)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_12_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


- Deformation & s(1,3)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_13_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


- Deformation & s(2,3)
<form class="row g-3" action="/downloadfile" method="get">
    <div class="col-auto">
        <input name="filename" type="text"  class="form-control" value=static_23_"""+filename+ """>
    </div>
    <div class="col-auto">
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>
</form>


</body>
</html>
    """    
    return HTMLResponse(content=content)




#### Modal analysis ####
#### Modal analysis ####
#### Modal analysis ####
#### Modal analysis ####


@app.get("/modal_analysis")
async def modal_analysis_info():
    content = """
<html>
  """+content_head+"""
<body>
    """+content_src+"""
    """+content_header_civil_gb_tp+"""

<div class="container-fluid">
    
    
<form class="form-group" action="/upload_vtk_file/" enctype="multipart/form-data" method="post">   
    [1] First, please upload VTK file and get access token. 
    
    <div class="col-auto">
        <input name="files"  class="form-control" type="file" multiple required>
    </div>
    <div class="col-auto">
        <input type="submit"  class="btn btn-outline-success" value="Create access token">
    </div>

</form>


<form class="form-group" action="/modal_analysis/uploadfile/" enctype="multipart/form-data" method="get">
    [2] Second, please upload conditions and access token. 
    
    <div class="col-auto">
        Set Young's modulus (kPa) <br>
        <input name="YoungModulus" class="form-control" type="text" value="10000000.00">
    </div>

    <div class="col-auto">
        Set Poisson's ratio  <br>
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
        Fixed boundary (x-min) (m) <br>
        <input name="fix_boundary_xmin"  class="form-control" type="text" value="-100000.0">
    </div>

    <div class="col-auto">
        Fixed boundary (x-max) (m) <br>
        <input name="fix_boundary_xmax"  class="form-control" type="text" value="100000.0">
    </div>

    <div class="col-auto">
        Fixed boundary (y-min) (m) <br>
        <input name="fix_boundary_ymin"  class="form-control" type="text" value="-100000.0">
    </div>

    <div class="col-auto">
        Fixed boundary (y-max) (m) <br>
        <input name="fix_boundary_ymax"  class="form-control" type="text" value="100000.0">
    </div>


    <div class="col-auto">
        Set VTK file access token <br>
        <input name="filename"  class="form-control" type="text" value="" required>
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
  """+content_head+"""

<body>

"""+content_header_top_page_and_go_back+"""

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
              if not filename.endswith(".stl"):
                return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response


@app.get("/modal_analysis/uploadfile/")
async def create_modal_analysis_upload_files(YoungModulus: str, PoissonRatio:str, Density: str,
    ground_level: str, fix_boundary_xmin:str, fix_boundary_xmax:str,
    fix_boundary_ymin:str, fix_boundary_ymax:str,filename: str):
    
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
    f.write(fix_boundary_xmin + "\n")
    f.write(fix_boundary_xmax + "\n")
    f.write(fix_boundary_ymin + "\n")
    f.write(fix_boundary_ymax + "\n")
    
    f.close()
    os.system("./server_modal_analysis.out "+filename)
    content = """
<html>
  """+content_head+"""

<body>



"""+content_header_top_page_and_go_back+"""

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



#from fastapi.staticfiles import StaticFiles
#app.mount("/static", StaticFiles(directory="static"), name="static")

### 3D viewer ###

@app.get("/viewer2d/")
async def two_d_renderer(filename:str):
  content="""

"""
  return HTMLResponse(content=content)


@app.get("/test3d/")
async def create_modal_analysis_upload_files():
  content="""
  <html>
  
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="user-scalable=no" />
    <meta name="viewport" content="width=device-width, initial-scale=1"/>
    <title>plantfem viewer</title>
    <link rel="stylesheet" href="https://plantfem.org/three_style.css">

    <!-- using libraries  -->
    <script src="https://threejs.org/build/three.js"></script> 
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script>
    <script src="https://plantfem.org/js/controls/OrbitControls.js"></script> 
    <!--
    <script src="https://plantfem.org/js/loaders/STLLoader.js"></script>
    -->
    <script src="https://threejs.org/examples/js/loaders/STLLoader.js"></script>
        
    <script src="https://plantfem.org/dat.gui/build/dat.gui.js"></script>
    <script src="https://plantfem.org/stats.js/build/stats.min.js"></script>
    <script src="https://plantfem.org/showchart.js?date=2021030v2"></script>
    <script src="https://plantfem.org/showBiomass.js?date=20210307v2"></script>
    <script src="https://plantfem.org/virtualjoystick.js/virtualjoystick.js"></script>
</head>

<body>
    <script type="text/javascript">
        
        function init() {
            // create botton
            
            var stats = new Stats();
            stats.showPanel(0);
              // Align top-left
            stats.domElement.style.position = 'absolute';
            stats.domElement.style.left = '0px';
            stats.domElement.style.top = '0px';
            
            document.body.appendChild( stats.dom );

            
            var charts = new Charts();
            charts.showPanel(0);
              // Align top-left
            charts.domElement.style.position = 'absolute';
            charts.domElement.style.left = '0px';
            charts.domElement.style.top = '60px';


            var charts2 = new Biomass();
            charts2.showPanel(0);
              // Align top-left
            charts2.domElement.style.position = 'absolute';
            charts2.domElement.style.left = '0px';
            charts2.domElement.style.top = '120px';


            
            document.body.appendChild( charts.dom );
            document.body.appendChild( charts2.dom );


            THREE.Cache.enabled = false;
            var joystick_right = new VirtualJoystick({
                mouseSupport: true,
                stationaryBase: true,
                baseX: window.innerWidth - 100,
                baseY: window.innerHeight - 100,
                limitStickTravel: true,
                stickRadius: 50
             });
             joystick_right.addEventListener('touchStartValidation', function(event){
                var touch	= event.changedTouches[0];
                event.preventDefault();
                if( touch.pageY < window.innerHeight/2   )	return false;
    
                if( touch.pageX < window.innerWidth-150 )	return false;
    
                return true;
            });
//    
    
            // create controller
            var joystick_left = new VirtualJoystick({
                mouseSupport: true,
                stationaryBase: true,
                baseX: 100,
                baseY: window.innerHeight - 100,
                limitStickTravel: true,
                stickRadius: 50
             });
             joystick_left.addEventListener('touchStartValidation', function(event){
                var touch	= event.changedTouches[0];
                event.preventDefault();
                
                if( touch.pageY < window.innerHeight/2   )	return false;
                if( touch.pageX >= 150 )	return false;

                return true;
            });
    
        var matloader = new THREE.TextureLoader();

        
        var root_material = new THREE.MeshLambertMaterial({color: 0xF3E495});
        var stem_material = new THREE.MeshLambertMaterial({color: 0xCBC547});
        var matloader = new THREE.TextureLoader();
        
        //var grass_material = new THREE.MeshLambertMaterial({color: 0xcccccc});
    
        var tgc_material = new THREE.MeshBasicMaterial({
            //map: texture, // テクスチャーを指定
            color: 0xa9ceec, // 色
            transparent: true, // 透明の表示許可
            blending: THREE.AdditiveBlending, // ブレンドモード
            side: THREE.DoubleSide, // 表裏の表示設定
            depthWrite: false // デプスバッファへの書き込み可否
        });
        
        var renderer = new THREE.WebGLRenderer();
        
        renderer.setSize( window.innerWidth, window.innerHeight );
        renderer.setClearColor(new THREE.Color(0xEEEEEE) );
        //renderer.setClearColorHex( 0x95F3F2, 1 );
        renderer.shadowMap.enabled =  true;
        document.body.appendChild( renderer.domElement );
        
        var scene = new THREE.Scene();
        var camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );
        camera.position.set(0, -3, 5 );
        const tloader = new THREE.TextureLoader();
        scene.background = tloader.load( 'https://plantfem.org/image.jpg' );
        
        // Objects    

        // Soil
        var soil = new THREE.PlaneGeometry(83, 83);
        

        let textuerLoader = new THREE.TextureLoader();
    
        // 3DS形式のモデルデータを読み込む
        var loader = new THREE.STLLoader();
        var light = new THREE.PointLight(0xFFFFFF, 3)
        light.position.set(2, 2, 30);
        light.castShadow = true;
        scene.add(light);

    
        // create soybean field
        var group = new THREE.Object3D();
    
    
        scene.add(group)
    
        var rot =0;
        stem_path = "uploaded_f928f6c0-371a-4db0-b1d3-2ae02fcd7306.json.vtk_000001.stl"
        root_path = "uploaded_f928f6c0-371a-4db0-b1d3-2ae02fcd7306.json.vtk_000001.stl"
        // load plants

        
                loader.load( stem_path,  function ( geometry1 ) {
                        var stl_geo_stem = new THREE.Mesh( geometry1, stem_material )
                        stl_geo_stem.rotation.x = 0//Math.PI / 180 * 90
                        stl_geo_stem.rotation.y = 0//Math.PI / 180 * 90
                        stl_geo_stem.rotation.z = Math.PI * (i+j+1);
                        stl_geo_stem.position.y = intra_row*(i)-22;
                        stl_geo_stem.position.x = inter_row*(j)+4;
                        stl_geo_stem.castShadow = true;
                        stl_geo_stem.receiveShadow = true;
                        group.add( stl_geo_stem);
                    
                });

                loader.load( root_path,  function ( geometry3 ) {
                    
                        var stl_geo_root = new THREE.Mesh( geometry3, root_material )
                        stl_geo_root.rotation.x = 0//Math.PI / 180 * 90
                        stl_geo_root.rotation.y = 0//Math.PI / 180 * 90
                        stl_geo_root.rotation.z = Math.PI * (i+j+1);
                        stl_geo_root.position.y = intra_row*(i)-22;
                        stl_geo_root.position.x = inter_row*(j)+4;
                        stl_geo_root.castShadow = true;
                        stl_geo_root.receiveShadow = true;
                        group.add( stl_geo_root);
                    
                });

    //ここまでやったが，結局.stlを高速化するほうがよいのでは？
        var lookat = new THREE.Vector3(0, 0, 0);
    
        var lookat_x = 0.0;
        var lookat_y = 1000.0;
        var lookat_z = 0.0;
        camera.lookAt(new THREE.Vector3(lookat_x,lookat_y,lookat_z));
        
        camera.position.y=-30.0;
        camera.position.x=5.0;
        camera.position.z=0.6;

        var x = getParam("x",url)
        camera.position.x=x;
        if(x==null){
            camera.position.x=5.0;
        };

        var y = getParam("y",url)
        camera.position.y=y;
        if(y==null){
            camera.position.y=-30.0;
        };
        var z = getParam("z",url)
        camera.position.z=z;
        if(z==null){
            camera.position.z=0.6;
        };

        var angle = getParam("angle",url)
        controls.look_down=angle;
        if(angle==null){
            controls.look_down=0;
        };
        camera.rotateX(-0.020*controls.look_down);
        controls.look_down = 0;

        var vx_n = 0.0;
        var vz_n = 0.0;
        var vx = 0.0;
        var vy = 0.0;
        var vz = 0.0;
        var vrot = 0.0;
        var rot=0;
        var nx=0.0;
        var ny=1.0;
    
    
        var axes = new THREE.AxesHelper(25);
        scene.add(axes);
    
        var lookAtVector = new THREE.Vector3(0,0, 0);
    

        tick();
    
          // 毎フレーム時に実行されるループイベントです
        function tick() {
            //var radian = rot * Math.PI / 180;


            stats.begin();
            charts.begin();
            charts2.begin();
            //stats.update();
            charts.update(20,200);
            charts2.update(20,200);

            requestAnimationFrame(tick);
            
            
            // keyboard operation
            // movement - please calibrate these values
            var xSpeed = 0.00005;
            var ySpeed = 0.00005;
    
            camera.getWorldDirection(lookAtVector );
            //console.log(lookAtVector.x,lookAtVector.y  );
    
            document.addEventListener("keydown", onDocumentKeyDown, false);
            function onDocumentKeyDown(event) {
                var keyCode = event.which;
                if (keyCode == 87) {
                    camera.position.z += ySpeed;
                } else if (keyCode == 83) {
                    camera.position.z -= ySpeed;
                } else if (keyCode == 65) {
                    camera.rotateY(xSpeed)/5.0;
                } else if (keyCode == 68) {
                    camera.rotateY(-xSpeed)/5.0;
                } else if (keyCode == 243) {
                    camera.position.set(0, 0, 0);
                } else if (keyCode == 73) {
                    camera.position.x += 2.0*ySpeed*lookAtVector.x;
                    camera.position.y += 2.0*ySpeed*lookAtVector.y;
                } else if (keyCode == 75) {
                    camera.position.x -= 2.0*ySpeed*lookAtVector.x;
                    camera.position.y -= 2.0*ySpeed*lookAtVector.y;
                } else if (keyCode == 74) {
                    camera.position.x -= ySpeed*lookAtVector.y;
                    camera.position.y += ySpeed*lookAtVector.x;
                } else if (keyCode == 76) {
                    camera.position.x += ySpeed*lookAtVector.y;
                    camera.position.y -= ySpeed*lookAtVector.x;
                } else if (keyCode == 82) {
                    camera.position.set(0, -2, 0);
                    camera.lookAt(0,1000,0);
                }
            };
    
    
            vx = joystick_right.deltaX();
            vy = joystick_right.deltaY();
    
            vz = joystick_left.deltaY();
            vrot = joystick_left.deltaX();
    
            if(Math.abs(vx) > 35.0 || Math.abs(vy) > 35.0){
                vx = 0.0;
                vy = 0.0;
            }
            if(Math.abs(vz) > 35.0 || Math.abs(vrot) > 35.0){
                vz = 0.0;
                vrot = 0.0;
            }
    
            if(Math.abs(vz) > Math.abs(vrot)) {
                vrot = 0.0;
            }else{
                vz = 0.0;
            }        
            
            vrot = Math.PI * vrot/180;
            rot +=vrot;
            
            if(Math.abs(vx) > Math.abs(vy)) {
                vy = 0;
                if(vx > 0.0) {
                    camera.position.x += 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.y;
                    camera.position.y -= 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.x;
                } else{
                    camera.position.x -= 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.y;
                    camera.position.y += 0.0001*Math.abs(vx)*Math.abs(vx)*lookAtVector.x;
                }
            }else{
                vx = 0;
                camera.position.x -= 0.0001*vy*Math.abs(vy)*lookAtVector.x;
                camera.position.y -= 0.0001*vy*Math.abs(vy)*lookAtVector.y;
            };
    
            camera.position.z += -0.0001*vz*Math.abs(vz);
            camera.rotateY(-0.02*vrot*Math.abs(vrot));
            // change angle
            camera.rotateX(0.0001*controls.look_up);
            camera.rotateX(-0.0001*controls.look_down);
    
            a = lookAtVector.x;
            b = lookAtVector.y;

    
            if (camera.position.z < 0.5) { 
                camera.position.z = 0.5;
            }
    
            if (camera.position.x > 150.0) { 
                camera.position.x = 150.0;
            }
    
            if (camera.position.x < -150.0) { 
                camera.position.x = -150.0;
            }
    
    
            if (camera.position.y > 150.0) { 
                camera.position.y = 150.0;
            }
    
            if (camera.position.y < -150.0) { 
                camera.position.y = -150.0;
            }
    
            
                    
            renderer.render(scene, camera); // レンダリング
    
            light.position.x = controls.light_x;
            light.position.y = controls.light_y;
            light.position.z = controls.light_z;
            
            light.power = controls.strength;
            soilmesh.position.z = controls.ground_level;
            
            
            grassmesh.position.z = soilmesh.position.z-0.01;
    
            stats.end();
            charts.end();
            charts2.end();
          }
        }

    </script>
</body>

  </html>
"""
  return HTMLResponse(content=content)

forming = """
<div class="col-auto">
    leaf_num
    <input name="leaf_num_1" type="text"  class="form-control" value="1">
</div>

"""

#def forming(i):
#    forms = """
#<div class="col-auto">
#    leaf_num
#    <input name='leaf_num"""+str(i)+"""' type="text"  class="form-control" value="1">
#</div>
#"""
#    return forms    

@app.get("/base/", response_class=HTMLResponse)
async def read_items(leaf_num_1:str):
    forms=""
    for i in range(int(leaf_num_1) ):
        forms = forms + forming
    content = """
    <html>
        <head>
            <title>Some HTML in here</title>
        </head>
        <body>
        <form class="form-group" action="/base/" method="get">
    """+forms+ """
    <div class="col-auto">
        Download from here!
        <input type="submit" class="btn btn-primary mb-2" value="Download">
    </div>

</form>

        </body>
    </html>
    """
    return HTMLResponse(content=content)

