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

################################################
######### Civil Engineering ####################
################################################


###### plantSPAC ######

## get result file
@app.get("/plantSPAC/downloadfile")
async def get_file(filename: str):
    current = Path()
    if not filename.endswith(".json"):
        if not filename.endswith(".csv"):
            if not filename.endswith(".txt"):
                return {"status": "error"}
    file_path = current / filename
    
    response = FileResponse(
        path=file_path,
        filename=f"download_{filename}"
        )
    
    return response


@app.get("/plantSPAC/download_new_json/")
async def download_new_json(
    sampling_hz:str,
    radius:str,
    num_logger:str,
    fft_size:str,
    maximum_phase_velocity:str,
    maximum_itr:str,
    num_smoothing:str):


    current = Path()
    filename = "plantSPAC.json"
    file_path = current / filename
    new_csv_filename = "download_"+str(uuid.uuid4())+".csv"
    new_filename = new_csv_filename +".condition.json"

    with open(file_path, 'r') as fcc_file:
        fcc_data = json.load(fcc_file)
        fcc_data["sampling_hz"] = int(sampling_hz)
        fcc_data["radius"] = float(radius)
        fcc_data["num_logger"] = int(num_logger)
        fcc_data["fft_size"] = int(fft_size)
        fcc_data["maximum_phase_velocity"] = float(maximum_phase_velocity)
        fcc_data["maximum_itr"] = int(maximum_itr)
        fcc_data["num_smoothing"]     = int(num_smoothing)
        fcc_data["filename"] = new_csv_filename
        
    
    f = open(new_filename,"w")
    f.write((json.dumps(fcc_data, indent=4)))
    f.close()

    file_path = current / new_filename
    response = FileResponse(
        path=file_path,
        filename=f"{new_filename}"
        )
    
    return response

@app.get("/plantSPAC/createjsonfile")
async def get_json_form():
    content = """
  """+content_head+"""

<body>

    """+content_header_civil_gb_tp+"""


<h2>Online plantSPAC editor </h2><br>
<form class="form-group" action="/plantSPAC/download_new_json" method="get">

    <div class="col-auto">
        Sampling rate (Hz)
        <input name="sampling_hz" type="text"  class="form-control" value="250">
    </div>


    <div class="col-auto">
        Radius of array (m)
        <input name="radius" type="text"  class="form-control" value="3">
    </div>

    <div class="col-auto">
        Number of logger
        <input name="num_logger" type="text"  class="form-control" value="4">
    </div>
    
    <div class="col-auto">
        Size of segment for FFT (2^n) 
        <input name="fft_size" type="text"  class="form-control" value="4096">
    </div>


    <div class="col-auto">
        Maximum phase velocity (m/s) for SPAC
        <input name="maximum_phase_velocity" type="text"  class="form-control" value="3000.0">
    </div>

    <div class="col-auto">
        Number of grid search for SPAC
        <input name="maximum_itr" type="text"  class="form-control" value="100000">
    </div>

    <div class="col-auto">
        Number of smoothing (times)
        <input name="num_smoothing" type="text"  class="form-control" value="10">
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

@app.post("/plantSPAC/runSPAC/")
async def create_upload_files(files: List[UploadFile] = File(...),
    ):
    for file in files:
        filename = file.filename
        f = open(filename, 'wb+')
        print(type(file.file) )
        fileobj = file.file
        shutil.copyfileobj(fileobj, f)
        f.close()
        #os.system("./server_plantSPAC.out "+filename)
        sts = subprocess.Popen("./server_plantSPAC.out " + filename, shell=True)
    content = """
<html>
  """+content_head+"""

<body>



    """+content_header_civil_gb_tp+"""
    It may take a few minutes please wait...
<form class="row g-3" action="/plantSPAC/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+"_HoverV-spectra_EW.csv"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Download H/V spectra for EW/UD">
    </div>
</form>

<form class="row g-3" action="/plantSPAC/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+"_HoverV-spectra_NS.csv"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Download H/V spectra for NS/UD">
    </div>
</form>

<form class="row g-3" action="/plantSPAC/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+"_SPAC_COEFF.csv"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="SPAC coefficient">
    </div>
</form>

<form class="row g-3" action="/plantSPAC/downloadfile/" method="get">
    <div class="col-auto">
        <input type="hidden" name="filename" value="""+filename+"_Rayl-Dispersion.csv"+ """>
        <input type="submit" class="btn btn-primary mb-2" value="Dispersion curve of Rayleigh wave">
    </div>
</form>

</body>
</html>
    """    
    return HTMLResponse(content=content)

@app.get("/plantSPAC")
async def main():
    content = """
<html>
  """+content_head+"""



<body>


    """+content_src+"""
    """+content_header_civil_gb_tp+"""

<div class="container-fluid">
    
    Step #1<br>
    Create JSON file and download <br>
    <form class="row g-3" action="/plantSPAC/createjsonfile/" method="get">
        <div class="col-auto">
            <input type="submit" class="btn btn-primary mb-3" value="Create .json file">
        </div>
    </form>


    Step #2 <br>
    (Locally) Open JSON and check filename, then rename your CSV-formatted waveform data. <br>
    <br>   

    Step #3 <br>
    Upload your CSV-formatted waveform data for all channels.<br>   
    Note that the .CSV has 3*N+1 columns, <br>   
    with time(s), UD(ch1), EW(ch1), NS(ch1),UD(ch2), EW(ch2), NS(ch2),... etc.
    <form class="row g-3" action="/plantSPAC/runSPAC/" enctype="multipart/form-data" method="post">
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



content_head = """
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="description" content="">
    <title>plantSPAC</title>
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
      <h5 class="my-0 mr-md-auto font-weight-normal">plantSPAC</h5>
      <nav class="my-2 my-md-0 mr-md-3">
        <a class="btn btn btn-secondary" href="/plantSPAC">Top page</a>
      </nav>
    </div>
"""



