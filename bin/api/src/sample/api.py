from fastapi import FastAPI

app = FastAPI(
    title="plantFEM webAPI",
    description="This is a webAPI for plantFEM 21.10",
    version="21.10.0"
)

@app.get("/script")
def read_root():
    return {"message": "sample API"}

