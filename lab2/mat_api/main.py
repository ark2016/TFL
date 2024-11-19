from fastapi import FastAPI
from app.routers.mat import mat

description = """
MAT_api
"""

app = FastAPI(
    title="MAT_api",
    description=description,
    summary="It is MAT_api!",
    version="0.0.1",
)


@app.get("/")
def root():
    return {"message": "Hello World"}


app.include_router(mat)