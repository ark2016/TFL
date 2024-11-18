from fastapi import FastAPI
import subprocess

from automaton import read_parametrs, convert_Automat_to_dict, save_to_file
from generate import AutomatGenerator

app = FastAPI()

process = None


import subprocess
import os



@app.get("/")
def root():
    return {"message": "Hello World"}

@app.get("/generate")
def generate():
    maxSize, maxNesting = read_parametrs(filename="parameters.txt")
    automat_generator = AutomatGenerator("lab2", maxSize=maxSize, maxNesting=maxNesting)
    automat = automat_generator.gen_random_program_Automat()
    automat_dict = convert_Automat_to_dict(automat)
    save_to_file("automaton.txt", automat_dict)
    return {"automat": "Success automat generation!"}

@app.get("/start_mat")
def start_mat():
    global process
    if process is not None:
        process.terminate()
    else:
        process = subprocess.Popen(
            ['./mat_linux'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            bufsize=1
        )
    file_path = "automaton.txt"
    process.stdin.write(f"{file_path}\n")
    process.stdin.flush()

    return {"mat": "Success start mat!"}


@app.post("/check_string")
def check_string(test_string: str):
    global process
    process.stdin.write(f"{test_string}\n")
    process.stdin.flush()
    result = process.stdout.readline().strip()
    print(result)
    if result == '1':
        return {"Accepted": True}
    return {"Accepted": False}

@app.post("/check_equivalence")
def check_equiv(eq_table: str):
    process.stdin.write(f"{eq_table}\n")
    process.stdin.flush()
    result = process.stdout.readline().strip()
    print(result)
    if result == '1':
        return {"equivalence": True}
    return {"equivalence": False}

@app.get("/stop_mat")
def stop_mat():
    if process is not None:
        process.terminate()


# python3 -m venv venv
#