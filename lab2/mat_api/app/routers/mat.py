from fastapi import APIRouter
from pyformlang.finite_automaton import DeterministicFiniteAutomaton

from ..mat.automaton import save_to_file, read_parametrs, convert_Automat_to_dict, automat_to_dot
from ..mat.generate import AutomatGenerator
from ..utils import start_haskel_mat

mat = APIRouter(
    tags=["Mat"]
)

automat: DeterministicFiniteAutomaton = DeterministicFiniteAutomaton()
maxSize, maxNesting = read_parametrs(filename="app/mat/parameters.txt")
automat_generator = AutomatGenerator("lab2", maxSize=maxSize, maxNesting=maxNesting)


@mat.get("/generate")
def generate():
    automat = automat_generator.gen_random_program_Automat()
    automat_dict = convert_Automat_to_dict(automat)
    save_to_file("app/output/automaton.txt", automat_dict)
    return {"automat": "Success automat generation!"}


@mat.post("/check_string")
def check_string(test_string: str):
    process = start_haskel_mat()

    process.stdin.write(f"{test_string}\n")
    process.stdin.flush()
    result = process.stdout.readline().strip()

    process.terminate()

    print(result)
    if result == '1':
        return {"Accepted": True}
    return {"Accepted": False}


@mat.post("/check_equivalence")
def check_equiv(eq_table: str):
    process = start_haskel_mat()

    process.stdin.write(f"{eq_table}\n")
    process.stdin.flush()
    result = process.stdout.readline().strip()

    process.terminate()
    print(result)
    if result == '1':
        return {"equivalence": True}
    return {"equivalence": False}


@mat.get("/dot_automat")
def dot_automat():
    return {"dot": automat_to_dot(automat)}
