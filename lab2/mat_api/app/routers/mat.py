from fastapi import APIRouter
from fastapi.responses import FileResponse
from pyformlang.finite_automaton import DeterministicFiniteAutomaton

from ..mat.automaton import save_to_file, read_parametrs, convert_Automat_to_dict, automat_to_dot, visualize_dfa, \
    dfa_to_dot_str
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
    global automat
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
    if result == '1':
        return {"equivalence": True}
    return {"equivalence": False}


@mat.get("/dot_automat")
def dot_automat(type: str):
    if type == "eol":
        automata = automat_generator.eol_Automat
    elif type == "blank":
        automata = automat_generator.blank_Automat
    elif type == "equal":
        automata = automat_generator.equal_Automat
    elif type == "const":
        automata = automat_generator.const_Automat
    elif type == "var":
        automata = automat_generator.var_Automaton
    elif type == "sep":
        automata = automat_generator.sep_Automat
    elif type == "lbr1":
        automata = automat_generator.lbr1_Automat
    elif type == "rbr1":
        automata = automat_generator.rbr1_Automat
    elif type == "lbr2":
        automata = automat_generator.lbr2_Automat
    elif type == "rbr2":
        automata = automat_generator.rbr2_Automat
    elif type == "lbr3":
        automata = automat_generator.lbr3_Automat
    elif type == "rbr3":
        automata = automat_generator.rbr3_Automat
    else:
        automata = automat

    dot_str = dfa_to_dot_str(automata)
    return {"type": type,"dot": dot_str}
