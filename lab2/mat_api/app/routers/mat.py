from fastapi import APIRouter
from pyformlang.finite_automaton import DeterministicFiniteAutomaton
from starlette.responses import FileResponse

from ..mat.automaton import save_to_file, read_parametrs, convert_Automat_to_dict, dfa_to_dot_str, visualize_dfa
from ..mat.generateV2 import AutomatGenerator
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
    process = start_haskel_mat(option=1)

    process.stdin.write(f"{test_string}\n")
    process.stdin.flush()
    result = process.stdout.readline().strip()

    process.terminate()
    return {"result": result}


@mat.post("/check_equivalence")
def check_equiv(eq_table: str):
    process = start_haskel_mat(option=2)

    process.stdin.write(f"{eq_table}\n")
    process.stdin.flush()
    result = process.stdout.readline().strip()

    process.terminate()
    if result == "":
        return {"equivalence": True}
    return {"equivalence": result}


@mat.get("/dot_automat")
def dot_automat(type: str):
    automata = get_automat_by_type(type)
    dot_str = dfa_to_dot_str(automata)
    return {"type": type, "dot": dot_str}


@mat.get("/file", response_class=FileResponse)
def generate_dot():
    path = "output/prog"
    visualize_dfa(automat, path)
    return "output/prog"
@mat.get("/image", response_class=FileResponse)
def generate_image1(type: str):
    automata = get_automat_by_type(type)
    path = "output/prog"
    visualize_dfa(automata, path)
    return "output/prog.pdf"


def get_automat_by_type(type: str):
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
    return automata
