from pyformlang.finite_automaton import DeterministicFiniteAutomaton
from generate import *


def convert_Automat_to_dict(automat: DeterministicFiniteAutomaton):
    automat_dict = {
        "states": automat.states,
        "albhabet": automat.symbols,
        "transictions": automat.to_fst().transitions,
        "start_states": automat.start_states,
        "final_states": automat.final_states
    }

    return automat_dict


def save_to_file(filename: str, automat_dict: dict):
    with open(filename, "w") as file:
        stroka = "Automaton " + automat_dict.__str__()
        file.write(stroka)


generator = AutomatGenerator("Automat")
sentence = generator.gen_random_expression_Automat()
save_to_file("info.txt", convert_Automat_to_dict(sentence))
