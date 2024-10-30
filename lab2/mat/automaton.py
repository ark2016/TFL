from pyformlang.finite_automaton import DeterministicFiniteAutomaton
from generate import *


def convert_Automat_to_dict(automat: DeterministicFiniteAutomaton):
    automat_dict = {
        "states": automat.states,
        "albhabet": automat.symbols,
        "transictions": automat.to_fst().transitions,
        "start_state": automat.start_state,
        "final_states": automat.final_states
    }

    return automat_dict


def convert_to_haskell_str(automat_dict: dict):
    states_srt = list(automat_dict["states"])
    states = []
    for elem in states_srt:
        elem = str(elem)
        state = int(elem[1:])
        states.append(state)
    alphabet = list(automat_dict["albhabet"])
    alphabet_str = ""
    for el in alphabet:
        alphabet_str += str(el)
    start_state = int(str(automat_dict["start_state"])[1:])
    final_states_str = list(automat_dict["final_states"])
    final_states = []
    for elem in final_states_str:
        elem = str(elem)
        state = int(elem[1:])
        final_states.append(state)
    transuctions_dict: dict = automat_dict["transictions"]
    transuctions_list = []

    for item in transuctions_dict.items():
        transuctions_list.append(((int(item[0][0][1:]), item[0][1]), int(item[1][0][0][1:])))

    haskel_str = "Automaton {states = " + f"{states}, alphabet = \"{alphabet_str}\", transitions = fromList {transuctions_list}, initialState = {start_state}, acceptingStates = {final_states}" + "}"

    return haskel_str


def save_to_file(filename: str, automat_dict: dict):
    with open(filename, "w") as file:
        # stroka = "Automaton " + automat_dict.__str__()
        stroka = convert_to_haskell_str(automat_dict)#.replace("E", "").replace("\'\'", "\"\"")
        file.write(stroka)


def automat_to_dot(self):
    dot = Digraph(comment='DFA')

    # Добавление состояний
    for state in self.states:
        if state in self.final_states:
            dot.node(state, state, shape='doublecircle')  # Конечное состояние
        else:
            dot.node(state, state, shape='circle')  # Обычное состояние

    # Добавление переходов
    for state, transitions in self.transition.items():
        for symbol, next_state in transitions.items():
            dot.edge(state, next_state, label=symbol)

    return dot


def convertToImage(self, filename: str):
    dfa_graph = self.visualize()
    dfa_graph.render(filename, format='png', cleanup=True)
    dfa_graph.view()


def get_automat_info(automat):
    info = ""
    info += "Starts: " + automat.start_states + "\n"
    info += "Finals: ", automat.final_states
    return info


def visualize_dfa(dfa, name):
    dot = Digraph(comment='The Round Table')

    # Добавляем состояния
    for state in dfa.states:
        dot.node(str(state), str(state))

    # Добавляем начальные состояния
    for start_state in dfa.start_states:
        dot.node(str(start_state) + "_start", shape="point")
        dot.edge(str(start_state) + "_start", str(start_state))

    # Добавляем конечные состояния
    for final_state in dfa.final_states:
        dot.node(str(final_state), str(final_state), shape="doublecircle")

    # Добавляем переходы
    for from_state, to_dict in dfa.to_dict().items():
        for symbol, to_state in to_dict.items():
            if type(to_state) == set:
                for to_state1 in to_dict:
                    dot.edge(str(from_state), str(to_state1), label=str(symbol))
            else:
                dot.edge(str(from_state), str(to_state), label=str(symbol))

    # Рендерим и сохраняем в файл
    dot.render(f'{name}', view=True)
