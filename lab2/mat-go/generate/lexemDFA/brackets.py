from graphviz import Digraph


class SingleBracketDFA:
    def __init__(self, bracket_symbol):
        self.bracket_symbol = bracket_symbol
        self.states = {'q0', 'accept'}
        self.final_states = {'accept'}
        self.transition = {
            'q0': {self.bracket_symbol: 'accept'},
            'accept': {}  # Нет переходов из финального состояния
        }

    def process_input(self, input_string):
        current_state = 'q0'
        for symbol in input_string:
            if symbol in self.transition[current_state]:
                current_state = self.transition[current_state][symbol]
            else:
                return False
        return current_state in self.final_states

    def visualize(self, label):
        dot = Digraph(comment=f'DFA for {label}')
        for state in self.states:
            if state in self.final_states:
                dot.node(state, state, shape='doublecircle')
            else:
                dot.node(state, state, shape='circle')
        for state, transitions in self.transition.items():
            for symbol, next_state in transitions.items():
                dot.edge(state, next_state, label=symbol)
        return dot


def t():
    # Создание ДКА для каждой отдельной скобки
    lbr1_dfa = SingleBracketDFA('(')  # [lbr-1]
    rbr1_dfa = SingleBracketDFA(')')  # [rbr-1]
    lbr2_dfa = SingleBracketDFA('[')  # [lbr-2]
    rbr2_dfa = SingleBracketDFA(']')  # [rbr-2]
    lbr3_dfa = SingleBracketDFA('{')  # [lbr-3]
    rbr3_dfa = SingleBracketDFA('}')  # [rbr-3]

    # Визуализация автоматов
    lbr1_dfa.visualize('[lbr-1]').render('lbr1_dfa', format='png', cleanup=True)
    rbr1_dfa.visualize('[rbr-1]').render('rbr1_dfa', format='png', cleanup=True)
