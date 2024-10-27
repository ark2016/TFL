from graphviz import Digraph


class ExpressionDFA:
    def __init__(self, var_dfa, const_dfa, blank_dfa, lbr2_dfa, rbr2_dfa, lbr3_dfa, rbr3_dfa):
        self.states = {'q0', 'q_end'}  # начальное и конечное состояния
        self.final_states = {'q_end'}
        self.transition = {}
        self.add_transitions(var_dfa, 'q0', 'q_end')
        self.add_transitions(const_dfa, 'q0', 'q_end')
        self.add_composite_transitions(blank_dfa, var_dfa, const_dfa)
        self.add_bracketed_expression(lbr3_dfa, rbr3_dfa, const_dfa, blank_dfa)
        self.add_nested_bracket_expression(lbr2_dfa, rbr2_dfa, const_dfa, blank_dfa)

    def add_transitions(self, dfa, from_state, to_state):
        for state in dfa.states:
            self.states.add(state)
        for state, transitions in dfa.transition.items():
            if state not in self.transition:
                self.transition[state] = {}
            for symbol, next_state in transitions.items():
                if next_state in dfa.final_states:
                    self.transition[state][symbol] = to_state
                else:
                    self.transition[state][symbol] = next_state

    def add_composite_transitions(self, blank_dfa, var_dfa, const_dfa):
        self.add_transitions(blank_dfa, 'q0', 'q1')
        self.add_transitions(var_dfa, 'q1', 'q2')
        self.add_transitions(const_dfa, 'q2', 'q_end')

    def add_bracketed_expression(self, lbr3_dfa, rbr3_dfa, const_dfa, blank_dfa):
        self.add_transitions(lbr3_dfa, 'q0', 'q3')
        self.add_transitions(const_dfa, 'q3', 'q4')
        self.add_transitions(blank_dfa, 'q4', 'q5')
        self.add_transitions(rbr3_dfa, 'q5', 'q_end')

    def add_nested_bracket_expression(self, lbr2_dfa, rbr2_dfa, const_dfa, blank_dfa):
        self.add_transitions(lbr2_dfa, 'q0', 'q6')
        self.add_transitions(const_dfa, 'q6', 'q7')
        self.add_transitions(blank_dfa, 'q7', 'q8')
        self.add_transitions(rbr2_dfa, 'q8', 'q_end')

    def process_input(self, input_string):
        current_state = 'q0'
        for symbol in input_string:
            if current_state in self.transition and symbol in self.transition[current_state]:
                current_state = self.transition[current_state][symbol]
            else:
                return False
        return current_state in self.final_states

    def visualize(self):
        dot = Digraph(comment='DFA for [expression]')
        for state in self.states:
            shape = 'doublecircle' if state in self.final_states else 'circle'
            dot.node(state, state, shape=shape)
        for state, transitions in self.transition.items():
            for symbol, next_state in transitions.items():
                dot.edge(state, next_state, label=symbol)
        return dot

    def convert_to_image(self):
        dfa_graph = self.visualize()
        dfa_graph.render('dfa_expression', format='png', cleanup=True)
        dfa_graph.view()
