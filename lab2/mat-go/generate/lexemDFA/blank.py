from graphviz import Digraph


class BlankDFA:
    def __init__(self):
        self.states = {'q0', 'q1'}  # Состояния
        self.final_states = {'q1'}  # Конечные состояния
        self.transition = {  # Таблица переходов
            'q0': {' ': 'q1', '\t': 'q1'},  # Переходы по пробелу и табуляции
            'q1': {}  # Конечное состояние
        }

    def process_input(self, input_string):
        current_state = 'q0'  # Начальное состояние
        for symbol in input_string:
            if symbol in self.transition[current_state]:
                current_state = self.transition[current_state][symbol]
            else:
                return False  # Перехода не существует, строка не принимается
        return current_state in self.final_states  # Проверяем, конечное ли состояние

    def visualize(self):
        dot = Digraph(comment='DFA for [blank]')

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

    def convertToImage(self):
        dfa_graph = self.visualize()
        dfa_graph.render('dfa_blank', format='png', cleanup=True)  # Сохраняет в файл dfa_blank.png
        dfa_graph.view()  # Открывает файл для просмотра