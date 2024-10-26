from graphviz import Digraph


class EolDFA:
    def __init__(self):
        self.states = {'q0', 'q1'}  # Состояния
        self.final_states = {'q1'}  # Конечные состояния
        self.transition = {  # Таблица переходов
            'q0': {'\n': 'q1'},  # Переход по символу конца строки
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
        dot = Digraph(comment='DFA for [eol]')

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
        dfa_graph.render('dfa_eol', format='png', cleanup=True)  # Сохраняет в файл dfa_eol.png
        dfa_graph.view()  # Открывает файл для просмотра
