from graphviz import Digraph


class EqualDFA:
    def __init__(self):
        self.states = {'q0', 'q1', 'q2'}  # Состояния
        self.final_states = {'q2'}  # Конечные состояния
        self.transition = {  # Таблица переходов
            'q0': {'=': 'q1'},  # Переход по символу '='
            'q1': {'a': 'q1', 'b': 'q1', 'c': 'q1', '0': 'q1', '1': 'q1', '2': 'q1', '=': 'q2'},
            # Переходы по другим символам и на '='
            'q2': {}  # Конечное состояние
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
        dot = Digraph(comment='DFA for [equal]')

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
        dfa_graph.render('dfa_equal', format='png', cleanup=True)  # Сохраняет в файл dfa_equal.png
        dfa_graph.view()  # Открывает файл для просмотра


class SepDFA:
    def __init__(self):
        self.states = {'q0', 'q1', 'q2'}  # Состояния
        self.final_states = {'q2'}  # Конечные состояния
        self.transition = {  # Таблица переходов
            'q0': {';': 'q1'},  # Переход по символу ';'
            'q1': {'a': 'q1', 'b': 'q1', 'c': 'q1', '0': 'q1', '1': 'q1', '2': 'q1', ';': 'q2'},
            # Переходы по другим символам и на ';'
            'q2': {}  # Конечное состояние
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
        dot = Digraph(comment='DFA for [sep]')

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
        dfa_graph.render('dfa_sep', format='png', cleanup=True)  # Сохраняет в файл dfa_sep.png
        dfa_graph.view()  # Открывает файл для просмотра


# Пример использования
if __name__ == "__main__":
    equal_dfa = EqualDFA()
    equal_dfa.convertToImage()  # Создание изображения для DFA [equal]

    sep_dfa = SepDFA()
    sep_dfa.convertToImage()  # Создание изображения для DFA [sep]