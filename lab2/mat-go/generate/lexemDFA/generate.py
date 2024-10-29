import random

from graphviz import Digraph
from pyformlang.finite_automaton import DeterministicFiniteAutomaton, State, Symbol, epsilon, \
    NondeterministicFiniteAutomaton, EpsilonNFA

# грамматика
"""
[program] ::= [eol]*([definition][eol]+)+
[definition] ::= [const] [lbr-1] ([eol]*[sentence])* [eol]*[rbr-1]
[sentence] ::= [pattern][equal][expression][sep]
[pattern] ::= [lbr-3][pattern][rbr-3]|[pattern][blank][pattern] | [var] | [const] |
[expression] ::= [var] | [const] | 
                [expression][blank][expression][lbr-3][expression] [rbr-3] |
                 [lbr-2] [const] [blank] [expression] [rbr-2]
"""


class AutomatGenerator:
    def __init__(self, name: str):
        self.name = name
        # сразу создаем автоматы для лексем
        self.const_Automat = self.gen_random_const_Automat()
        self.var_Automaton = self.gen_random_var_Automat()

        self.eol_Automat = self.gen_random_eol_Automat()
        self.equal_Automat = self.gen_random_equal_Automat()
        self.sep_Automat = self.gen_random_sep_Automat()
        self.blank_Automat = self.gen_random_blank_Automat()

        self.lbr1_Automat = self.gen_random_lbr1_Automat()
        self.rbr1_Automat = self.gen_random_rbr1_Automat()

        self.lbr2_Automat = self.gen_random_lbr2_Automat()
        self.rbr2_Automat = self.gen_random_rbr2_Automat()

        self.lbr3_Automat = self.gen_random_lbr3_Automat()
        self.rbr3_Automat = self.gen_random_rbr3_Automat()

    def _gen_random_tokensCods(self):
        tokensCods = {
            'eol': random.randint(1, 5) * 'c' + random.randint(1, 3) * '0',  # Конец строки
            'blank': random.randint(1, 6) * 'b' + random.randint(1, 6) * '0',  # Пробел
            'equal': random.randint(1, 3) * 'a' + random.randint(1, 2) * 'b' + random.randint(1, 2) * '2',  # =
            'sep': random.randint(1, 3) * 'a' + random.randint(1, 2) * 'b' + random.randint(1, 2) * '1',  # ;
            'const': ['0', '1', '2'],  # Константы
            'var': ['a', 'b', 'c'],  # Переменные
            'lbracket1': random.randint(1, 5) * '0' + random.randint(1, 3) * 'a',  # {
            'rbracket1': random.randint(1, 5) * '0' + random.randint(1, 3) * 'b',  # }
            'lbracket2': random.randint(1, 5) * '1' + random.randint(1, 3) * 'a',  # [
            'rbracket2': random.randint(1, 5) * '1' + random.randint(1, 3) * 'b',  # ]
            'lbracket3': random.randint(1, 5) * '2' + random.randint(1, 3) * 'a',  # (
            'rbracket3': random.randint(1, 5) * '2' + random.randint(1, 3) * 'b',  # )
        }
        return tokensCods

    def generate_dfa(self, input_string, condition, state_name):
        """
        Генерирует ДКА на основе заданной строки и условия.

        :param state_name:
        :param input_string: строка, на основе которой строится ДКА.
        :param condition: словарь с условиями (символ: количество повторений).
        :return: объект DeterministicFiniteAutomaton, представляющий сгенерированный ДКА.
        """
        dfa = DeterministicFiniteAutomaton()
        previous_state = State(f"{state_name}0")
        dfa.add_start_state(previous_state)

        # Текущее состояние для каждого символа
        state_counter = 0

        for symbol in input_string:
            if symbol not in condition:
                raise ValueError(f"Условие для символа '{symbol}' не задано.")
            repetitions = condition[symbol]
            for _ in range(repetitions):
                state_counter += 1
                current_state = State(f"{state_name}{state_counter}")
                dfa.add_transition(previous_state, Symbol(symbol), current_state)
                previous_state = current_state

        # Добавляем конечное состояние
        dfa.add_final_state(previous_state)

        return dfa

    def gen_random_eol_Automat(self):
        input_string = "c0"
        countC, count0 = random.randint(1, 5), random.randint(2, 6)
        condition = {"c": countC, "0": count0}
        dfa = self.generate_dfa(input_string, condition, "EOL")
        return dfa

    def gen_random_const_Automat(self):
        # Создаём ДКА
        dfa = DeterministicFiniteAutomaton()

        # Определяем единственное состояние
        state = State("C0")

        # Добавляем состояние как начальное и конечное
        dfa.add_start_state(state)
        dfa.add_final_state(state)

        # Добавляем переходы для символов '0', '1', и '2'
        dfa.add_transition(state, Symbol('0'), state)
        dfa.add_transition(state, Symbol('1'), state)
        dfa.add_transition(state, Symbol('2'), state)

        return dfa

    def gen_random_var_Automat(self):
        # Создаём ДКА
        dfa = DeterministicFiniteAutomaton()

        # Определяем единственное состояние
        state = State("V0")

        # Добавляем состояние как начальное и конечное
        dfa.add_start_state(state)
        dfa.add_final_state(state)

        # Добавляем переходы для символов 'a', 'b', и 'c'
        dfa.add_transition(state, Symbol('a'), state)
        dfa.add_transition(state, Symbol('b'), state)
        dfa.add_transition(state, Symbol('c'), state)
        return dfa

    def gen_random_blank_Automat(self):
        dfa = DeterministicFiniteAutomaton()
        input_string = "b0"
        countB, count0 = random.randint(1, 5), random.randint(2, 6)
        condition = {"b": countB, "0": count0}
        dfa = self.generate_dfa(input_string, condition, "B")
        return dfa

    def gen_random_lbr1_Automat(self):
        dfa = DeterministicFiniteAutomaton()
        input_string = "0a"
        count0, countA = random.randint(1, 5), random.randint(2, 6)
        condition = {"0": count0, "a": countA}
        dfa = self.generate_dfa(input_string, condition, "lbr1")
        return dfa

    def gen_random_rbr1_Automat(self):
        input_string = "0b"
        count0, countB = random.randint(1, 5), random.randint(2, 6)
        condition = {"0": count0, "b": countB}
        dfa = self.generate_dfa(input_string, condition, "rbr1")
        return dfa

    def gen_random_equal_Automat(self):
        dfa = DeterministicFiniteAutomaton()
        input_string = "ab2"
        countA, countB, count2 = random.randint(1, 5), random.randint(2, 6), random.randint(1, 6)
        condition = {"a": countA, "b": countB, "2": count2}
        dfa = self.generate_dfa(input_string, condition, "EQ")
        return dfa

    def gen_random_sep_Automat(self):
        input_string = "ab1"
        countA, countB, count1 = random.randint(1, 5), random.randint(2, 6), random.randint(1, 6)
        condition = {"a": countA, "b": countB, "1": count1}
        dfa = self.generate_dfa(input_string, condition, "SEP")
        return dfa

    def gen_random_lbr2_Automat(self):
        input_string = "1a"
        count1, countA = random.randint(1, 5), random.randint(2, 6)
        condition = {"1": count1, "a": countA}
        dfa = self.generate_dfa(input_string, condition, "lbr2")
        return dfa

    def gen_random_rbr2_Automat(self):
        input_string = "1b"
        count1, countB = random.randint(1, 5), random.randint(2, 6)
        condition = {"1": count1, "b": countB}
        dfa = self.generate_dfa(input_string, condition, "rbr2")
        return dfa

    def gen_random_lbr3_Automat(self):
        input_string = "2a"
        count2, countA = random.randint(1, 5), random.randint(2, 6)
        condition = {"2": count2, "a": countA}
        dfa = self.generate_dfa(input_string, condition, "lbr3")
        return dfa

    def gen_random_rbr3_Automat(self):
        input_string = "2b"
        count2, countB = random.randint(1, 5), random.randint(2, 6)
        condition = {"2": count2, "b": countB}
        dfa = self.generate_dfa(input_string, condition, "rbr3")
        return dfa

    # --------------------------------------

    def gen_random_expression_Automat(self, depth=0, max_depth=2):
        """
        [expression] ::= [var] | [const] |
                    [expression][blank][expression][lbr-3][expression] [rbr-3] |
                     [lbr-2] [const] [blank] [expression] [rbr-2]
        :param depth:
        :param max_depth:
        :return:
        """
        print(depth, max_depth)
        if depth >= max_depth:
            # На максимальной глубине возвращаем только var или const
            return random.choice([self.var_Automaton, self.const_Automat])

        choice = random.choice(["var", "const", "expr_expr", "const_expr"])

        if choice == "var":
            return self.var_Automaton

        elif choice == "const":
            return self.const_Automat

        elif choice == "expr_expr":
            expr1 = self.gen_random_expression_Automat(depth + 1, max_depth)
            expr2 = self.gen_random_expression_Automat(depth + 1, max_depth)
            expr3 = self.gen_random_expression_Automat(depth + 1, max_depth)

            # Конкатенация автоматов
            # конечные состояния expr1 соединяем с начальным у blank
            expr1_blank = self.__connectDFA1_to_DFA2(expr1, self.blank_Automat)
            expr1_blank_expr2 = self.__connectDFA1_to_DFA2(expr1_blank, expr2)
            expr1_blank_expr2_lbr3 = self.__connectDFA1_to_DFA2(expr1_blank_expr2, self.lbr3_Automat)
            expr1_blank_expr2_lbr3_expr3 = self.__connectDFA1_to_DFA2(expr1_blank_expr2_lbr3, expr3)
            newDka5 = self.__connectDFA1_to_DFA2(expr1_blank_expr2_lbr3_expr3, self.rbr3_Automat)

            return newDka5

        elif choice == "const_expr":
            expr = self.gen_random_expression_Automat(depth + 1, max_depth)

            lbr2_const = self.__connectDFA1_to_DFA2(self.lbr2_Automat, self.const_Automat)
            lbr2_const_blank = self.__connectDFA1_to_DFA2(lbr2_const, self.blank_Automat)
            lbr2_const_blank_expr = self.__connectDFA1_to_DFA2(lbr2_const_blank, expr)
            newDka4 = self.__connectDFA1_to_DFA2(lbr2_const_blank_expr, self.rbr2_Automat)

            return newDka4

    def gen_random_pattern_Automat(self, depth=0, max_depth=2):
        """
        [pattern] ::= [lbr-3][pattern][rbr-3]|[pattern][blank][pattern] | [var] | [const] |

        :param depth:
        :param max_depth:
        :return:
        """
        if depth >= max_depth:
            # На максимальной глубине возвращаем только var или const
            return random.choice([self.var_Automaton, self.const_Automat])

        choice = random.choice(["brackets", "concat", "var", "const"])

        if choice == "brackets":
            pattern = self.gen_random_pattern_Automat(depth + 1, max_depth)

            lbr3_pattern = self.__connectDFA1_to_DFA2(self.lbr3_Automat, pattern)
            lbr3_pattern_rbr3 = self.__connectDFA1_to_DFA2(lbr3_pattern, self.rbr3_Automat)

            return lbr3_pattern_rbr3

        elif choice == "concat":
            pattern1 = self.gen_random_pattern_Automat(depth + 1, max_depth)
            pattern2 = self.gen_random_pattern_Automat(depth + 1, max_depth)

            pattern1_blank = self.__connectDFA1_to_DFA2(pattern1, self.blank_Automat)
            pattern1_blank_pattern2 = self.__connectDFA1_to_DFA2(pattern1_blank, pattern2)

            return pattern1_blank_pattern2

        elif choice == "var":
            return self.var_Automaton

        elif choice == "const":
            return self.const_Automat

    def gen_random_sentence_Automat(self):
        """
        [sentence] ::= [pattern][equal][expression][sep]

        :return:
        """
        pattern = self.gen_random_pattern_Automat()
        expression = self.gen_random_expression_Automat()

        # Соединяем все части вместе
        pattern_equal = self.__connectDFA1_to_DFA2(pattern, self.equal_Automat)
        pattern_equal_expr = self.__connectDFA1_to_DFA2(pattern_equal, expression)
        newDKA3 = self.__connectDFA1_to_DFA2(pattern_equal_expr, self.sep_Automat)

        return newDKA3

    def gen_random_definition_Automat(self):
        """
        [definition] ::= [const] [lbr-1] ([eol]*[sentence])* [eol]*[rbr-1]

        :return:
        """

        sentence = self.gen_random_sentence_Automat()

        definition_dka = self.__connectDFA1_to_DFA2(self.const_Automat, self.lbr1_Automat)

        # Создаем автомат для (eol* sentence)*

        # Переходы для eol* sentence
        eol_kleene: EpsilonNFA = self.eol_Automat.kleene_star()  # пока не используем

        eol_kleene: DeterministicFiniteAutomaton = self.eol_Automat

        middle_dka = self.__connectDFA1_to_DFA2(eol_kleene, sentence)

        # Переходы для (eol* sentence)*
        # middle_nfa = middle_nfa.kleene_star()
        middle_kleene: EpsilonNFA = middle_dka.kleene_star()
        middle_kleene_dka: DeterministicFiniteAutomaton = middle_kleene.to_deterministic()

        # Добавляем middle_nfa к definition_nfa
        definition_middle_dka = self.__connectDFA1_to_DFA2(definition_dka, middle_kleene_dka)

        # Добавляем eol и rbr в конце
        definition_middle_eol_dka = self.__connectDFA1_to_DFA2(definition_middle_dka, eol_kleene)
        newDKA6 = self.__connectDFA1_to_DFA2(definition_middle_eol_dka, self.rbr1_Automat)

        return newDKA6

    def gen_random_program_Automat(self):
        """
        [program] ::= [eol]*([definition][eol]+)+

        :return:
        """
        definition_automaton = self.gen_random_definition_Automat()

        # делаем [eol]*
        eol_nfa: EpsilonNFA = self.eol_Automat.kleene_star()

        eol = eol_nfa.to_deterministic()

        # Создаем автомат для ([definition][eol]+)+
        # пока eol без +
        definition_eol_dka = self.__connectDFA1_to_DFA2(definition_automaton, eol)

        # ([definition][eol]+)+
        # тоже пока без +
        # definition_eol_nfa = definition_eol_nfa.plus()

        # Добавляем ([definition][eol]+)+ к [eol]*
        dfa = self.__connectDFA1_to_DFA2(eol, definition_eol_dka)

        return dfa

    # -------------------------------------

    def visualize_dfa(self, dfa, name):
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

    def __canonical_numbering(self, dfa, type):
        # Получаем все состояния
        states = list(dfa.states)

        # Сортируем состояния
        states.sort(key=lambda state: str(state))

        # Создаем отображение старых состояний в новые
        state_mapping = {state: State(type + str(i)) for i, state in enumerate(states)}

        # Создаем новый ДКА с переименованными состояниями
        new_dfa = DeterministicFiniteAutomaton()

        # Добавляем состояния
        for old_state, new_state in state_mapping.items():
            if old_state == dfa.start_state:
                new_dfa.add_start_state(new_state)
            if old_state in dfa.final_states:
                new_dfa.add_final_state(new_state)

        # Добавляем переходы
        for from_state, symbol, to_state in dfa:
            new_dfa.add_transition(state_mapping[from_state], symbol, state_mapping[to_state])

        return new_dfa

    def __connectDFA1_to_DFA2(self, dfa11: DeterministicFiniteAutomaton, dfa21: DeterministicFiniteAutomaton):
        # перенумеровываем состояния автоматов, чтобы они не совпадали
        renumbered_DFA1 = self.__canonical_numbering(dfa11, "t")
        renumbered_DFA2 = self.__canonical_numbering(dfa21, "n")

        dfa1 = renumbered_DFA1
        dfa2 = renumbered_DFA2

        # запоминаем финальные состояния автоматов
        finalStates1 = list(dfa1.final_states)
        finalStates2 = list(dfa2.final_states)

        # убираем финальность состояний для dfa1
        for final_state in finalStates1:
            dfa1.remove_final_state(final_state)

        # запоминаем стартовые состояния автоматов
        startState1 = dfa1.start_state
        startState2 = dfa2.start_state

        # создаем новый автомат
        newDFA = DeterministicFiniteAutomaton()
        newDFA.add_start_state(startState1)

        transictons1 = dfa1.to_fst().transitions

        for transicton in transictons1:
            from_state = State(transicton[0])
            symbol = Symbol(transicton[1])
            to_states = transictons1[transicton]

            for state_transicton in to_states:
                to_state = State(state_transicton[0])
                """
                if to_state in finalStates1:
                    if to_state == startState1:
                        to_state = startState1
                        newDFA.add_transition(to_state, Symbol('E'), startState2)
                    else:
                        to_state = startState2
                """

                newDFA.add_transition(from_state, symbol, to_state)

        # соединяем финальные сотояния первого со стартовыми второго
        # E - epsilon переход
        for state in finalStates1:
            newDFA.add_transition(state, Symbol('E'), startState2)

        transictons2 = dfa2.to_fst().transitions

        for state in finalStates2:
            newDFA.add_final_state(state)

        for transicton in transictons2:
            state = State(transicton[0])
            symbol = Symbol(transicton[1])
            to_states = transictons2[transicton]

            for state_transicton in to_states:
                to_state = State(state_transicton[0])
                newDFA.add_transition(state, symbol, to_state)

        # перенумеровываем состояния в едином стиле
        renumbered_DFA = self.__canonical_numbering(newDFA, "q")

        return renumbered_DFA

    def get_automat_info(self, automat):
        info = ""
        info += "Starts: " + automat.start_states + "\n"
        info += "Finals: ", automat.final_states
        return info
