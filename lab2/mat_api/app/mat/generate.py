import random

from graphviz import Digraph
from pyformlang.finite_automaton import DeterministicFiniteAutomaton, State, Symbol, epsilon, \
    NondeterministicFiniteAutomaton, EpsilonNFA, FiniteAutomaton, Epsilon

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
    def __init__(self, name: str, maxSize: int, maxNesting: int):
        self.name = name
        self.maxSize = maxSize
        self.maxNesting = maxNesting

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
        input_string = "0"
        count0 = random.randint(2, self.maxSize)
        condition = {"0": count0}
        dfa = self.generate_dfa(input_string, condition, "EOL")
        return dfa

    def gen_random_blank_Automat(self):
        input_string = "1"
        count1 = random.randint(2, self.maxSize)
        condition = {"1": count1}
        dfa = self.generate_dfa(input_string, condition, "B")
        return dfa

    def gen_random_equal_Automat(self):
        input_string = "2"
        count2 = random.randint(2, self.maxSize)
        condition = {"2": count2}
        dfa = self.generate_dfa(input_string, condition, "EQ")
        return dfa

    def gen_random_sep_Automat(self):
        input_string = "a"
        countA = random.randint(2, self.maxSize)
        condition = {"a": countA}
        dfa = self.generate_dfa(input_string, condition, "SEP")
        return dfa

    def gen_random_const_Automat(self):
        input_string = "bccb"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "C")
        return dfa

    def gen_random_var_Automat(self):
        input_string = "cbbc"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "V")
        return dfa

    def gen_random_lbr1_Automat(self):
        input_string = "bbbc"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "lbr1")
        return dfa

    def gen_random_rbr1_Automat(self):
        input_string = "cbcc"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "rbr1")
        return dfa

    def gen_random_lbr2_Automat(self):
        input_string = "cccb"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "lbr2")
        return dfa

    def gen_random_rbr2_Automat(self):
        input_string = "bcbb"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "rbr2")
        return dfa

    def gen_random_lbr3_Automat(self):
        input_string = "ccbc"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
        dfa = self.generate_dfa(input_string, condition, "lbr3")
        return dfa

    def gen_random_rbr3_Automat(self):
        input_string = "bbcb"
        max = self.maxSize // 4
        countB, countC = random.randint(1, max), random.randint(1, max)
        condition = {"b": countB, "c": countC}
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
        pattern = self.gen_random_pattern_Automat(max_depth=self.maxNesting)
        expression = self.gen_random_expression_Automat(max_depth=self.maxNesting)

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

        eol_kleene_dka: DeterministicFiniteAutomaton = eol_kleene.to_deterministic()

        middle_dka = self.__connectDFA1_to_DFA2(eol_kleene_dka, sentence)

        # Переходы для (eol* sentence)*
        # middle_nfa = middle_nfa.kleene_star()
        middle_kleene: EpsilonNFA = middle_dka.kleene_star()
        middle_kleene_dka: DeterministicFiniteAutomaton = middle_kleene.to_deterministic()

        # Добавляем middle_nfa к definition_nfa
        definition_middle_dka = self.__connectDFA1_to_DFA2(definition_dka, middle_kleene_dka)

        # Добавляем eol и rbr в конце
        definition_middle_eol_dka = self.__connectDFA1_to_DFA2(definition_middle_dka, eol_kleene_dka)
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
        eol_plus =  self.__connectDFA1_to_DFA2(self.eol_Automat, eol)

        # Создаем автомат для ([definition][eol]+)+
        definition_eol_dka = self.__connectDFA1_to_DFA2(definition_automaton, eol_plus)

        # ([definition][eol]+)+
        definition_eol_nfa_star = definition_eol_dka.kleene_star()
        definition_eol_dka_star_dka = definition_eol_nfa_star.to_deterministic()

        definition_eol_dka_plus = self.__connectDFA1_to_DFA2(definition_eol_dka, definition_eol_dka_star_dka)

        # Добавляем ([definition][eol]+)+ к [eol]*
        dfa = self.__connectDFA1_to_DFA2(eol, definition_eol_dka_plus)

        return dfa

    # -------------------------------------

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

    def __canonical_numbering_dfa(self, dfa):
        """
        Возвращает новый DFA с канонически пронумерованными состояниями.
        """
        # Начинаем с обхода от начального состояния
        start_state = next(iter(dfa.start_states))
        queue = [start_state]
        visited = set([start_state])

        # Словарь для соответствия оригинальных состояний новым
        canonical_states = {start_state: State(0)}
        current_number = 1

        # Создаем новый ДКА с канонической нумерацией состояний
        new_dfa = DeterministicFiniteAutomaton()
        new_dfa.add_start_state(canonical_states[start_state])

        # Обход в ширину и создание нового автомата
        while queue:
            state = queue.pop(0)
            new_state = canonical_states[state]

            # Проверка переходов для каждого символа алфавита
            for symbol in dfa.symbols:
                next_states = dfa(state, symbol)
                if next_states:
                    next_state = next_states.pop()
                    if next_state not in visited:
                        visited.add(next_state)
                        canonical_states[next_state] = State(current_number)
                        current_number += 1
                        queue.append(next_state)

                    # Добавляем переход в новый ДКА
                    new_dfa.add_transition(new_state, symbol, canonical_states[next_state])

        # Устанавливаем конечные состояния
        for state in dfa.final_states:
            if state in canonical_states:
                new_dfa.add_final_state(canonical_states[state])

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
        # newDFA = DeterministicFiniteAutomaton()
        epsilon = Epsilon()
        # newDFA = NondeterministicFiniteAutomaton()
        newDFA = EpsilonNFA()
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
        # print(epsilon)
        for state in finalStates1:
            newDFA.add_transition(state, epsilon, startState2)

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
        renumbered_DFA = self.__canonical_numbering_dfa(newDFA.to_deterministic())

        return renumbered_DFA
