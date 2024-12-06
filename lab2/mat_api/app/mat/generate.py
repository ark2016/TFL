import random

import rstr
from pyformlang.finite_automaton import DeterministicFiniteAutomaton, State, Symbol, EpsilonNFA, Epsilon

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

        self._alphabet = {'a', 'b', 'c', '0', '1', '2'}
        self._eol_alphabet = None
        self._blank_alphabet = None

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

        self.check, self.check_list = self.check_intersections()

    def generate_dfa(self, input_string, state_name):
        dfa = DeterministicFiniteAutomaton()
        previous_state = State(f"{state_name}0")
        dfa.add_start_state(previous_state)

        # Текущее состояние для каждого символа
        state_counter = 0

        for symbol in input_string:
            state_counter += 1
            current_state = State(f"{state_name}{state_counter}")
            dfa.add_transition(previous_state, Symbol(symbol), current_state)
            previous_state = current_state

        # Добавляем конечное состояние
        dfa.add_final_state(previous_state)

        return dfa

    def gen_alphabet1(self) -> set[str]:
        # n1 = random.randint(1, 2)
        n1 = 1
        # определяем алфавит для лексемы
        lexem_alphabet: set[str] = set()
        for i in range(n1):
            el = random.choice(list(self._alphabet))
            lexem_alphabet.add(el)
            self._alphabet.remove(el)
        return lexem_alphabet

    def generate_random_string(self, char_set, max_length):
        length = random.randint(1, max_length)
        return ''.join(random.choice(list(char_set)) for _ in range(length))

    def gen_random_eol_Automat(self):
        self._eol_alphabet = self.gen_alphabet1()
        eol_str = self.generate_random_string(self._eol_alphabet, self.maxSize)
        dfa = self.generate_dfa(eol_str, "EOL")
        return dfa

    def gen_random_blank_Automat(self):
        self._blank_alphabet = self.gen_alphabet1()
        blank_str = self.generate_random_string(self._blank_alphabet, self.maxSize)
        dfa = self.generate_dfa(blank_str, "B")
        return dfa

    def gen_random_equal_Automat(self):
        equal_alphabet_s = self.gen_alphabet1()
        first_eq_symb = list(equal_alphabet_s)[0]
        equal_str = first_eq_symb + self.generate_random_string(self._alphabet, self.maxSize - 2) + first_eq_symb
        dfa = self.generate_dfa(equal_str, "EQ")
        return dfa

    def gen_random_sep_Automat(self):
        sep_alphabet_s = self.gen_alphabet1()
        first_sep_symb = list(sep_alphabet_s)[0]
        sep_str = first_sep_symb + self.generate_random_string(self._alphabet, self.maxSize - 2) + first_sep_symb
        dfa = self.generate_dfa(sep_str, "SEP")
        return dfa

    def gen_random_const_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s1}{s2 * 2}{s1}"
        input_string = rstr.xeger(regex)
        dfa = self.generate_dfa(input_string, "C")
        return dfa

    def gen_random_var_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s2}{s1 * 2}{s2}"
        input_string = rstr.xeger(regex)
        dfa = self.generate_dfa(input_string, "V")
        return dfa

    def gen_random_lbr1_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s1 * 3}{s2}"
        input_string = rstr.xeger(regex)

        dfa = self.generate_dfa(input_string, "lbr1")
        return dfa

    def gen_random_rbr1_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s2}{s1}{s2 * 2}"
        input_string = rstr.xeger(regex)

        dfa = self.generate_dfa(input_string, "rbr1")
        return dfa

    def gen_random_lbr2_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s2 * 3}{s1}"
        input_string = rstr.xeger(regex)

        dfa = self.generate_dfa(input_string, "lbr2")
        return dfa

    def gen_random_rbr2_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s1}{s2}{s1 * 2}"
        input_string = rstr.xeger(regex)

        dfa = self.generate_dfa(input_string, "rbr2")
        return dfa

    def gen_random_lbr3_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s2 * 2}{s1}{s2}"
        input_string = rstr.xeger(regex)
        dfa = self.generate_dfa(input_string, "lbr3")
        return dfa

    def gen_random_rbr3_Automat(self):
        symbols = list(self._alphabet)
        s1, s2 = symbols[0], symbols[1]

        regex = f"{s1 * 2}{s2}{s1}"
        input_string = rstr.xeger(regex)

        dfa = self.generate_dfa(input_string, "rbr3")
        return dfa


    def _check_intersection_empty(self, dfa1: DeterministicFiniteAutomaton, dfa2: DeterministicFiniteAutomaton) -> bool:
        intersection: EpsilonNFA = dfa1.get_intersection(dfa2)
        return (intersection.is_empty())

    def check_intersections(self):
        result = {}
        # проверяем пересечение const и var
        r1 = self._check_intersection_empty(self.var_Automaton, self.const_Automat)
        result["VarAndConst"] = r1
        # проверяем попарные пересечения для скобок
        br1 = self._check_intersection_empty(self.lbr1_Automat, self.rbr1_Automat)
        result["lbr1Andlbr2"] = br1
        br2 = self._check_intersection_empty(self.lbr2_Automat, self.rbr2_Automat)
        result["lbr2Andlbr2"] = br2
        br3 = self._check_intersection_empty(self.lbr3_Automat, self.rbr3_Automat)
        result["lbr3Andlbr3"] = br3

        # проверяем пересечения для кадого типа скобок с const и var
        brackets = [self.lbr1_Automat, self.rbr1_Automat, self.lbr2_Automat, self.rbr2_Automat, self.lbr3_Automat, self.rbr3_Automat]
        c = 0
        for bracket in brackets:
            br11 = self._check_intersection_empty(bracket, self.const_Automat)
            br22 = self._check_intersection_empty(bracket, self.var_Automaton)
            # if br11 and br22:
            #print("Empty with var and const", br11 and br22)
            type = f"type{c}"
            c +=1
            result[f"Var{type}"] = br11
            result[f"Const{type}"] = br22

        # проверяем конкатенацию любых двух автоматов для скобок
        result["concatBrackets"] = True
        for bracket1 in brackets:
            for bracket2 in brackets:
                if bracket1.is_equivalent_to(bracket2):
                    continue
                concat_brackets = self.__connectDFA1_to_DFA2(bracket1, bracket2)
                for bracket3 in brackets:
                    if bracket3.is_equivalent_to(bracket1) or bracket3.is_equivalent_to(bracket2):
                        continue
                    br111 = self._check_intersection_empty(bracket3, concat_brackets)
                    if not br111:
                        result["concatBrackets"] = False
                        break

        # проверяем есть ли False после проверок
        check = False in set(result.values())
        return not check, result




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
        eol_plus = self.__connectDFA1_to_DFA2(self.eol_Automat, eol)

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
                newDFA.add_transition(from_state, symbol, to_state)

        # соединяем финальные соcтояния первого со стартовыми второго
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
