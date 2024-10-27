import random

from graphviz import Digraph
from pyformlang.finite_automaton import DeterministicFiniteAutomaton, State, Symbol

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

nesting = 2


def gen_random_tokensCods():
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


def generate_dfa(input_string, condition):
    """
    Генерирует ДКА на основе заданной строки и условия.

    :param input_string: строка, на основе которой строится ДКА.
    :param condition: словарь с условиями (символ: количество повторений).
    :return: объект DeterministicFiniteAutomaton, представляющий сгенерированный ДКА.
    """
    dfa = DeterministicFiniteAutomaton()
    previous_state = State("S0")
    dfa.add_start_state(previous_state)

    # Текущее состояние для каждого символа
    state_counter = 0

    for symbol in input_string:
        if symbol not in condition:
            raise ValueError(f"Условие для символа '{symbol}' не задано.")
        repetitions = condition[symbol]
        for _ in range(repetitions):
            state_counter += 1
            current_state = State(f"S{state_counter}")
            dfa.add_transition(previous_state, Symbol(symbol), current_state)
            previous_state = current_state

    # Добавляем конечное состояние
    dfa.add_final_state(previous_state)

    return dfa


def gen_random_eol_Automat():
    dfa = DeterministicFiniteAutomaton()
    input_string = "c0"
    countC, count0 = random.randint(1, 5), random.randint(2, 6)
    condition = {"c": countC, "0": count0}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_const_Automat():
    # Создаём ДКА
    dfa = DeterministicFiniteAutomaton()

    # Определяем единственное состояние
    state = State("S0")

    # Добавляем состояние как начальное и конечное
    dfa.add_start_state(state)
    dfa.add_final_state(state)

    # Добавляем переходы для символов '0', '1', и '2'
    dfa.add_transition(state, Symbol('0'), state)
    dfa.add_transition(state, Symbol('1'), state)
    dfa.add_transition(state, Symbol('2'), state)

    return dfa


def gen_random_var_Automat():
    # Создаём ДКА
    dfa = DeterministicFiniteAutomaton()

    # Определяем единственное состояние
    state = State("S0")

    # Добавляем состояние как начальное и конечное
    dfa.add_start_state(state)
    dfa.add_final_state(state)

    # Добавляем переходы для символов 'a', 'b', и 'c'
    dfa.add_transition(state, Symbol('a'), state)
    dfa.add_transition(state, Symbol('b'), state)
    dfa.add_transition(state, Symbol('c'), state)
    return dfa


def gen_random_blank_Automat():
    dfa = DeterministicFiniteAutomaton()
    input_string = "b0"
    countB, count0 = random.randint(1, 5), random.randint(2, 6)
    condition = {"b": countB, "0": count0}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_lbr1_Automat():
    dfa = DeterministicFiniteAutomaton()
    input_string = "0a"
    count0, countA = random.randint(1, 5), random.randint(2, 6)
    condition = {"0": count0, "a": countA}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_rbr1_Automat():
    input_string = "0b"
    count0, countB = random.randint(1, 5), random.randint(2, 6)
    condition = {"0": count0, "b": countB}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_equal_Automat():
    dfa = DeterministicFiniteAutomaton()
    input_string = "ab2"
    countA, countB, count2 = random.randint(1, 5), random.randint(2, 6), random.randint(1, 6)
    condition = {"a": countA, "b": countB, "2": count2}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_sep_Automat():
    input_string = "ab1"
    countA, countB, count1 = random.randint(1, 5), random.randint(2, 6), random.randint(1, 6)
    condition = {"a": countA, "b": countB, "1": count1}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_lbr2_Automat():
    input_string = "1a"
    count1, countA = random.randint(1, 5), random.randint(2, 6)
    condition = {"1": count1, "a": countA}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_rbr2_Automat():
    input_string = "1b"
    count1, countB = random.randint(1, 5), random.randint(2, 6)
    condition = {"1": count1, "b": countB}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_lbr3_Automat():
    input_string = "2a"
    count2, countA = random.randint(1, 5), random.randint(2, 6)
    condition = {"2": count2, "a": countA}
    dfa = generate_dfa(input_string, condition)
    return dfa


def gen_random_rbr3_Automat():
    input_string = "2b"
    count2, countB = random.randint(1, 5), random.randint(2, 6)
    condition = {"2": count2, "b": countB}
    dfa = generate_dfa(input_string, condition)
    return dfa


# --------------------------------------


def gen_random_expression_Automat(depth=0, max_depth=3):
    print(depth, max_depth)
    if depth >= max_depth:
        # На максимальной глубине возвращаем только var или const
        return random.choice([gen_random_var_Automat(), gen_random_const_Automat()])

    choice = random.choice(["var", "const", "expr_expr", "const_expr"])

    if choice == "var":
        return gen_random_var_Automat()

    elif choice == "const":
        return gen_random_const_Automat()

    elif choice == "expr_expr":
        expr1 = gen_random_expression_Automat(depth + 1, max_depth)
        blank = gen_random_blank_Automat()
        expr2 = gen_random_expression_Automat(depth + 1, max_depth)
        lbr3 = gen_random_lbr3_Automat()
        expr3 = gen_random_expression_Automat(depth + 1, max_depth)
        rbr3 = gen_random_rbr3_Automat()

        # Конкатенация автоматов
        return expr1.concatenate(blank).concatenate(expr2).concatenate(lbr3).concatenate(expr3).concatenate(rbr3)

    elif choice == "const_expr":
        lbr2 = gen_random_lbr2_Automat()
        const = gen_random_const_Automat()
        blank = gen_random_blank_Automat()
        expr = gen_random_expression_Automat(depth + 1, max_depth)
        rbr2 = gen_random_rbr2_Automat()

        # Конкатенация автоматов
        return lbr2.concatenate(const).concatenate(blank).concatenate(expr).concatenate(rbr2)


def gen_random_pattern_Automat(depth=0, max_depth=3):
    if depth >= max_depth:
        # На максимальной глубине возвращаем только var или const
        return random.choice([gen_random_var_Automat(), gen_random_const_Automat()])

    choice = random.choice(["brackets", "concat", "var", "const"])

    if choice == "brackets":
        lbr3 = gen_random_lbr3_Automat()
        pattern = gen_random_pattern_Automat(depth + 1, max_depth)
        rbr3 = gen_random_rbr3_Automat()
        return lbr3.concatenate(pattern).concatenate(rbr3)

    elif choice == "concat":
        pattern1 = gen_random_pattern_Automat(depth + 1, max_depth)
        blank = gen_random_blank_Automat()
        pattern2 = gen_random_pattern_Automat(depth + 1, max_depth)
        return pattern1.concatenate(blank).concatenate(pattern2)

    elif choice == "var":
        return gen_random_var_Automat()

    elif choice == "const":
        return gen_random_const_Automat()


def gen_random_sentence_Automat():
    pattern = gen_random_pattern_Automat()
    equal = gen_random_equal_Automat()
    expression = gen_random_expression_Automat()
    sep = gen_random_sep_Automat()

    # Соединяем все части вместе
    sentence = pattern.concatenate(equal).concatenate(expression).concatenate(sep)

    return sentence


def gen_random_definition_Automat():
    dfa = DeterministicFiniteAutomaton()
    return dfa


def gen_random_program_Automat():
    dfa = DeterministicFiniteAutomaton()
    return dfa

# -------------------------------------

def visualize_dfa(dfa):
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
            print(symbol, to_state, type(to_state))
            if type(to_state) == set:
                for to_state1 in to_dict:
                    dot.edge(str(from_state), str(to_state1), label=str(symbol))
            else:
                dot.edge(str(from_state), str(to_state), label=str(symbol))

    # Рендерим и сохраняем в файл
    dot.render('dfa.gv', view=True)

def main():
    print(gen_random_tokensCods())
    eol = gen_random_eol_Automat()
    #visualize_dfa(eol)

    expr = gen_random_expression_Automat()
    #visualize_dfa(expr)

    #print(expr.states)
    #print(expr.to_regex())

    #patern = gen_random_pattern_Automat()
    #visualize_dfa(patern)
    #print(patern.states)
    #print(patern.to_regex())

    #sentence = gen_random_sentence_Automat()
    #print(sentence.states)
    #print(sentence.to_regex())


main()
