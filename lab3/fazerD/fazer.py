from CNF import CFG_to_CNF
from CYK import cyk_algorithm
from bigrams import generate_tests_from_matrix
from parse_grammar import recognize_grammar


def marking_tests(grammar, start_symbol, tests):
    marked_tests = []
    for test in tests:
        res = cyk_algorithm(grammar, start_symbol, test)
        marked_tests.append(f"{test} {int(res)}")
    return marked_tests


def save_tests(tests, filename="tests.txt"):
    with open(filename, 'w') as f:
        for test in tests:
            f.write(test + '\n')


def fazzer(grammar_str, start_symbol='S', num_tests=10):
    # считывем из строки грамматику
    grammar = recognize_grammar(grammar_str)

    # преобразуем к CNF
    CNF_grammar = CFG_to_CNF(grammar, start_symbol)

    # генерируем тесты
    tests = generate_tests_from_matrix(CNF_grammar, start_symbol, num_tests=num_tests)

    # размечаем тесты
    marked_tests = marking_tests(CNF_grammar, start_symbol, tests)

    return CNF_grammar, marked_tests