from fazer import fazzer

# Пример 1: Простая грамматика без ε-правил и цепных правил
grammar_str_1 = """
S -> aA
A -> b
"""

# Пример 2: Грамматика с ε-правилом
grammar_str_2 = """
S -> A
S -> 
A -> a
"""

# Пример 3: Грамматика с длинным правилом
grammar_str_3 = """
S -> abc
"""


# Пример 4: Грамматика с бесполезными символами
grammar_str_4 = """
S -> aA
A -> b
B -> c
"""

# Пример 5: Грамматика с несколькими стартовыми вариантами
grammar_str_5 = """
S -> A B
A -> a
B -> b
"""

# Пример 6: Сложная грамматика с ε-правилом, длинными правилами и бесполезными символами
grammar_str_6 = """
S -> AB
S -> 
A -> a
B -> bC
C -> c
D -> d
"""

# Пример 7: Грамматика для чередования символов
grammar_str_7 = """
S -> aB bA
A -> aB a
B -> bA b
"""

# Пример 8
grammar_str_8 = """
S -> aC bA
A -> aB
B -> bA b
C -> cB c D
D -> d
"""


grammars = [grammar_str_1, grammar_str_2, grammar_str_3, grammar_str_4, grammar_str_5, grammar_str_6, grammar_str_7, grammar_str_8]

for i, grammar_str in enumerate(grammars, start=1):
    print(f"=== Грамматика {i} ===")
    CNF_grammar, marked_tests = fazzer(grammar_str, start_symbol='S', num_tests=5)
    print(f"CNF-грамматика: {CNF_grammar}")
    print("Тесты и их разметка:")
    for test in marked_tests:
        print(f"  Тест: {test} ")
    print("\n")