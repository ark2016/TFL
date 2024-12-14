def cyk_algorithm(grammar, start_symbol, string):
    n = len(string)
    # Создаем таблицу P
    P = [[set() for _ in range(n)] for _ in range(n)]

    cnf_grammar = grammar
    # Шаг 1: Заполняем диагональ таблицы P
    for i in range(n):
        for nt, rules in cnf_grammar.items():
            for rule in rules:
                if len(rule) == 1 and rule[0] == string[i]:  # Если терминал соответствует
                    P[i][i].add(nt)

    # Шаг 2: Заполняем таблицу для подстрок длины > 1
    for length in range(2, n + 1):  # Длина подстроки
        for i in range(n - length + 1):
            j = i + length - 1
            for k in range(i, j):
                # Проверяем все разбиения на две части
                for nt, rules in cnf_grammar.items():
                    for rule in rules:
                        if len(rule) == 2:  # Применяем правило A -> BC
                            B, C = rule
                            if B in P[i][k] and C in P[k + 1][j]:
                                P[i][j].add(nt)

    # Шаг 3: Проверяем, принадлежит ли строка грамматике
    # Строка принадлежит грамматике, если начальный символ S есть в P[0][n-1]
    return start_symbol in P[0][n - 1]
