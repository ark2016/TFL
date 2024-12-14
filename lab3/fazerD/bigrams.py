import random


def compute_first(grammar):
    first = {non_term: set() for non_term in grammar}

    changed = True
    while changed:
        changed = False
        for non_term, rules in grammar.items():
            for rule in rules:
                if rule[0].islower():  # Если первый символ - терминал
                    if rule[0] not in first[non_term]:
                        first[non_term].add(rule[0])
                        changed = True
                elif rule[0].isupper():  # Если первый символ - нетерминал
                    size_before = len(first[non_term])
                    first[non_term].update(first[rule[0]])
                    if len(first[non_term]) > size_before:
                        changed = True
    return first


def compute_follow(grammar, start_symbol, first):
    follow = {non_term: set() for non_term in grammar}
    follow[start_symbol].add("$")  # $ обозначает конец строки

    changed = True
    while changed:
        changed = False
        for non_term, rules in grammar.items():
            for rule in rules:
                for i, symbol in enumerate(rule):
                    if symbol.isupper():  # Если символ - нетерминал
                        if i + 1 < len(rule):  # Если есть следующий символ
                            next_symbol = rule[i + 1]
                            if next_symbol.islower():  # Если следующий символ - терминал
                                if next_symbol not in follow[symbol]:
                                    follow[symbol].add(next_symbol)
                                    changed = True
                            else:  # Если следующий символ - нетерминал
                                size_before = len(follow[symbol])
                                follow[symbol].update(first[next_symbol])
                                if len(follow[symbol]) > size_before:
                                    changed = True
                        else:  # Если это последний символ в правиле
                            size_before = len(follow[symbol])
                            follow[symbol].update(follow[non_term])
                            if len(follow[symbol]) > size_before:
                                changed = True
    return follow


def compute_last(grammar):
    last = {non_term: set() for non_term in grammar}

    changed = True
    while changed:
        changed = False
        for non_term, rules in grammar.items():
            for rule in rules:
                if rule[-1].islower():  # Если последний символ терминал
                    if rule[-1] not in last[non_term]:
                        last[non_term].add(rule[-1])
                        changed = True
                elif rule[-1].isupper():  # Если последний символ нетерминал
                    size_before = len(last[non_term])
                    last[non_term].update(last[rule[-1]])
                    if len(last[non_term]) > size_before:
                        changed = True
    return last


def compute_precede(grammar):
    precede = {non_term: set() for non_term in grammar}

    changed = True
    while changed:
        changed = False
        for non_term, rules in grammar.items():
            for rule in rules:
                for i in range(1, len(rule)):
                    if rule[i].isupper():
                        size_before = len(precede[rule[i]])
                        precede[rule[i]].update(compute_last({rule[i - 1]: [rule[:i]]})[rule[i - 1]])
                        if len(precede[rule[i]]) > size_before:
                            changed = True
    return precede


def build_bigram(grammar, first, follow, last, precede, start_symbol):
    # Собираем все терминалы из грамматики
    terminals = set()
    for rules in grammar.values():
        for rule in rules:
            for symbol in rule:
                if symbol.islower():  # Терминалы
                    terminals.add(symbol)

    # Матрица биграмм
    bigram_matrix = {}

    # Перебираем все пары терминалов
    for gamma1 in terminals:
        for gamma2 in terminals:
            # Условие 1: γ1γ2 встречается в правой части правила грамматики
            condition1 = any(
                gamma1 + gamma2 in "".join(rule) for rules in grammar.values() for rule in rules
            )

            # Условие 2: ∃A1(γ1 ∈ Last(A1) & γ2 ∈ Follow(A1))
            condition2 = any(
                gamma1 in last[A1] and gamma2 in follow[A1] for A1 in grammar
            )

            # Условие 3: ∃A2(γ1 ∈ Precede(A2) & γ2 ∈ First(A2))
            condition3 = any(
                gamma1 in precede[A2] and gamma2 in first[A2] for A2 in grammar
            )

            # Условие 4: ∃A1, A2(γ1 ∈ Last(A1) & γ2 ∈ First(A1) & A2 ∈ Follow(A1))
            condition4 = any(
                gamma1 in last[A1] and gamma2 in first[A1] and A2 in follow[A1]
                for A1 in grammar
                for A2 in grammar
            )

            # Добавляем биграмму в матрицу, если выполняется хотя бы одно из условий
            if condition1 or condition2 or condition3 or condition4:
                bigram_matrix[(gamma1, gamma2)] = True
            else:
                bigram_matrix[(gamma1, gamma2)] = False

    # Определяем стартовые и финальные терминалы
    start_terminals = first[start_symbol]
    final_terminals = last[start_symbol]

    # Возвращаем матрицу биграмм, стартовые и финальные терминалы
    return bigram_matrix, start_terminals, final_terminals


def generate_tests_from_matrix(grammar, start_symbol, num_tests=10):
    first = compute_first(grammar)
    follow = compute_follow(grammar, start_symbol, first)
    last = compute_last(grammar)
    precede = compute_precede(grammar)

    bigram_matrix, start_terminals, final_terminals = build_bigram(
        grammar, first, follow, last, precede, start_symbol
    )

    tests = []
    i = 0
    while i < num_tests:
        test = []
        current_terminal = random.choice(list(start_terminals))  # Выбираем стартовый терминал
        test.append(current_terminal)

        while True:
            # С вероятностью 90% выбираем терминал из матрицы, 10% -- случайный терминал
            if random.random() < 0.9:
                # Выбираем следующий терминал, согласованный с матрицей
                next_terminals = [
                    gamma2 for (gamma1, gamma2), valid in bigram_matrix.items()
                    if gamma1 == current_terminal and valid
                ]
                if not next_terminals:
                    break
                current_terminal = random.choice(next_terminals)
            else:
                # Выбираем случайный терминал
                current_terminal = random.choice(list(bigram_matrix.keys()))[1]

            test.append(current_terminal)

            # Завершаем строку, если текущий терминал является финальным
            if current_terminal in final_terminals:
                break

        test_str = "".join(test)
        # проверка сгенирированного теста на уникальность
        # if test_str not in tests:
        #   print(test_str in tests, test_str, tests)
        i += 1
        tests.append(test_str)

    return tests
