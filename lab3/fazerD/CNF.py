import copy


def remove_large_rules(rules, letters_pool, vocabulary):
    # удаляет длинные правила (длиной > 2) из грамматики
    new_rules = copy.deepcopy(rules)
    for non_terminal, productions in list(new_rules.items()):
        for production in productions:
            if len(production) > 2:
                prev_non_terminal = non_terminal
                for i in range(len(production) - 2):
                    new_non_terminal = letters_pool.pop()
                    vocabulary.append(new_non_terminal)
                    rules.setdefault(prev_non_terminal, []).append([production[i], new_non_terminal])
                    prev_non_terminal = new_non_terminal
                rules.setdefault(prev_non_terminal, []).append(production[-2:])
                rules[non_terminal].remove(production)
    return rules, letters_pool, vocabulary


def remove_empty_rules(rules, vocabulary):
    # Удаляет ε-правила (A → ε) из грамматики.
    nullable = set()

    # Шаг 1: Найти все nullable нетерминалы
    for non_terminal, productions in rules.items():
        for production in productions:
            if production == []:  # ε-правило
                nullable.add(non_terminal)

    updated = True
    while updated:
        updated = False
        for non_terminal, productions in rules.items():
            if non_terminal not in nullable:
                for production in productions:
                    if all(symbol in nullable for symbol in production):
                        nullable.add(non_terminal)
                        updated = True

    # Шаг 2: Удалить ε-правила
    new_rules = {}
    for non_terminal, productions in rules.items():
        new_rules[non_terminal] = []
        for production in productions:
            if production == []:
                continue
            subsets = [[]]
            for symbol in production:
                if symbol in nullable:
                    subsets = [s + [symbol] for s in subsets] + [s for s in subsets]
                else:
                    subsets = [s + [symbol] for s in subsets]
            new_rules[non_terminal].extend([s for s in subsets if s])

    return new_rules, vocabulary


def remove_short_rules(rules, vocabulary):
    # Удаляет цепные правила (A → B) из грамматики.
    reachable = {non_terminal: set() for non_terminal in vocabulary}

    # Найти все цепные пары
    for non_terminal in vocabulary:
        reachable[non_terminal].add(non_terminal)

    updated = True
    while updated:
        updated = False
        for non_terminal, productions in rules.items():
            current_reachable = reachable[non_terminal].copy()  # Копия множества
            for production in productions:
                if len(production) == 1 and production[0] in vocabulary:
                    target = production[0]
                    for r in current_reachable:  # Обход по копии множества
                        if target not in reachable[r]:
                            reachable[r].add(target)
                            updated = True

    # Удалить цепные правила
    new_rules = {non_terminal: [] for non_terminal in vocabulary}
    for non_terminal, targets in reachable.items():
        for target in targets:
            if target in rules:
                for production in rules[target]:
                    if len(production) != 1 or production[0] not in vocabulary:
                        if production not in new_rules[non_terminal]:
                            new_rules[non_terminal].append(production)

    return new_rules


def remove_useless_symbols(grammar, start_symbol):
    # Шаг 1: Удаление непорождающих символов
    productive = set()

    # Инициализация продуктивных символов
    while True:
        new_productive = productive.copy()
        for non_terminal, rules in grammar.items():
            for rule in rules:
                if all(symbol in productive or symbol.islower() for symbol in rule):
                    new_productive.add(non_terminal)
        if new_productive == productive:
            break
        productive = new_productive

    # Удаляем непродуктивные символы и их правила
    grammar = {
        non_terminal: [
            rule for rule in rules if all(symbol in productive or symbol.islower() for symbol in rule)
        ]
        for non_terminal, rules in grammar.items()
        if non_terminal in productive
    }

    # Шаг 2: Удаление недостижимых символов
    reachable = {start_symbol}
    while True:
        new_reachable = reachable.copy()
        for non_terminal in reachable:
            if non_terminal in grammar:
                for rule in grammar[non_terminal]:
                    for symbol in rule:
                        if symbol.isupper():
                            new_reachable.add(symbol)
        if new_reachable == reachable:
            break
        reachable = new_reachable

    # Удаляем недостижимые символы и их правила
    grammar = {
        non_terminal: [
            rule for rule in rules if all(symbol in reachable or symbol.islower() for symbol in rule)
        ]
        for non_terminal, rules in grammar.items()
        if non_terminal in reachable
    }

    # Удаление пустых правил
    grammar = {non_terminal: rules for non_terminal, rules in grammar.items() if rules}

    # Если после удаления всех правил грамматика пуста, возвращаем пустую грамматику
    if not grammar:
        return {}

    return grammar


def remove_terminal_repetitions(grammar, letters_pool):
    # Генерация уникальных имен для новых нетерминалов
    new_non_terminals = {}
    terminal_to_non_terminal = {}
    non_terminal_count = 0
    letter = letters_pool.pop()

    def get_new_non_terminal():
        nonlocal non_terminal_count
        non_terminal_count += 1
        return f"{letter}{non_terminal_count}"

    # Шаг 1: Преобразуем все правила, заменяя терминалы на новые нетерминалы
    new_grammar = {}

    for non_terminal, rules in grammar.items():
        new_grammar[non_terminal] = []
        for rule in rules:
            new_rule = []
            if len(rule) == 1:
                new_grammar[non_terminal].append(rule)
                continue
            for symbol in rule:
                if symbol.islower():  # если символ терминальный
                    if symbol not in terminal_to_non_terminal:
                        # Создаем новый нетерминал для данного терминала
                        new_non_terminal = get_new_non_terminal()
                        terminal_to_non_terminal[symbol] = new_non_terminal
                        # Добавляем правило для нового нетерминала
                        new_grammar[new_non_terminal] = [[symbol]]
                    # Заменяем терминал на новый нетерминал
                    new_rule.append(terminal_to_non_terminal[symbol])
                else:
                    new_rule.append(symbol)
            # Добавляем измененное правило
            new_grammar[non_terminal].append(new_rule)

    # Шаг 2: Возвращаем новую грамматику
    return new_grammar


def print_grammar(rules):
    """Печатает грамматику в читабельном формате."""
    for non_terminal, productions in rules.items():
        for production in productions:
            print(f"{non_terminal} → {''.join(production) if production else 'ε'}")


# Основная программа
def CFG_to_CNF(grammar, start_symbol):
    # Входная грамматика

    vocabulary = list(grammar.keys())
    letters_pool = [chr(i) for i in range(65, 91) if chr(i) not in vocabulary]  # A-Z, исключая уже используемые символы

    # Удаление длинных правил
    grammar, letters_pool, vocabulary = remove_large_rules(grammar, letters_pool, vocabulary)

    # Удаление ε-правил
    grammar, vocabulary = remove_empty_rules(grammar, vocabulary)

    # Удаление цепных правил
    grammar = remove_short_rules(grammar, vocabulary)

    # Удаляем бесполезные символы
    grammar = remove_useless_symbols(grammar, start_symbol)

    grammar = remove_terminal_repetitions(grammar, letters_pool)

    if len(grammar) == 2 and len(grammar[start_symbol][0]) == 1:
        symbol = grammar[start_symbol][0][0]
        grammar[start_symbol] = grammar[grammar[start_symbol][0][0]]
        grammar.pop(symbol)
    return grammar
