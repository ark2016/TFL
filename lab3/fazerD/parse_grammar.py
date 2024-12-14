import re

def is_terminal(symbol):
    return re.match(r'^[a-z]$', symbol) is not None


def is_non_terminal(symbol):
    return re.match(r'^[A-Z][0-9]?|[A-Za-z]+[0-9]*$', symbol) is not None


# Основная функция для распознавания грамматики
def recognize_grammar(grammar_text):
    rules = {}
    for line in grammar_text.split('\n'):
        if line.strip():  # Пропускаем пустые строки
            try:
                # Убираем лишние пробелы в начале и конце правила
                line = line.strip()
                # Разделяем на левую и правую части
                parts = re.split(r'\s*->\s*', line)
                if len(parts) != 2:
                    raise ValueError("Неверный формат правила")

                left_part = parts[0].strip()
                if not is_non_terminal(left_part):
                    raise ValueError(f"Левая часть должна быть нетерминалом: {left_part}")

                right_part = re.findall(r'\S+', parts[1])
                if left_part not in rules:
                    rules[left_part] = []

                for symbol in right_part:
                    if not (is_terminal(symbol) or is_non_terminal(symbol)):
                        raise ValueError(f"Недопустимый символ в правой части: {symbol}")
                    mas_symbol = list(symbol)
                    if mas_symbol not in rules[left_part]:
                        rules[left_part].append(mas_symbol)

                if len(rules[left_part]) == 0:
                    rules.pop(left_part)

            except ValueError as e:
                print(f"Ошибка в правиле: {e}")
    return rules
