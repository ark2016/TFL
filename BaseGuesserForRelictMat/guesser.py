from random import randint
# from unittest import expectedFailure
import requests
import json

# Класс для обращения к мату Реликта (Для тестирования базовой версии)
class RelictMatQuerryModel:
    def __init__(self, path_to_mat):
        self.result = "1"
        self.error_code = "! :-( !"

    def membershipQuerry(self, word):
        querry_body = {
            "word": word
        }
        try:
            response = requests.get("/checkWorld", json.decoder(querry_body))
        except Exception as e:
            print("Произошел error при проверке на включение :-( => ", e)

        return response.json()["response"]

    def equivalenceQuerry(self, S, S_extra, E, table):
        querry_body = {
            "main_prefixes": S,
            "non_main_prefixes": S_extra,
            "suffixes": E, 
            "table": table 
        }
        try:
            response = requests.get("/checkTable", json.decoder(querry_body)).json()
            if response["type"] and response["response"]:
                    print("лабиринт МАТа имеет слова, которые не принадлежат языку пользователя, контрпример: ", response["response"])
            elif response["type"] == False:
                return (False, response["response"])
            else:
                return (True, "") 

        except Exception as e:
            print("Произошел error при проверке на эквивалентность :-( => ", e)

        return response.json()["response"]

# Class of SIMPLE L* Algorithm
class LStarAlgorithm:
    def __init__(self, A, mat_model):
        self.mat = mat_model
        self.A = A  # alphabet
        self.S = [""]  # префиксы, начинаются с ε
        self.E = [""]  # суффиксы, начинаются с ε
        self.T = {}    # вся таблица (основная + расширенная часть)
        
        # инициализация таблицы
        self.initialize_table()

    def initialize_table(self):
        # S\E   ε
        #  ε  f(ε,ε)
        # где f - результат {0,1} от membership query
        for s in self.S:
            for e in self.E:
                self.T[(s, e)] = self.mat.membershipQuerry(s + e)

    # проверка на полноту
    def is_closed(self):
        # теория:
        # T - таблица
        # неоюходимо проверить
        # Если для всех строк s.a(s in S, a in A, s.a = s + a) 
        # существует s'(s' in S)  row(s') = row(s).a)
        for s1 in self.S:
            for a in self.A:
                row_s1a = self.get_row(s1 + a)
                # если ни одной
                if not any(self.get_row(s) == row_s1a for s in self.S):
                    return False, s1, a
        return True, None, None

    # проверка на замкнутость
    def is_consistent(self):
        for i in range(len(self.S)):  
            for j in range(i + 1, len(self.S)):
                s1, s2 = self.S[i], self.S[j]
                if self.get_row(s1) == self.get_row(s2):
                    for a in self.A:
                        for e in self.E:
                            if self.T[(s1 + a, e)] != self.T[(s2 + a, e)]:
                                return False, s1, s2, a, e
        return True, None, None, None, None

    def get_row(self, s):
        # возращает строку из таблицы T
        return tuple(self.T.get((s, e), '?') for e in self.E)

    def extend_table(self):  
        for s in self.S:
            for a in self.A:
                for e in self.E:
                    if (s + a, e) not in self.T:
                        self.T[(s + a, e)] = self.mat.membershipQuerry(s + a + e)

    def run(self):
        while True:
            # Step 1: проверка на полноту
            closed, s1, a = self.is_closed()
            if not closed:
                # добавляем s1 ⋅ a в S 
                self.S.add(s1 + a)
                self.extend_table()
                continue

            # Step 2: проверка на противоричивость 
            consistent, s1, s2, a, e = self.is_consistent()
            if not consistent:
                # добавляем a ⋅ e в E
                self.E.add(a + e)
                self.extend_table()
                continue

            # Шаг 3: equal query
            hypothesis = self.build_hypothesis()
            equivalent, counterexample = self.mat.equivalence_query(hypothesis)
            if equivalent:
                print("Алгоритм завершен. Модель правильна.")
                return hypothesis
            else:
                # adding conterexample and it's prefix to S
                for i in range(len(counterexample) + 1):
                    self.S.add(counterexample[:i])
                self.extend_table()

    def build_hypothesis(self):
        non_main_prefixes = [x for x in self.T.keys() if not(x in self.S)]
        table_string = ""
        for s in self.S:
            for e in self.E:
                table_string += str(self.T.get((s, e)))
        
        for s in non_main_prefixes:
            for e in self.E:
                table_string += str(self.T.get((s, e)))
        
        return self.mat.equivalenceQuerry(self.S, non_main_prefixes, self.E, self.T)

# A = ['a', 'b', 'c', '0', '1', '2']
A = ["L", "R"]
mat_model = RelictMatQuerryModel()
l_star = LStarAlgorithm(A, mat_model)

final_hypothesis = l_star.run()
print("Гипотеза:", final_hypothesis)