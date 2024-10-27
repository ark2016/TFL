from random import randint
# from unittest import expectedFailure
import requests
import json

# Класс для обращениѝ к мату Реликта (Для тестированиѝ базовой версии)
class RelictMatQueryModel:
    def __init__(self):
        self.result = "1"
        self.error_code = "! :-( !"

    def membershipQuery(self, word):
        # word = word.replace("", "ε")
        if word == "":
            word = "ε"
        
        query_body = {
            "word": word
        }
        try:
            response = requests.post(
                "http://127.0.0.1:8095/checkWord", 
                data=json.dumps(query_body), 
                headers={"Content-Type": "application/json"}
            )
            # print("HTTP Status Code:", response.status_code)
            # print("Response Content:", response.content)
            # print(response.content)
            response_json = response.json()
            # print(response_json["response"])
            # print(word, response_json["response"])
            return response_json["response"]
        except Exception as e:
            print("Произошла ошибка при проверке на включение :-( =>", e)
            return None

    def equivalenceQuery(self, S, S_extra, E, table):
        query_body = {
            "main_prefixes": S,
            "non_main_prefixes": S_extra,
            "suffixes": E,
            "table": table
        }
        # print(query_body)
        try:
            response = requests.post(
                "http://127.0.0.1:8095/checkTable", 
                data=json.dumps(query_body), 
                headers={"Content-Type": "application/json"}
            )
            # print("HTTP Status Code:", response.status_code)
            # print("Response Content:", response.content)
            
        except Exception as e:
            print("Произошла ошибка при проверке на эквивалентность :-( =>", e)
            return None
        # print(response)
        response = response.json()
        # print(response)
        if response["type"]==True and response["response"] != None:
            # print("лабиринт МАТа имеет слова, которые не принадлежат языку пользователѝ, контрпример:", response["response"])
            return (0, response["response"])
            
        elif response["type"] == False:
            # print(response["response"])
            return (1, response["response"])
        else:
            return (2, "")


# Class of SIMPLE L* Algorithm
class LStarAlgorithm:
    def __init__(self, A, mat_model):
        self.mat = mat_model
        self.A = A  # alphabet
        self.S = [""]  # префиксы, начинаются с ε
        self.E = [""]  # суффиксы, начинаются с ε
        self.T = {}    # вся таблица (основнаѝ + расширеннаѝ часть)
        
        # инициализациѝ таблицы
        self.initialize_table()

    def initialize_table(self):
        # S\E   ε
        #  ε  f(ε,ε)
        # где f - результат {0,1} от membership query
        for s in self.S:
            for e in self.E:
                self.T[(s, e)] = self.mat.membershipQuery(s + e)

    # проверка на полноту
    def is_closed(self):
        # теория:
        # T - таблица
        # необходимо проверить
        # Если для всех строк s.a(s in S, a in A, s.a = s + a) 
        # существует s'(s' in S)  row(s') = row(s).a)
        for s1 in self.S:
            for a in self.A:
                row_s1a = self.get_row(s1 + a)
                # еѝли ни одной
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
        # возращает ѝтроку из таблицы T
        return tuple(self.T.get((s, e), '?') for e in self.E)

    def extend_table(self):  
        for s in self.S:
            for a in ([""] + self.A ):
                for e in self.E:
                    if (s + a, e) not in self.T:
                        self.T[(s + a, e)] = self.mat.membershipQuery(s + a + e)

    def run(self):
        # i = -1
        # while True:
        #     # i+=1
        for i in range(1000):
            # print("iteration i: ", i)
            # print("WHILE")
            # print(self.S)
            # print(self.E)
            # print(self.T)
            # print(self.E)
            # print(self.T)
            # print()
            # Step 1: проверка на полноту
            closed, s1, a = self.is_closed()
            if not closed:
                # добавляем s1 ⋅ a в S 
                self.S.append(s1 + a)
                self.extend_table()
                continue

            # Step 2: проверка на противоричивость 
            consistent, s1, s2, a, e = self.is_consistent()
            if not consistent:
                # добавляем a ⋅ e в E
                self.E.append(a + e)
                self.extend_table()
                continue

            # Шаг 3: equal query
            hypothesis = self.build_hypothesis()
            equivalent, counterexample = self.mat.equivalenceQuery(*hypothesis)
            if equivalent == 2:
                print("Алгоритм завершен. Модель правильна.")
                return hypothesis
            elif equivalent == 1:
                # adding conterexample and it's prefix to S
                for i in range(1, len(counterexample) + 1):
                    temp = counterexample[:i]
                    if not(temp in self.S):
                        self.S.append(temp)
                self.extend_table()
            elif equivalent == 0:
                # adding conterexample and it's suffix to E
                for i in range(0, len(counterexample)):
                    temp = counterexample[i:]
                    if not(temp in self.E):
                        self.E.append(temp)
                self.extend_table()

    def build_hypothesis(self):
        non_main_prefixes = []
        for x in self.T.keys():
            if not(x[0] in self.S) and not(x[0] in non_main_prefixes):
                non_main_prefixes.append(x[0])
        table_string = ""
        for s in self.S:
            for e in self.E: 
                if self.T.get((s, e)):
                    table_string += "1 "
                else:
                    table_string += "0 "
        
        for s in non_main_prefixes:
            for e in self.E:
                if self.T.get((s, e)):
                    table_string += "1 "
                else:
                    table_string += "0 "
        
        Sm = ""
        for x in self.S:
            if x == "": Sm += "ε "
            else: Sm += str(x) + " "
        Em = ""
        for x in self.E:
            if x == "": Em += "ε "
            else: Em += str(x) + " "
        
        Nm = ""
        for x in non_main_prefixes:
            if x == "": Nm += "ε "
            else: Nm += str(x) + " "
        
        Sm = Sm[:len(Sm)-1]
        Em = Em[:len(Em)-1]
        Nm = Nm[:len(Nm)-1]
        table_string = table_string[:len(table_string)-1]
            
        # print("___________________")
        # print(Sm, self.S)
        # print(Em, self.E)
        # print(Nm, non_main_prefixes)
        # print(table_string)
        # print("___________________")

        return [Sm, Nm, Em, table_string]

# A = ['a', 'b', 'c', '0', '1', '2']
A = ["L", "R"]
mat_model = RelictMatQueryModel()
l_star = LStarAlgorithm(A, mat_model)

final_hypothesis = l_star.run()
print("Гипотеза:", final_hypothesis)