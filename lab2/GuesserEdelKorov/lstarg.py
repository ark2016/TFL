from random import randint

def membership_query_fn(word):
    #  Code for membership querry
    return randint(0,2)

def equivalence_query_fn(hypothesis):
    # Code for equivalence query

    equal, counterexample = True, ""  # Here you need to recive the answer from MAT
    if not equal :
        # if it is equal
        return True, None
    else:
        return False, "ababa"  # returning the conterexample

# Class of SIMPLE L* Algorithm (TEST VERSION)
class LStarAlgorithm:
    def __init__(self, A, membership_query_fn, equivalence_query_fn):
        self.A = A  # alphabet
        self.membership_query = membership_query_fn  # membership querry
        self.equivalence_query = equivalence_query_fn  # equivalence query
        
        self.S = {''}  # the set S is initialized as λ
        self.E = {''}  # the set E is initialized as λ
        self.T = {}    # the observation the set S is initialized as λ (S, E, T)
        
        # table initialization
        self.initialize_table()

    def initialize_table(self):
        # it looks like (in brain :-} ):
        # S\E   λ
        #  λ  f(λ,λ)
        # where f is result {0,1} of membership query
        for s in self.S:
            for e in self.E:
                self.T[(s, e)] = self.membership_query(s + e)

    # checking for closeness
    def is_closed(self):
        # theory:
        # T - our observation table 
        # we need to check 
        # if for all strings s.a(s in S, a in A, s.a = s + a) 
        # there is s'(s' in S) that raw(s') = raw(s).a)
        for s1 in self.S:
            for a in self.A:
                row_s1a = self.get_row(s1 + a)
                # if there is not at least one
                if not any(self.get_row(s) == row_s1a for s in self.S):
                    return False, s1, a
        return True, None, None

    def is_consistent(self):
        # theory:
        # we need to check
        # if row(s1) = row(s2) is executed for 
        # all combinations of strings s1,s2, 
        # then raw(s1.a) = raw(s2.a) must be executed for any a
        for s1 in self.S:
            for s2 in self.S:
                if self.get_row(s1) == self.get_row(s2):
                    for a in self.A:
                        for e in self.E:
                            if self.T[(s1 + a, e)] != self.T[(s2 + a, e)]:
                                return False, s1, s2, a, e
        return True, None, None, None, None

    def get_row(self, s):
        # returning the raw from observation table (default = '?')
        return tuple(self.T.get((s, e), '?') for e in self.E)

    def extend_table(self):
        # Обновляем таблицу запросов членства для всех элементов (S ∪ S ⋅ A) ⋅ E
        for s in self.S:
            for a in self.A:
                for e in self.E:
                    self.T[(s + a, e)] = self.membership_query(s + a + e)

    def run(self):
        while True:
            # Step 1: check closeness
            closed, s1, a = self.is_closed()
            if not closed:
                # adding s1 ⋅ a в S (need check)
                self.S.add(s1 + a)
                self.extend_table()
                continue

            # Step 2: check consistent
            consistent, s1, s2, a, e = self.is_consistent()
            if not consistent:
                # adding a ⋅ e в E (need check)
                self.E.add(a + e)
                self.extend_table()
                continue

            # Шаг 3: equal query
            hypothesis = self.build_hypothesis()
            equivalent, counterexample = self.equivalence_query(hypothesis)
            if equivalent:
                print("Алгоритм завершен. Модель правильна.")
                return hypothesis
            else:
                # adding conterexample and it's prefix to S
                for i in range(len(counterexample) + 1):
                    self.S.add(counterexample[:i])
                self.extend_table()

    def build_hypothesis(self):
        equivalence_classes = []
        for s in self.S:
            state = self.T.get(s, '')
            equivalence_classes.append((s, state))
        return equivalence_classes

# start
A = {'a', 'b'}
l_star = LStarAlgorithm(A, membership_query_fn, equivalence_query_fn)
final_hypothesis = l_star.run()
