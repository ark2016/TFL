import random


class Automaton:
    def __init__(self, states=None, alphabet=None, transitions=None, initial_state=None, accepting_states=None):
        if accepting_states is None:
            accepting_states = set()
        if states is None:
            states = []
        if transitions is None:
            transitions = []
        if alphabet is None:
            alphabet = []
        self.accepting_states = accepting_states.copy()
        self.states = states.copy()
        self.alphabet = alphabet.copy()
        self.transitions = transitions.copy()
        self.num_states = len(states)
        self.num_transitions = len(transitions)
        self.initial_state = initial_state

    def fill_random_data(self, is_finite, special_alphabet=None):
        if special_alphabet is None:
            special_alphabet = []
        num_states = random.randint(3, max_lexem_size)
        num_transitions = random.randint(3, 10)
        self.num_states = num_states
        self.num_transitions = num_transitions
        self.states = [i for i in range(num_states)]
        self.transitions = []
        for i in range(self.num_transitions):
            if i == self.num_transitions - 1 and not is_finite:
                random_transition = random.choice(self.transitions)
                symbol = random.choice(self.alphabet)
                start_state = random_transition[2]
                end_state = random_transition[0]
                self.transitions.append((start_state, symbol, end_state))
                continue

            start_state = random.choice(self.states[:-1])
            end_state = random.choice([s for s in self.states if (s != start_state and not is_finite) or
                                       (s > start_state and is_finite)])
            symbol = random.choice(self.alphabet)
            self.transitions.append((start_state, symbol, end_state))
            self.transitions = list(set(self.transitions))
            if i == 0:
                self.initial_state = start_state

        end_states = []
        q = [self.initial_state]
        while len(q) > 0:
            for transition in self.transitions:
                if transition[0] == q[0] and transition[2] != self.initial_state and transition[2] not in end_states:
                    q.append(transition[2])
                    end_states.append(transition[2])
            q = q[1:]
        self.accepting_states = set(random.sample(end_states, k=random.randint(1, len(end_states))))
        if len(special_alphabet) > 0:
            self.states.append(num_states)
            self.states.append(num_states + 1)
            self.num_states += 2
            self.transitions.append((num_states, random.choice(special_alphabet), self.initial_state))
            self.num_transitions += 1
            self.initial_state = num_states
            for accept_state in self.accepting_states:
                self.transitions.append((accept_state, random.choice(special_alphabet), num_states + 1))
                self.num_transitions += 1
            self.accepting_states = {num_states + 1}
            self.alphabet += special_alphabet
        self.num_transitions = len(self.transitions)

    def is_empty_language(self):
        from collections import deque

        visited = set()
        queue = deque([self.initial_state])
        while queue:
            state = queue.popleft()
            if state not in visited:
                visited.add(state)
                for transition in self.transitions:
                    if transition[0] == state and transition[2] not in visited:
                        queue.append(transition[2])
        return not any(state in self.accepting_states for state in visited)

    def rename(self, offset):
        self.states = [i + offset for i in self.states]
        self.transitions = [(i[0] + offset, i[1], i[2] + offset) for i in self.transitions]
        self.initial_state += offset
        self.accepting_states = {i + offset for i in self.accepting_states}

    def copy(self):
        return Automaton(states=self.states.copy(), alphabet=self.alphabet.copy(), transitions=self.transitions.copy(),
                         initial_state=self.initial_state, accepting_states=self.accepting_states.copy())

    def __str__(self):
        str_transitions = ""
        for transition in self.transitions:
            str_transitions += str(transition[0]) + " -" + str(transition[1]) + "-> " + str(transition[2]) + '\n'
        return "States: " + str(self.states) + '\n' + "Alphabet: " + str(self.alphabet) + '\n' + \
            "Initial_state: " + str(self.initial_state) + '\n' + \
            "Accepting states: " + str(self.accepting_states) + '\n' + "Transitions:\n" + str_transitions


def concatenate(automaton_1, automaton_2, conn_symbol="#", accepting_states=None, initial_state=None):
    if accepting_states is None:
        accepting_states = automaton_1.accepting_states
    if initial_state is None:
        initial_state = automaton_2.initial_state
    new_transitions = automaton_1.transitions.copy()
    for accept_state in accepting_states:
        new_transitions.append((accept_state, conn_symbol, initial_state + automaton_1.num_states))
    for transition in automaton_2.transitions:
        start_state = transition[0]
        symbol = transition[1]
        end_state = transition[2]
        new_transitions.append((start_state + automaton_1.num_states, symbol, end_state + automaton_1.num_states))
    new_initial_state = automaton_1.initial_state
    new_accepting_states = set([i + automaton_1.num_states for i in automaton_2.accepting_states.copy()])
    new_states = automaton_1.states.copy() + [i + automaton_1.num_states for i in automaton_2.states]
    new_alphabet = list(set(automaton_1.alphabet + automaton_2.alphabet))
    return Automaton(states=new_states, alphabet=new_alphabet, transitions=new_transitions,
                     initial_state=new_initial_state, accepting_states=new_accepting_states)


def or_concatenate(list_of_automatons):
    new_automaton = Automaton(states=[0], initial_state=0)
    for automaton in list_of_automatons:
        automaton.rename(new_automaton.num_states)
        new_automaton.transitions += [(0, "#", automaton.initial_state)] + automaton.transitions.copy()
        new_automaton.num_transitions = len(new_automaton.transitions)
        new_automaton.states += automaton.states
        new_automaton.num_states = len(new_automaton.states)
        new_automaton.alphabet = list(set(new_automaton.alphabet + automaton.alphabet))
        new_automaton.accepting_states = new_automaton.accepting_states.union(automaton.accepting_states)
    return new_automaton


def intersection(automaton_1, automaton_2):
    states_intersection = [(q1, q2) for q1 in automaton_1.states for q2 in automaton_2.states]
    alphabet_intersection = automaton_1.alphabet
    transitions_intersection = []
    initial_state_intersection = (automaton_1.initial_state, automaton_2.initial_state)
    accepting_states_intersection = set([(q1, q2) for q1 in automaton_1.accepting_states for q2 in
                                         automaton_2.accepting_states])
    for transition_1 in automaton_1.transitions:
        for transition_2 in automaton_2.transitions:
            if transition_1[1] == transition_2[1] and transition_1[1] != "#":
                transitions_intersection.append(((transition_1[0], transition_2[0]), transition_1[1],
                                                 (transition_1[2], transition_2[2])))
    for state_1 in automaton_1.states:
        epsilon_closure_1 = []
        st = [state_1]
        while len(st) > 0:
            n_state = st.pop()
            for transition in automaton_1.transitions:
                if transition[0] == n_state and transition[2] not in epsilon_closure_1 and transition[1] == "#":
                    st.append(transition[2])
                    epsilon_closure_1.append(transition[2])
        for state_2 in automaton_2.states:
            epsilon_closure_2 = []
            st = [state_2]
            while len(st) > 0:
                n_state = st.pop()
                for transition in automaton_2.transitions:
                    if transition[0] == n_state and transition[2] not in epsilon_closure_2 and transition[1] == "#":
                        st.append(transition[2])
                        epsilon_closure_2.append(transition[2])
            for eps_state1 in epsilon_closure_1:
                for eps_state2 in epsilon_closure_2:
                    transitions_intersection.append(((state_1, state_2), "#", (eps_state1, eps_state2)))

    return Automaton(states=states_intersection, alphabet=alphabet_intersection, transitions=transitions_intersection,
                     initial_state=initial_state_intersection, accepting_states=accepting_states_intersection)


def generate_random_input_data():
    main_alphabet = ['a', 'b', 'c', '0', '1', '2']
    symbol = random.choice(main_alphabet)
    alphabet_1 = [symbol]
    main_alphabet.remove(symbol)
    symbol = random.choice(main_alphabet)
    alphabet_2 = [symbol]
    main_alphabet.remove(symbol)
    alphabet_3 = []
    for i in range(random.randint(1, 2)):
        symbol = random.choice(main_alphabet)
        alphabet_3.append(symbol)
        main_alphabet.remove(symbol)
    alphabet_4 = main_alphabet
    alphabets = [alphabet_4, alphabet_2, alphabet_3, alphabet_1]
    return alphabets


def generate_lexem_automatons():
    alphabets = generate_random_input_data()
    main_alphabet = alphabets[0]

    eol_automaton = Automaton(alphabet=alphabets[1])
    eol_automaton.fill_random_data(False)
    blank_automaton = Automaton(alphabet=alphabets[2])
    blank_automaton.fill_random_data(False)
    equal_automaton = Automaton(alphabet=main_alphabet)
    equal_automaton.fill_random_data(True, alphabets[3])
    sep_automaton = Automaton(alphabet=main_alphabet)
    sep_automaton.fill_random_data(True, alphabets[3])
    var_automaton = Automaton(alphabet=main_alphabet)
    var_automaton.fill_random_data(False)
    f = True
    const_automaton = Automaton(alphabet=main_alphabet)
    while f:
        const_automaton.fill_random_data(False)
        if intersection(var_automaton, const_automaton).is_empty_language():
            f = False
    list_automatons = [var_automaton, const_automaton]
    list_bracket_automatons = []
    lbr_1_automaton = Automaton(alphabet=main_alphabet)
    f = True
    while f:
        lbr_1_automaton.fill_random_data(True)
        if is_automaton_correct(lbr_1_automaton, list_automatons, list_bracket_automatons):
            f = False
    list_automatons.append(lbr_1_automaton)
    list_bracket_automatons.append(lbr_1_automaton)
    rbr_1_automaton = Automaton(alphabet=main_alphabet)
    f = True
    while f:
        rbr_1_automaton.fill_random_data(True)
        if is_automaton_correct(rbr_1_automaton, list_automatons, list_bracket_automatons):
            f = False
    list_automatons.append(rbr_1_automaton)
    list_bracket_automatons.append(rbr_1_automaton)
    rbr_2_automaton = Automaton(alphabet=main_alphabet)
    f = True
    while f:
        rbr_2_automaton.fill_random_data(True)
        if is_automaton_correct(rbr_2_automaton, list_automatons, list_bracket_automatons):
            f = False
    list_automatons.append(rbr_2_automaton)
    list_bracket_automatons.append(rbr_2_automaton)
    lbr_2_automaton = Automaton(alphabet=main_alphabet)
    f = True
    while f:
        lbr_2_automaton.fill_random_data(True)
        if is_automaton_correct(lbr_2_automaton, list_automatons, list_bracket_automatons):
            f = False
    list_automatons.append(lbr_2_automaton)
    list_bracket_automatons.append(lbr_2_automaton)
    lbr_3_automaton = Automaton(alphabet=main_alphabet)
    f = True
    while f:
        lbr_3_automaton.fill_random_data(True)
        if is_automaton_correct(lbr_3_automaton, list_automatons, list_bracket_automatons):
            f = False
    list_automatons.append(lbr_3_automaton)
    list_bracket_automatons.append(lbr_3_automaton)
    rbr_3_automaton = Automaton(alphabet=main_alphabet)
    f = True
    while f:
        rbr_3_automaton.fill_random_data(True)
        if is_automaton_correct(rbr_3_automaton, list_automatons, list_bracket_automatons):
            f = False
    '''
    for i in [eol_automaton, blank_automaton, equal_automaton, sep_automaton, lbr_1_automaton, lbr_2_automaton, 
              lbr_3_automaton, rbr_1_automaton, rbr_2_automaton, rbr_3_automaton, var_automaton, const_automaton]:
        print(i)
    '''
    generate_pattern_automaton(lbr_3_automaton, rbr_3_automaton, blank_automaton, var_automaton, const_automaton)
    generate_expression_automaton(var_automaton, const_automaton, blank_automaton, lbr_3_automaton, rbr_3_automaton,
                                  lbr_2_automaton, rbr_2_automaton)
    return eol_automaton, blank_automaton, equal_automaton, sep_automaton, var_automaton, const_automaton, \
        lbr_1_automaton, rbr_1_automaton, lbr_2_automaton, rbr_2_automaton, lbr_3_automaton, rbr_3_automaton


def generate_pattern_automaton(lbr_3_automaton, rbr_3_automaton, blank_automaton, var_automaton, const_automaton):
    a = or_concatenate([var_automaton.copy(), const_automaton.copy()])
    blank_automaton_copy = blank_automaton.copy()
    blank_automaton_copy.rename(a.num_states)
    a.transitions += blank_automaton_copy.transitions.copy()
    a.states += blank_automaton_copy.states.copy()
    a.alphabet = list(set(a.alphabet + blank_automaton_copy.alphabet))
    a.num_states = len(a.states)
    for state in a.accepting_states:
        a.transitions.append((state, "#", blank_automaton_copy.initial_state))
    for state in blank_automaton_copy.accepting_states:
        a.transitions.append((state, "#", 0))
    a.num_transitions = len(a.transitions)
    b_example = concatenate(a.copy(), rbr_3_automaton.copy())
    b = b_example.copy()
    n = max_bracket_depth
    for i in range(n - 1):
        b1 = b_example.copy()
        b = concatenate(lbr_3_automaton.copy(), b.copy())
        b.rename(b1.num_states)
        b1.states += b.states.copy()
        b1.alphabet = list(set(b1.alphabet + b.alphabet))
        b1.transitions += b.transitions.copy()
        for accept_state in b.accepting_states:
            b1.transitions.append((accept_state, "#", rbr_3_automaton.initial_state + a.num_states))
        b1.transitions.append((b1.initial_state, '#', b.initial_state))
        b1.num_states = len(b1.states)
        b1.num_transitions = len(b1.transitions)
        b = b1.copy()
    b = concatenate(lbr_3_automaton.copy(), b.copy())
    b.rename(a.num_states)
    a.states += b.states
    a.transitions += b.transitions.copy()
    a.alphabet = list(set(a.alphabet + b.alphabet))
    a.accepting_states = a.accepting_states.union(b.accepting_states)
    a.transitions.append((a.initial_state, "#", b.initial_state))
    return a


def generate_expression_automaton(var_automaton, const_automaton, blank_automaton, lbr_3_automaton, rbr_3_automaton,
                                  lbr_2_automaton, rbr_2_automaton):
    a = or_concatenate([var_automaton.copy(), const_automaton.copy()])
    blank_automaton_copy = blank_automaton.copy()
    blank_automaton_copy.rename(a.num_states)
    a.transitions += blank_automaton_copy.transitions.copy()
    a.states += blank_automaton_copy.states.copy()
    a.alphabet = list(set(a.alphabet + blank_automaton_copy.alphabet))
    a.num_states = len(a.states)
    for state in a.accepting_states:
        a.transitions.append((state, "#", blank_automaton_copy.initial_state))
    for state in blank_automaton_copy.accepting_states:
        a.transitions.append((state, "#", 0))
    a.num_transitions = len(a.transitions)
    b = concatenate(lbr_2_automaton.copy(), concatenate(const_automaton.copy(),
                                                        concatenate(blank_automaton.copy(),
                                                                    concatenate(a.copy(),
                                                                                rbr_2_automaton.copy()))))
    c = concatenate(lbr_3_automaton, concatenate(a.copy(), rbr_3_automaton.copy()))
    a2 = or_concatenate([b.copy(), c.copy()])
    main_part = a.copy()
    a2.rename(main_part.num_states)
    main_part.states += a2.states.copy()
    main_part.num_states = len(main_part.states)
    main_part.alphabet = list(set(main_part.alphabet.copy() + a2.alphabet.copy()))
    main_part.transitions += a2.transitions.copy()
    main_part.transitions.append((main_part.initial_state, "#", a2.initial_state))
    main_part.num_transitions = len(main_part.transitions)
    main_part.accepting_states = main_part.accepting_states.union(a2.accepting_states)
    key_init_states = [blank_automaton.num_states + const_automaton.num_states + lbr_2_automaton.num_states + 1 +
                       a.num_states, lbr_3_automaton.num_states + 1 + blank_automaton.num_states +
                       const_automaton.num_states + lbr_2_automaton.num_states + 2 * a.num_states]
    key_accept_states = [2 * a.num_states + lbr_2_automaton.num_states + const_automaton.num_states +
                         blank_automaton.num_states + 1, 3 * a.num_states + lbr_2_automaton.num_states +
                         const_automaton.num_states + blank_automaton.num_states + 1 + lbr_3_automaton.num_states +
                         rbr_2_automaton.num_states]
    n = max_bracket_depth
    lst = [(1, 0), (0, 1)]
    while len(key_init_states) > 0:
        new_lst = []
        new_key_initial_states = []
        new_key_accept_states = []
        for i in range(len(key_init_states)):
            if lst[i][0] < n and lst[i][1] < n:
                new_lst.append((lst[i][0] + 1, lst[i][1]))
                new_lst.append((lst[i][0], lst[i][1] + 1))
                a3 = or_concatenate([b.copy(), c.copy()])
                a3.rename(main_part.num_states)
                new_key_initial_states.append(a3.initial_state + 1 + blank_automaton.num_states +
                                              const_automaton.num_states + lbr_2_automaton.num_states)
                new_key_initial_states.append(a3.initial_state + 1 + b.num_states + lbr_3_automaton.num_states)
                new_key_accept_states.append(a.num_states + lbr_2_automaton.num_states + const_automaton.num_states +
                                             blank_automaton.num_states + 1 + main_part.num_states)
                new_key_accept_states.append(a.num_states + lbr_2_automaton.num_states + const_automaton.num_states +
                                             blank_automaton.num_states + 1 + main_part.num_states)
                new_key_accept_states.append(2 * a.num_states + lbr_2_automaton.num_states +
                                             const_automaton.num_states + blank_automaton.num_states + 1 +
                                             main_part.num_states + lbr_3_automaton.num_states +
                                             rbr_2_automaton.num_states)
                main_part.states += a3.states.copy()
                main_part.num_states = len(main_part.states)
                main_part.alphabet = list(set(main_part.alphabet.copy() + a3.alphabet.copy()))
                main_part.transitions += a3.transitions.copy()
                main_part.transitions.append((key_init_states[i], "#", a3.initial_state))
                for state in a3.accepting_states:
                    main_part.transitions.append((state, "#", key_accept_states[i]))
                main_part.num_transitions = len(main_part.transitions)

            elif lst[i][0] < n:
                new_lst.append((lst[i][0] + 1, lst[i][1]))
                a3 = b.copy()
                a3.rename(main_part.num_states)
                main_part.states += a3.states.copy()
                main_part.num_states = len(main_part.states)
                main_part.alphabet = list(set(main_part.alphabet.copy() + a3.alphabet.copy()))
                main_part.transitions += a3.transitions.copy()
                main_part.transitions.append((key_init_states[i], "#", a3.initial_state))
                for state in a3.accepting_states:
                    main_part.transitions.append((state, "#", key_accept_states[i]))
                main_part.num_transitions = len(main_part.transitions)
                new_key_initial_states.append(a3.initial_state + blank_automaton.num_states +
                                              const_automaton.num_states + lbr_2_automaton.num_states)
                new_key_accept_states.append(a.num_states + lbr_2_automaton.num_states + const_automaton.num_states +
                                             blank_automaton.num_states + main_part.num_states)
            elif lst[i][1] < n:
                new_lst.append((lst[i][0], lst[i][1] + 1))
                new_lst.append((lst[i][0] + 1, lst[i][1]))
                a3 = c.copy()
                a3.rename(main_part.num_states)
                main_part.states += a3.states.copy()
                main_part.num_states = len(main_part.states)
                main_part.alphabet = list(set(main_part.alphabet.copy() + a3.alphabet.copy()))
                main_part.transitions += a3.transitions.copy()
                main_part.transitions.append((key_init_states[i], "#", a3.initial_state))
                for state in a3.accepting_states:
                    main_part.transitions.append((state, "#", key_accept_states[i]))
                main_part.num_transitions = len(main_part.transitions)
                new_key_initial_states.append(a3.initial_state + lbr_3_automaton.num_states)
                new_key_accept_states.append(a.num_states + lbr_3_automaton.num_states + main_part.num_states)

        key_init_states = new_key_initial_states.copy()
        key_accept_states = new_key_accept_states.copy()
        lst = new_lst.copy()
    return main_part


def generate_main_automaton():
    eol_automaton, blank_automaton, equal_automaton, sep_automaton, var_automaton, const_automaton, \
        lbr_1_automaton, rbr_1_automaton, lbr_2_automaton, rbr_2_automaton, lbr_3_automaton, \
        rbr_3_automaton = generate_lexem_automatons()
    pattern_automaton = generate_pattern_automaton(lbr_3_automaton.copy(), rbr_3_automaton.copy(),
                                                   blank_automaton.copy(), var_automaton.copy(), const_automaton.copy())
    expression_automaton = generate_expression_automaton(var_automaton.copy(), const_automaton.copy(),
                                                         blank_automaton.copy(), lbr_3_automaton.copy(),
                                                         rbr_3_automaton.copy(), lbr_2_automaton.copy(),
                                                         rbr_2_automaton.copy())
    sentence_automaton = concatenate(pattern_automaton.copy(), concatenate(equal_automaton.copy(), concatenate(expression_automaton.copy(),
                                                                                                 sep_automaton.copy())))
    definition_automaton = concatenate(const_automaton.copy(), lbr_1_automaton.copy())
    eol_automaton_1 = eol_automaton.copy()
    eol_automaton_1.rename(1)
    eol_automaton_1.states = [0] + eol_automaton_1.states
    eol_automaton_1.transitions.append((0, '#', eol_automaton_1.initial_state))
    eol_automaton_1.states.append(eol_automaton_1.num_states + 1)
    for state in eol_automaton_1.accepting_states:
        eol_automaton_1.transitions.append((state, "#", eol_automaton_1.initial_state))
        eol_automaton_1.transitions.append((state, "#", eol_automaton_1.num_states + 1))
    eol_automaton_1.transitions.append((0, "#", eol_automaton_1.num_states + 1))
    eol_automaton_1.initial_state = 0
    eol_automaton_1.accepting_states = {eol_automaton_1.num_states + 1}
    eol_automaton_1.num_states = len(eol_automaton_1.states)
    eol_automaton_1.num_transitions = len(eol_automaton_1.transitions)
    definition_automaton_part = concatenate(eol_automaton_1.copy(), sentence_automaton.copy())
    definition_automaton_part.rename(1)
    definition_automaton_part.states = [0] + definition_automaton_part.states
    definition_automaton_part.transitions.append((0, '#', definition_automaton_part.initial_state))
    definition_automaton_part.states.append(definition_automaton_part.num_states + 1)
    for state in definition_automaton_part.accepting_states:
        definition_automaton_part.transitions.append((state, "#", definition_automaton_part.initial_state))
        definition_automaton_part.transitions.append((state, "#", definition_automaton_part.num_states + 1))
    definition_automaton_part.transitions.append((0, "#", definition_automaton_part.num_states + 1))
    definition_automaton_part.initial_state = 0
    definition_automaton_part.accepting_states = {definition_automaton_part.num_states + 1}
    definition_automaton_part.num_states = len(definition_automaton_part.states)
    definition_automaton_part.num_transitions = len(definition_automaton_part.transitions)
    definition_automaton = concatenate(concatenate(concatenate(definition_automaton.copy(),
                                                               definition_automaton_part.copy()),
                                                   eol_automaton_1.copy()), rbr_1_automaton.copy())
    eol_automaton_2 = eol_automaton.copy()
    for state in eol_automaton_2.accepting_states:
        eol_automaton_2.transitions.append((state, "#", eol_automaton_2.initial_state))
    eol_automaton_2.num_transitions = len(eol_automaton_2.transitions)
    program_automaton_part = concatenate(definition_automaton.copy(), eol_automaton_2.copy())
    for state in program_automaton_part.accepting_states:
        program_automaton_part.transitions.append((state, "#", program_automaton_part.initial_state))
    program_automaton = concatenate(eol_automaton_1.copy(), program_automaton_part.copy())
    return program_automaton


def is_automaton_correct(automaton, other_automatons, automaton_for_concatenate):
    list_of_concatenated_automatons = [concatenate(automaton, i) for i in automaton_for_concatenate]
    a = []
    for automaton1 in other_automatons + [automaton]:
        for automaton2 in list_of_concatenated_automatons:
            a.append(intersection(automaton1, automaton2).is_empty_language())
    return all([intersection(automaton, i).is_empty_language() for i in other_automatons]) and all(a)


f = open("parameters.txt", 'r')
max_bracket_depth, max_lexem_size = map(int, f.readline().split())
f.close()
a = generate_main_automaton()


