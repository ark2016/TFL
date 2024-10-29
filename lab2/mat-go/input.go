package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// State represents a state in DFA
type State struct {
	Name        string
	IsFinal     bool
	Transitions map[string]string // переходы по символам
}

// DFA represents the deterministic finite automaton.
type DFA struct {
	States       map[string]*State
	InitialState string
	Alphabet     []string
}

// NewDFA initializes a new DFA.
func NewDFA(alphabet []string) *DFA {
	return &DFA{
		States:   make(map[string]*State),
		Alphabet: alphabet,
	}
}

// AddState adds a new state to the DFA.
func (dfa *DFA) AddState(name string, isFinal bool) {
	dfa.States[name] = &State{
		Name:        name,
		IsFinal:     isFinal,
		Transitions: make(map[string]string),
	}
}

// AddTransition adds a transition between two states.
func (dfa *DFA) AddTransition(from string, input string, to string) {
	if _, ok := dfa.States[from]; ok {
		dfa.States[from].Transitions[input] = to
	}
}

// SetInitialState sets the initial state of the DFA.
func (dfa *DFA) SetInitialState(name string) {
	dfa.InitialState = name
}

// Display prints the DFA states and transitions.
func (dfa *DFA) Display() {
	fmt.Printf("Initial State: %s\n", dfa.InitialState)
	fmt.Println("States and Transitions:")
	for _, state := range dfa.States {
		finalState := ""
		if state.IsFinal {
			finalState = "(final)"
		}
		fmt.Printf("State %s %s:\n", state.Name, finalState)
		for input, to := range state.Transitions {
			fmt.Printf("  '%s' -> %s\n", input, to)
		}
	}
}

// ReadTable reads the equivalence table from standard input.
func ReadTable() (E, S, states []string, table [][]string) {
	scanner := bufio.NewScanner(os.Stdin)
	tableFormat :=
		"   | ε  | e1 | e2 | e3 | ... | en \n" +
			" ε | +  | +  | -  | +  | ... | + \n" +
			"s1 | +  | +  | -  | +  | ... | + \n" +
			"s2 | -  | +  | +  | +  | ... | + \n" +
			"s3 | +  | +  | -  | +  | ... | - \n" +
			"...|... |... |... |... | ... |... \n" +
			"sn | +  | +  | -  | +  | ... | +"
	fmt.Printf("Введите таблицу эквивалентности в формате \n%s \n", tableFormat)
	count := 0

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}
		parts := strings.Split(line, "|")
		for i, s := range parts {
			parts[i] = strings.TrimSpace(s)
		}

		if count == 0 {
			E = append(E, parts...)
		} else {
			state := parts[0]
			S = append(S, state)
			states = append(states, state)
			table = append(table, parts[1:])
		}
		count++
	}
	return E, S, states, table
}

func (e *EquivalenceClassesTable) BuildDFA1() StateMachine {
	countOfStates := -1
	finalStates := make(map[int]struct{})
	prefixToStateNum := make(map[string]int)
	used := make(map[string]struct{})

	for prefix, cls := range e.mainTable {
		if _, exists := used[cls]; !exists {
			countOfStates++
			used[cls] = struct{}{}
			prefixToStateNum[prefix] = countOfStates
			if cls[0] == '+' {
				finalStates[countOfStates] = struct{}{}
			}
		}
	}

	transitions := make([][]string, countOfStates+1)
	for i := range transitions {
		transitions[i] = make([]string, countOfStates+1)
	}

	for prefix, from := range prefixToStateNum {
		for _, letter := range e.alphabet {
			nextState := prefix + string(letter)
			to := prefixToStateNum[e.classesOfEquivalence[e.additionalTable[nextState]]]
			if transitions[from][to] == "" {
				transitions[from][to] = string(letter)
			} else {
				transitions[from][to] += " " + string(letter)
			}
		}
	}

	return StateMachine{transitions: transitions, finalStates: finalStates, stateCount: countOfStates}
}

func main() {
	// Чтение таблицы эквивалентности с ввода пользователя
	E, S, states, table := ReadTable()

	fmt.Println(E)
	fmt.Println(S)

	for _, elem := range table {
		fmt.Println(elem)
	}

	// Known alphabet {a, b, c, 0, 1, 2}
	alphabet := []string{"a", "b", "c", "0", "1", "2"}

	// Создание DFA
	dfa := NewDFA(alphabet)

	// Добавление состояний в DFA
	for i, state := range states {
		isFinal := (table[i][0] == "+") // Если для ε стоит '+', это финальное состояние
		dfa.AddState(state, isFinal)
	}

	// Установка начального состояния
	dfa.SetInitialState(states[0]) // Первое состояние (обычно ε) — начальное
	fmt.Println(states[0])

	// Добавление переходов на основе таблицы эквивалентности
	for i, state := range states {
		for j, symbol := range alphabet {
			if table[i][j] == "+" {
				dfa.AddTransition(state, symbol, states[j])
			}
		}
	}

	// Вывод автомата
	dfa.Display()
}

/*
  | ε  | a | b
ε | +  | +  | -
a | +  | +  | -
b | -  | +  | +
ab | +  | +  | -

*/
