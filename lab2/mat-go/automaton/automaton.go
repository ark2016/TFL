package automaton

import (
	"fmt"
	"math/rand"
	"time"
)

// Automaton представляет автомат с состояниями и переходами
type Automaton struct {
	States      []string
	Transitions map[string]map[string]string // Состояние -> символ -> новое состояние
	FinalStates map[string]bool
	Alphabet    []string
	StartState  string
}

// RandomAutomaton генерирует случайный автомат с учетом ограничений
func RandomAutomaton(numStates int, alphabet []string) *Automaton {
	rand.Seed(time.Now().UnixNano())

	states := make([]string, numStates)
	for i := range states {
		states[i] = fmt.Sprintf("q%d", i)
	}

	transitions := make(map[string]map[string]string)
	finalStates := make(map[string]bool)

	// Заполняем автомат случайными переходами
	for _, state := range states {
		transitions[state] = make(map[string]string)
		for _, symbol := range alphabet {
			nextState := states[rand.Intn(numStates)]
			transitions[state][symbol] = nextState
		}
	}

	// Устанавливаем случайные финальные состояния
	for _, state := range states {
		finalStates[state] = rand.Intn(2) == 1
	}

	startState := states[0]

	return &Automaton{
		States:      states,
		Transitions: transitions,
		FinalStates: finalStates,
		Alphabet:    alphabet,
		StartState:  startState,
	}
}

// AcceptString проверяет, принимает ли автомат строку
func (a *Automaton) AcceptString(input string) bool {
	currentState := a.StartState

	for _, symbol := range input {
		transitions := a.Transitions[currentState]
		nextState, exists := transitions[string(symbol)]
		if !exists {
			return false
		}
		currentState = nextState
	}

	return a.FinalStates[currentState]
}
