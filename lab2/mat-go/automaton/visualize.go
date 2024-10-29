package automaton

import (
	"fmt"
	"os"
)

// ToDot сохраняет автомат в формате dot для визуализации
func (a *Automaton) ToDot(filename string) error {
	dot := "digraph Automaton {\n"
	dot += "    rankdir=LR;\n"
	dot += fmt.Sprintf("    start [shape=point];\n")
	dot += fmt.Sprintf("    start -> %s;\n", a.StartState)

	// Финальные состояния
	for state, isFinal := range a.FinalStates {
		if isFinal {
			dot += fmt.Sprintf("    %s [shape=doublecircle];\n", state)
		} else {
			dot += fmt.Sprintf("    %s [shape=circle];\n", state)
		}
	}

	// Переходы
	for state, transitions := range a.Transitions {
		for symbol, nextState := range transitions {
			dot += fmt.Sprintf("    %s -> %s [label=\"%s\"];\n", state, nextState, symbol)
		}
	}

	dot += "}\n"

	return writeFile(filename, dot)
}

// writeFile записывает строку в файл
func writeFile(filename, data string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	_, err = file.WriteString(data)
	return err
}
