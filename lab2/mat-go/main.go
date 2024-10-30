package main

import (
	"bufio"
	"fmt"
	"mat-go/automaton"
	"os"
	"strconv"
	"strings"
)

func main55() {
	// Определяем алфавит для автомата
	alphabet := []string{"a", "b", "c", "0", "1", "2"}

	// Генерация случайного автомата
	auto := automaton.RandomAutomaton(5, alphabet)

	// Визуализируем автомат
	err := auto.ToDot("automaton.dot")
	if err != nil {
		fmt.Println("Ошибка при создании .dot файла:", err)
		return
	}
	fmt.Println("Автомат сохранен в automaton.dot")

	// Работаем с вводом от пользователя
	scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Println("Введите запрос (include или equivalence) или 'exit' для выхода:")
		scanner.Scan()
		input := scanner.Text()

		if input == "exit" {
			break
		}

		// Определяем тип запроса
		if strings.HasPrefix(input, "include:") {
			// Запрос на включение
			processInclusionRequest(auto, strings.TrimPrefix(input, "include:"))
		} else if strings.HasPrefix(input, "equivalence:") {
			// Запрос на эквивалентность
			processEquivalenceRequest(auto, strings.TrimPrefix(input, "equivalence:"))
		} else {
			fmt.Println("Неизвестный запрос. Введите 'include:<строка>' для проверки включения или 'equivalence:<таблица>' для эквивалентности.")
		}
	}
}

// processInclusionRequest обрабатывает запросы на включение строки
func processInclusionRequest(auto *automaton.Automaton, input string) {
	input = strings.TrimSpace(input)
	isAccepted := auto.AcceptString(input)
	if isAccepted {
		fmt.Println(1) // 1 если строка принимается автоматом
	} else {
		fmt.Println(0) // 0 если строка не принимается
	}
}

// processEquivalenceRequest обрабатывает запросы на эквивалентность
func processEquivalenceRequest(auto *automaton.Automaton, tableInput string) {
	equivalenceTable := make(map[string]bool)
	lines := strings.Split(tableInput, "\n")
	for _, line := range lines {
		parts := strings.Split(line, "|")
		if len(parts) == 2 {
			class := strings.TrimSpace(parts[0])
			val, err := strconv.Atoi(strings.TrimSpace(parts[1]))
			if err != nil || (val != 0 && val != 1) {
				fmt.Println("Ошибка: Некорректная таблица эквивалентности.")
				return
			}
			equivalenceTable[class] = (val == 1)
		}
	}

	// Проверка эквивалентности по таблице
	isEquivalent, counterExample := auto.EquivalenceTable(equivalenceTable)
	if isEquivalent {
		fmt.Println("TRUE") // Автоматы эквивалентны
	} else {
		fmt.Printf("Контрпример: %s\n", counterExample) // Контрпример, если автоматы не эквивалентны
	}
}
