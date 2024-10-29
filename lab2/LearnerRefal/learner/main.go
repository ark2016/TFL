package main

import (
	"LearnerRefal/mat"
	"fmt"
	"strings"
)

type ObservationTable struct {
	S            map[string]bool
	E            map[string]bool
	T            map[string]map[string]int
	A            []string
	ExtendedPart map[string]bool
}

func NewObservationTable(alphabet []string) *ObservationTable {
	table := &ObservationTable{
		S:            make(map[string]bool),
		E:            make(map[string]bool),
		T:            make(map[string]map[string]int),
		A:            alphabet,
		ExtendedPart: make(map[string]bool),
	}

	table.S[""] = true
	table.E[""] = true
	epsMembership, err := mat.MembershipQuery("")
	if err != nil {
		panic(err)
	}
	table.T[""] = make(map[string]int)
	table.T[""][""] = epsMembership

	for s := range table.S {
		if table.T[s] == nil {
			table.T[s] = make(map[string]int)
		}
		for _, a := range alphabet {
			extendedPref := s + a
			if !table.ExtendedPart[extendedPref] {
				result, err := mat.MembershipQuery(extendedPref)
				if err != nil {
					panic(err)
				}
				table.ExtendedPart[extendedPref] = true
				table.T[extendedPref] = make(map[string]int)
				table.T[extendedPref][""] = result
			}
		}
	}
	return table
}

func (table *ObservationTable) rowsEqual(s1, s2 string) bool {
	for e := range table.E {
		if table.T[s1][e] != table.T[s2][e] {
			return false
		}
	}
	return true
}

func (table *ObservationTable) isClosed() (bool, string) {
	for extendedPrefix := range table.ExtendedPart {
		matchFound := false
		for s := range table.S {
			if table.rowsEqual(s, extendedPrefix) {
				matchFound = true
				break
			}
		}
		if !matchFound {
			fmt.Printf("строка из доп.части, которой нет в основной: %s - %v\n", extendedPrefix, table.T[extendedPrefix])
			return false, extendedPrefix
		}
	}
	return true, ""
}

func (t *ObservationTable) isConsistent() (bool, string) {
	for s1 := range t.S {
		for s2 := range t.S {
			if s1 != s2 && t.rowsEqual(s1, s2) {
				for _, a := range t.A {
					for e := range t.E {
						if t.T[s1+a][e] != t.T[s2+a][e] {
							extendedSuf := a + e
							return false, extendedSuf
						}
					}
				}
			}
		}
	}
	return true, ""
}

func (table *ObservationTable) extendTableWithPrefixes() {
	for s := range table.S {
		for _, a := range table.A {
			extendedPref := s + a
			if !table.S[extendedPref] && !table.ExtendedPart[extendedPref] {
				table.ExtendedPart[extendedPref] = true

				if table.T[extendedPref] == nil {
					table.T[extendedPref] = make(map[string]int)
				}
				for e := range table.E {
					result, err := mat.MembershipQuery(extendedPref + e)
					if err != nil {
						panic(err)
					}
					table.T[extendedPref][e] = result
				}
			}
		}
	}
}
func (table *ObservationTable) extendTableWithNewSuffix(extendedSuf string) {
	for s := range table.S {
		if table.T[s] == nil {
			table.T[s] = make(map[string]int)
		}
		result, err := mat.MembershipQuery(s + extendedSuf)
		if err != nil {
			panic(err)
		}
		table.T[s][extendedSuf] = result
	}
	for extendedPrefix := range table.ExtendedPart {
		if table.T[extendedPrefix] == nil {
			table.T[extendedPrefix] = make(map[string]int)
		}
		result, err := mat.MembershipQuery(extendedPrefix + extendedSuf)
		if err != nil {
			panic(err)
		}
		table.T[extendedPrefix][extendedSuf] = result
	}
}

func (table *ObservationTable) addCounterExample(counterExample string) {
	// add counterExample and its suffixes to E
	for i := 0; i < len(counterExample); i++ {
		suffix := counterExample[i:]
		if !table.E[suffix] {
			table.E[suffix] = true
			table.extendTableWithNewSuffix(suffix)
		}
	}
}
func (table *ObservationTable) formQuery() (string, string, string, string) {
	mainPrefixes := make([]string, 0, len(table.S))
	nonMainPrefixes := make([]string, 0, len(table.ExtendedPart))
	suffixes := make([]string, 0, len(table.E))

	for prefix := range table.S {
		if prefix == "" {
			mainPrefixes = append(mainPrefixes, "ε")
		} else {
			mainPrefixes = append(mainPrefixes, prefix)
		}
	}
	for prefix := range table.ExtendedPart {
		if prefix == "" {
			nonMainPrefixes = append(nonMainPrefixes, "ε")
		} else {
			nonMainPrefixes = append(nonMainPrefixes, prefix)
		}
	}
	for suffix := range table.E {
		if suffix == "" {
			suffixes = append(suffixes, "ε")
		} else {
			suffixes = append(suffixes, suffix)
		}
	}

	tableEntries := make([]string, 0, (len(mainPrefixes)+len(nonMainPrefixes))*len(suffixes))
	for _, prefix := range mainPrefixes {
		for _, suffix := range suffixes {
			prefixToCheck := prefix
			if prefix == "ε" {
				prefixToCheck = ""
			}
			suffixToCheck := suffix
			if suffix == "ε" {
				suffixToCheck = ""
			}
			value, exists := table.T[prefixToCheck][suffixToCheck]
			if exists {
				tableEntries = append(tableEntries, fmt.Sprintf("%d", value))
			} else {
				tableEntries = append(tableEntries, "?")
			}
		}
	}
	for _, extendedPrefix := range nonMainPrefixes {
		for _, suffix := range suffixes {
			prefixToCheck := extendedPrefix
			if extendedPrefix == "ε" {
				prefixToCheck = ""
			}
			suffixToCheck := suffix
			if suffix == "ε" {
				suffixToCheck = ""
			}
			value, exists := table.T[prefixToCheck][suffixToCheck]
			if exists {
				tableEntries = append(tableEntries, fmt.Sprintf("%d", value))
			} else {
				tableEntries = append(tableEntries, "?")
			}
		}
	}
	mainPrefixesStr := strings.Join(mainPrefixes, " ")
	nonMainPrefixesStr := strings.Join(nonMainPrefixes, " ")
	suffixesStr := strings.Join(suffixes, " ")
	tableStr := strings.Join(tableEntries, " ")
	return mainPrefixesStr, nonMainPrefixesStr, suffixesStr, tableStr
}
func (table *ObservationTable) printObservationTable() {
	mainPrefixes, nonMainPrefixes, suffixes, tableStr := table.formQuery()
	fmt.Println("Main Prefixes:", mainPrefixes)
	fmt.Println("Non-Main Prefixes:", nonMainPrefixes)
	fmt.Println("Suffixes:", suffixes)
	fmt.Println("Table:", tableStr)
	fmt.Println("Таблица принадлежности (T):")
	for s, transitions := range table.T {
		fmt.Printf("T[%s]: %v\n", s, transitions)
	}
	fmt.Println()
}

func (table *ObservationTable) Learn(maxIter int) {
	iter := 0
	for iter < maxIter {
		fmt.Printf("Итерация %d:\n", iter+1)
		table.printObservationTable()
		closed, extendedPref := table.isClosed()
		if !closed {
			table.S[extendedPref] = true
			delete(table.ExtendedPart, extendedPref)
			fmt.Println("таблица неполная")
			table.extendTableWithPrefixes()
			continue
		}
		fmt.Println("таблица полная")
		consistent, extendedSuf := table.isConsistent()
		if !consistent {
			table.E[extendedSuf] = true
			table.extendTableWithNewSuffix(extendedSuf)
			fmt.Println("таблица противоречива")
			continue
		}
		mainPrefixes, nonMainPrefixes, suffixes, tableStr := table.formQuery()
		equivalent, counterexample, err := mat.EquivalenceQuery(mainPrefixes, nonMainPrefixes, suffixes, tableStr)
		if err != nil {
			fmt.Println("Ошибка при проверке эквивалентности:", err)
			return
		} else if equivalent {
			fmt.Println("Автомат эквивалентен.")
			return
		} else {
			fmt.Println("Найден контрпример:", counterexample)
			fmt.Println("Добавляем его со всеми его суффиксами в E")
			table.addCounterExample(counterexample)
			iter++
		}
	}
}

func main() {
	alphabet := []string{"L", "R"}
	observationTable := NewObservationTable(alphabet)

	observationTable.Learn(10)
}
