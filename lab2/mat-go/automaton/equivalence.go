package automaton

// EquivalenceTable проверяет эквивалентность автоматов по таблице эквивалентности
func (a *Automaton) EquivalenceTable(table map[string]bool) (bool, string) {
	for class, accept := range table {
		if a.AcceptString(class) != accept {
			return false, class // Возвращаем контрпример
		}
	}
	return true, "" // Автоматы эквивалентны
}
