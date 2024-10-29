use std::collections::{HashMap, HashSet};
use crate::mat;

pub struct ObservationTable {
    s: HashMap<String, bool>,
    e: HashMap<String, bool>,
    t: HashMap<String, HashMap<String, i32>>,
    a: Vec<String>,
    extended_part: HashMap<String, bool>,
}

impl ObservationTable {
    pub fn new(alphabet: Vec<String>) -> Self {
        let mut table = ObservationTable {
            s: HashMap::new(),
            e: HashMap::new(),
            t: HashMap::new(),
            a: alphabet,
            extended_part: HashMap::new(),
        };

        table.s.insert("".to_string(), true);
        table.e.insert("".to_string(), true);

        let eps_membership = mat::membership_query("").expect("Ошибка выполнения MembershipQuery");
        table.t.insert("".to_string(), HashMap::new());
        table.t.get_mut("").unwrap().insert("".to_string(), eps_membership);

        for s in table.s.keys() {
            if !table.t.contains_key(s) {
                table.t.insert(s.clone(), HashMap::new());
            }
            for a in &table.a {
                let extended_pref = format!("{}{}", s, a);
                if !table.extended_part.contains_key(&extended_pref) {
                    let result = mat::membership_query(&extended_pref).expect("Ошибка выполнения MembershipQuery");
                    table.extended_part.insert(extended_pref.clone(), true);
                    table.t.insert(extended_pref.clone(), HashMap::new());
                    table.t.get_mut(&extended_pref).unwrap().insert("".to_string(), result);
                }
            }
        }
        table
    }

    fn rows_equal(&self, s1: &str, s2: &str) -> bool {
        for e in self.e.keys() {
            if self.t[s1][e] != self.t[s2][e] {
                return false;
            }
        }
        true
    }

    fn is_closed(&self) -> (bool, Option<String>) {
        for extended_prefix in self.extended_part.keys() {
            let mut match_found = false;
            for s in self.s.keys() {
                if self.rows_equal(s, extended_prefix) {
                    match_found = true;
                    break;
                }
            }
            if !match_found {
                println!("строка из доп.части, которой нет в основной: {} - {:?}", extended_prefix, self.t[extended_prefix]);
                return (false, Some(extended_prefix.clone()));
            }
        }
        (true, None)
    }

    fn is_consistent(&self) -> (bool, Option<String>) {
        for s1 in self.s.keys() {
            for s2 in self.s.keys() {
                if s1 != s2 && self.rows_equal(s1, s2) {
                    for a in &self.a {
                        for e in self.e.keys() {
                            let t_s1a = format!("{}{}", s1, a);
                            let t_s2a = format!("{}{}", s2, a);
                            if self.t[&t_s1a][e] != self.t[&t_s2a][e] {
                                let extended_suf = format!("{}{}", a, e);
                                return (false, Some(extended_suf));
                            }
                        }
                    }
                }
            }
        }
        (true, None)
    }

    fn extend_table_with_prefixes(&mut self) {
        for s in self.s.keys().cloned().collect::<Vec<_>>() {
            for a in &self.a {
                let extended_pref = format!("{}{}", s, a);
                if !self.s.contains_key(&extended_pref) && !self.extended_part.contains_key(&extended_pref) {
                    self.extended_part.insert(extended_pref.clone(), true);

                    if !self.t.contains_key(&extended_pref) {
                        self.t.insert(extended_pref.clone(), HashMap::new());
                    }
                    for e in self.e.keys() {
                        let result = mat::membership_query(&format!("{}{}", extended_pref, e)).expect("Ошибка выполнения MembershipQuery");
                        self.t.get_mut(&extended_pref).unwrap().insert(e.clone(), result);
                    }
                }
            }
        }
    }

    fn extend_table_with_new_suffix(&mut self, extended_suf: &str) {
        for s in self.s.keys().chain(self.extended_part.keys()) {
            if !self.t.contains_key(s) {
                self.t.insert(s.clone(), HashMap::new());
            }
            let result = mat::membership_query(&format!("{}{}", s, extended_suf)).expect("Ошибка выполнения MembershipQuery");
            self.t.get_mut(s).unwrap().insert(extended_suf.to_string(), result);
        }
    }

    fn add_counter_example(&mut self, counter_example: &str) {
        for i in 0..counter_example.len() {
            let suffix = &counter_example[i..];
            if !self.e.contains_key(suffix) {
                self.e.insert(suffix.to_string(), true);
                self.extend_table_with_new_suffix(suffix);
            }
        }
    }

    fn form_query(&self) -> (String, String, String, String) {
        let mut main_prefixes = Vec::new();
        let mut non_main_prefixes = Vec::new();
        let mut suffixes = Vec::new();
    
        for prefix in self.s.keys() {
            if prefix.is_empty() {
                main_prefixes.push("ε".to_string());
            } else {
                main_prefixes.push(prefix.clone());
            }
        }
    
        for prefix in self.extended_part.keys() {
            if prefix.is_empty() {
                non_main_prefixes.push("ε".to_string());
            } else {
                non_main_prefixes.push(prefix.clone());
            }
        }
    
        for suffix in self.e.keys() {
            if suffix.is_empty() {
                suffixes.push("ε".to_string());
            } else {
                suffixes.push(suffix.clone());
            }
        }
    
        let mut table_entries = Vec::new();
    
        for prefix in &main_prefixes {
            for suffix in &suffixes {
                let prefix_to_check = if prefix == "ε" { "" } else { prefix };
                let suffix_to_check = if suffix == "ε" { "" } else { suffix };
                let value = self.t.get(prefix_to_check)
                    .and_then(|m| m.get(suffix_to_check))
                    .map(|v| v.to_string())
                    .unwrap_or("?".to_string());
                table_entries.push(value);
            }
        }
    
        for prefix in &non_main_prefixes {
            for suffix in &suffixes {
                let prefix_to_check = if prefix == "ε" { "" } else { prefix };
                let suffix_to_check = if suffix == "ε" { "" } else { suffix };
                let value = self.t.get(prefix_to_check)
                    .and_then(|m| m.get(suffix_to_check))
                    .map(|v| v.to_string())
                    .unwrap_or("?".to_string());
                table_entries.push(value);
            }
        }
    
        (
            main_prefixes.join(" "),
            non_main_prefixes.join(" "),
            suffixes.join(" "),
            table_entries.join(" "),
        )
    }
    

    fn print_observation_table(&self) {
        let (main_prefixes, non_main_prefixes, suffixes, table_str) = self.form_query();
        println!("Main Prefixes: {}", main_prefixes);
        println!("Non-Main Prefixes: {}", non_main_prefixes);
        println!("Suffixes: {}", suffixes);
        println!("Table: {}", table_str);
        println!("Переходная таблица (T):");
        for (s, transitions) in &self.t {
            println!("T[{}]: {:?}", s, transitions);
        }
    }

    pub fn learn(&mut self, max_iter: i32) {
        let mut iter = 0;
        while iter < max_iter {
            println!("Итерация {}:", iter + 1);
            self.print_observation_table();

            if let (false, Some(extended_pref)) = self.is_closed() {
                self.s.insert(extended_pref.clone(), true);
                self.extended_part.remove(&extended_pref);
                println!("таблица неполная");
                self.extend_table_with_prefixes();
                continue;
            }
            println!("таблица полная");

            if let (false, Some(extended_suf)) = self.is_consistent() {
                self.e.insert(extended_suf.clone(), true);
                println!("таблица противоречива");
                self.extend_table_with_new_suffix(&extended_suf);
                continue;
            }

            let (main_prefixes, non_main_prefixes, suffixes, table_str) = self.form_query();
            match mat::equivalence_query(&main_prefixes, &non_main_prefixes, &suffixes, &table_str) {
                Ok((true, _)) => {
                    println!("Автомат эквивалентен.");
                    return;
                },
                Ok((false, counterexample)) => {
                    println!("Найден контрпример: {}", counterexample);
                    println!("Добавляем его со всеми его суффиксами в E");
                    self.add_counter_example(&counterexample);
                    iter += 1;
                },
                Err(e) => {
                    println!("Ошибка при проверке эквивалентности: {:?}", e);
                    return;
                },
            }
        }
    }
}
