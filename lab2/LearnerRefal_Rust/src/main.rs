mod learner;
mod mat;

fn main() {
    let alphabet = vec!["L".to_string(), "R".to_string()];
    let mut observation_table = learner::ObservationTable::new(alphabet);

    observation_table.learn(10);
}