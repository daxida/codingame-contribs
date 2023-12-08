// TODO: .clone() less things!

use std::io;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;

// Custom min binary heap for strings
#[derive(Eq, PartialEq)]
struct CustomString {
    value: String,
}

impl Ord for CustomString {
    fn cmp(&self, other: &Self) -> Ordering {
        other.value.cmp(&self.value) // reversed
    }
}

impl PartialOrd for CustomString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type FakeHash = Vec<(String, Vec<String>)>;

fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let n_imp: i32 = input_line.trim().parse().unwrap();
    // There is no map-like structure that preserves insertion order
    let mut dependencies: FakeHash = Vec::new();
    let mut idxs = HashMap::new();
    for i in 0..n_imp as usize {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let library = input_line.trim().replace("import ", "").to_string();
        dependencies.push((library.clone(), Vec::new()));
        idxs.insert(library, i);
    }

    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let n_dep: i32 = input_line.trim().parse().unwrap();
    for _ in 0..n_dep {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let parts: Vec<&str> = input_line.trim().split(" requires ").collect();
        if let [library, deps] = parts[..] {
            let dependencies_list: Vec<String> = deps.split(", ").map(|s| s.to_string()).collect();
            if let Some(&index) = idxs.get(library) {
                dependencies[index].1.extend(dependencies_list);
            }
        }
    }
    if !successful_compile(&dependencies) {
        kahn(&dependencies);
    }
}

fn successful_compile(dependencies: &FakeHash) -> bool {
    let mut compiled = HashSet::new();
    for (library, deps) in dependencies.iter() {
        for d in deps {
            if !compiled.contains(d) {
                println!("Import error: tried to import {} but {} is required.", library, d);
                return false;
            }
        }
        compiled.insert(library);
    }
    println!("Compiled successfully!");
    true
}

fn kahn(dependencies: &FakeHash) {
    let mut D: FakeHash = dependencies.clone();
    let mut heap: BinaryHeap<CustomString> = BinaryHeap::new();
    let mut record = Vec::<String>::new();
    // Need this extra sets since BinaryHeap doesn't have a contains method
    let mut in_heap: HashSet<String> = HashSet::new();

    for (library, deps) in D.iter() {
        if deps.is_empty() {
            heap.push(CustomString { value: library.clone() });
            in_heap.insert(library.clone());
        }
    }

    while let Some(library_wrapper) = heap.pop() {
        let library = library_wrapper.value;
        record.push(library.clone());
        in_heap.remove(&library);

        for (other, deps) in D.iter_mut() {
            if deps.contains(&library) {
                deps.retain(|dep| dep != &library);
            }
            if deps.is_empty() && !record.contains(other) && !in_heap.contains(other) {
                in_heap.insert(other.clone());
                heap.push(CustomString { value: other.clone() });
            }
        }
    }

    if record.len() != dependencies.len() {
        println!("Fatal error: interdependencies.")
    } else {
        println!("Suggest to change import order:");
        for library in record {
            println!("import {}", library);
        }
    }
}