use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct EmbeddingServer {
    embeddings: HashMap<String, Vec<f32>>,
}

impl EmbeddingServer {
    fn new(file_path: &str) -> Self {
        let mut embeddings = HashMap::new();
        let file = File::open(file_path).expect("Unable to open embedding file");
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line.expect("Unable to read line");
            let parts: Vec<&str> = line.split_whitespace().collect();
            let word = parts[0].to_string();
            let vector: Vec<f32> = parts[1..]
                .iter()
                .map(|&s| s.parse::<f32>().expect("Unable to parse float"))
                .collect();
            embeddings.insert(word, vector);
        }
        Self { embeddings }
    }

    fn get_embedding(&self, word: &str) -> Option<&Vec<f32>> {
        self.embeddings.get(word)
    }

    fn aggregate_embeddings(&self, words: &[&str], method: &str) -> Option<Vec<f32>> {
        let mut vectors: Vec<Vec<f32>> = Vec::new();
        for &word in words {
            if let Some(embedding) = self.get_embedding(word) {
                vectors.push(embedding.clone());
            }
        }
        match method {
            "average" => {
                if vectors.is_empty() {
                    return None;
                }
                let sum: Vec<f32> = vectors.iter().cloned().reduce(|a, b| 
                    a.iter().zip(b.iter()).map(|(x, y)| x + y).collect()
                )?;
                Some(sum.iter().map(|&x| x / vectors.len() as f32).collect())
            },
            "sum" => {
                if vectors.is_empty() {
                    return None;
                }
                Some(vectors.iter().cloned().reduce(|a, b| 
                    a.iter().zip(b.iter()).map(|(x, y)| x + y).collect()
                )?)
            },
            _ => None,
        }
    }
}

fn main() {
    let server = EmbeddingServer::new("path/to/glove.6B.50d.txt");
    if let Some(embedding) = server.get_embedding("hello") {
        println!("Embedding for 'hello': {:?}", embedding);
    }
    if let Some(aggregated) = server.aggregate_embeddings(&["hello", "world"], "average") {
        println!("Aggregated embedding (average): {:?}", aggregated);
    }
}