use reqwest::blocking::{Client, Response};
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::fmt;

#[derive(Serialize)]
struct InitData {
    mode: String,
}

#[derive(Serialize)]
struct MembershipData {
    word: String,
}

#[derive(Serialize)]
struct EquivalenceRequest {
    main_prefixes: String,
    non_main_prefixes: String,
    suffixes: String,
    table: String,
}

#[derive(Deserialize)]
struct EquivalenceResponse {
    response: Option<String>,
    #[serde(rename = "type")]
    response_type: Option<bool>,
}

#[derive(Debug)]
struct HttpResponseError {
    message: String,
}

impl fmt::Display for HttpResponseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for HttpResponseError {}

impl HttpResponseError {
    fn new(message: &str) -> Self {
        HttpResponseError {
            message: message.to_string(),
        }
    }
}

pub fn init(mode: &str) -> Result<(), Box<dyn Error>> {
    let client = Client::new();
    let url = "http://localhost:8080/generate";
    let data = InitData {
        mode: mode.to_string(),
    };

    let resp: Response = client.post(url).json(&data).send()?;
    
    if resp.status().is_success() {
        println!("Соединение установлено и запрос на /generate выполнен успешно");
        Ok(())
    } else {
        Err(Box::new(HttpResponseError::new(&format!(
            "Ошибка: статус ответа {}",
            resp.status()
        ))))
    }
}

pub fn membership_query(query: &str) -> Result<i32, Box<dyn Error>> {
    let client = Client::new();
    let url = "http://localhost:8095/checkWord";
    let word = if query.is_empty() { "ε".to_string() } else { query.to_string() };
    let data = MembershipData { word };

    let resp: Response = client.post(url).json(&data).send()?;
    
    if resp.status().is_success() {
        let response: serde_json::Value = resp.json()?;
        
        if response["response"].as_bool().unwrap_or(false) {
            Ok(1)
        } else {
            Ok(0)
        }
    } else {
        Err(Box::new(HttpResponseError::new(&format!(
            "Ошибка: статус ответа {}",
            resp.status()
        ))))
    }
}

pub fn equivalence_query(
    main_prefixes: &str,
    non_main_prefixes: &str,
    suffixes: &str,
    table: &str,
) -> Result<(bool, String), Box<dyn Error>> {
    let client = Client::new();
    let url = "http://localhost:8095/checkTable";
    let request_data = EquivalenceRequest {
        main_prefixes: main_prefixes.to_string(),
        non_main_prefixes: non_main_prefixes.to_string(),
        suffixes: suffixes.to_string(),
        table: table.to_string(),
    };

    let resp: Response = client.post(url).json(&request_data).send()?;
    
    if resp.status().is_success() {
        let response_body: EquivalenceResponse = resp.json()?;
        
        if let Some(response_type) = response_body.response_type {
            Ok((false, response_body.response.unwrap_or_default()))
        } else {
            Ok((true, String::new()))
        }
    } else {
        Err(Box::new(HttpResponseError::new(&format!(
            "Ошибка: статус ответа {}",
            resp.status()
        ))))
    }
}


