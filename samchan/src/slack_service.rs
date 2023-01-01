use dotenvy::dotenv;
use reqwest::{Error, Response};

pub async fn send_message(text: &str) -> Result<Response, Error> {
    dotenv().ok();
    let token = std::env::var("SLACK_OAUTH_TOKEN").expect("DATABASE_URL must be set");
    let client = reqwest::Client::new();
    client
        .post("https://slack.com/api/chat.postMessage")
        .body(format!(
            r#"{{"channel": "C04GPBBHRM4", "text": "{}"}}"#,
            text
        ))
        .header("Authorization", format!("Bearer {}", token))
        .header("Content-type", "application/json")
        .send()
        .await
}
