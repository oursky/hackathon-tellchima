use actix_web::{get, Result};

#[get("/")]
async fn handler() -> Result<String> {
    Ok(String::from("Hello !!"))
}
