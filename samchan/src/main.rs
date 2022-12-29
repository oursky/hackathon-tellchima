use actix_web::{middleware, post, web, App, HttpServer, Result};
use diesel::{
    r2d2::{self, ConnectionManager},
    PgConnection,
};
use dotenvy::dotenv;
use serde::Deserialize;
use tell_machi::{
    message_service::{create_message, delete_message},
    Pool,
};

#[derive(Deserialize)]
struct Info {
    command: String,
    text: String,
}

#[post("/")]
async fn post_handler(info: web::Form<Info>, pool: web::Data<Pool>) -> Result<String> {
    match &info.command {
        x if x == "/tell-machi" => {
            if info.text.trim().len() == 0 {
                return Ok(format!(
                    "
                      There's no message text in your last command.\n\
                      You can tell chima like this: /tellchima <YOUR MESSAGE>
                      "
                ));
            }
            match create_message(&info.text, pool) {
                Ok(message) => Ok(format!(
                    "
                        Received!\n\
                        Preview: `#{id}` {body}\n\
                        P.S. You can remove this post with `/untell-machi #{id} {code}`\
                        ",
                    id = message.id,
                    body = message.body,
                    code = message.code,
                )),
                Err(_) => Ok(format!("Error ^(00)^")),
            }
        }
        x if x == "/untell-machi" => {
            let re = regex::Regex::new(r"#(\d+) (\w+)").unwrap();
            let ca = re.captures(&info.text).unwrap();
            let (message_id, message_code) = (&ca[1], &ca[2]);
            let message_id = message_id.parse::<i32>();
            match message_id {
                Ok(message_id) => match delete_message(message_id, message_code, pool).await {
                    Ok(_) => Ok(format!("Deleted, meow!")),
                    Err(_) => Ok(format!("Failed to delete message")),
                },
                Err(_) => Ok(format!("Failed to parse message_id to i32")),
            }
        }
        _ => panic!("Unmatched command"),
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();
    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    let pool: Pool = r2d2::Pool::builder()
        .build(manager)
        .expect("Failed to create pool.");

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(pool.clone()))
            .wrap(middleware::Logger::default())
            .service(post_handler)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
