use crate::services::slack_service::SignedMessage;
use crate::services::{message_service, slack_service::verify_signed_message};
use crate::Pool;
use actix_web::{post, web, HttpRequest, Result};
use serde::Deserialize;

#[derive(Deserialize, Debug)]
struct Info {
    command: String,
    text: String,
}

fn tell(info: Info, pool: web::Data<Pool>) -> Result<String> {
    if info.text.trim().len() == 0 {
        return Ok(format!(
            "
              There's no message text in your last command.\n\
              You can tell chima like this: /tellchima <YOUR MESSAGE>
              "
        ));
    }
    match message_service::create_message(&info.text, pool) {
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

async fn untell(info: Info, pool: web::Data<Pool>) -> Result<String> {
    let re = regex::Regex::new(r"#(\d+) (\w+)").unwrap();
    let ca = re.captures(&info.text).unwrap();
    let (message_id, message_code) = (&ca[1], &ca[2]);
    let message_id = message_id.parse::<i32>();
    match message_id {
        Ok(message_id) => {
            match message_service::delete_message(message_id, message_code, pool).await {
                Ok(_) => Ok(format!("Deleted, meow!")),
                Err(_) => Ok(format!("Failed to delete message")),
            }
        }
        Err(_) => Ok(format!("Failed to parse message_id to i32")),
    }
}

#[post("/")]
async fn handler(req: HttpRequest, body: String, pool: web::Data<Pool>) -> Result<String> {
    let signing_secret =
        std::env::var("SLACK_SINGING_SECRET").expect("SLACK_SINGING_SECRET must be set");
    let signed_message = SignedMessage::new(&body, &req.headers());
    if let Err(_) = signed_message {
        return Ok(format!("Failed to get signed_message"));
    }
    let res = verify_signed_message(
        &SignedMessage::new(&body, &req.headers()).unwrap(),
        &signing_secret,
    );
    match res {
        Err(_) => {
            return Ok(format!("Failed to verify_signed_message"));
        }
        Ok(false) => {
            return Ok(format!("Invalid signature"));
        }
        _ => (),
    }
    let info: Info = serde_urlencoded::from_str(&body)?;
    match &info.command {
        x if x == "/tell-machi" => tell(info, pool),
        x if x == "/untell-machi" => untell(info, pool).await,
        _ => panic!("Unmatched command"),
    }
}
