use actix_web::http::header::HeaderMap;
use anyhow::anyhow;
use dotenvy::dotenv;
use reqwest::{Error, Response};

use hmac::{Hmac, Mac};
use sha2::Sha256;

pub struct SignedMessage<'a> {
    body: &'a [u8],
    timestamp: u64,
    signature: &'a [u8],
}

impl<'a> SignedMessage<'a> {
    pub fn new(body: &'a str, headers: &'a HeaderMap) -> Result<Self, anyhow::Error> {
        Ok(SignedMessage {
            body: body.as_bytes(),
            timestamp: headers
                .get("X-Slack-Request-Timestamp")
                .ok_or(anyhow!(""))?
                .to_str()?
                .parse::<u64>()?,
            signature: headers
                .get("X-Slack-Signature")
                .ok_or(anyhow!(""))?
                .as_bytes(),
        })
    }
}

type HmacSha256 = Hmac<Sha256>;

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

pub fn verify_signed_message(
    signed_message: &SignedMessage,
    signing_secret: &str,
) -> Result<bool, anyhow::Error> {
    let sig_basestring = format!(
        "v0:{}:{}",
        signed_message.timestamp,
        String::from_utf8(signed_message.body.to_vec())?
    );
    let mut mac = HmacSha256::new_from_slice(signing_secret.as_bytes())?;
    mac.update(sig_basestring.as_bytes());
    let tmp = hex::encode(&mac.finalize().into_bytes()[..]);
    let true_signature = ["v0=".as_bytes(), tmp.as_bytes()].concat();
    if true_signature != signed_message.signature {
        return Ok(false);
    }
    Ok(true)
}

#[test]
fn testing() {
    // example from https://api.slack.com/authentication/verifying-requests-from-slack
    dotenv().ok();
    let signing_secret = "8f742231b10e8888abcd99yyyzzz85a5";
    let timestamp = 1531420618;
    let body = b"token=xyzz0WbapA4vBCDEFasx0q6G&team_id=T1DC2JH3J&team_domain=testteamnow&channel_id=G8PSS9T3V&channel_name=foobar&user_id=U2CERLKJA&user_name=roadrunner&command=%2Fwebhook-collect&text=&response_url=https%3A%2F%2Fhooks.slack.com%2Fcommands%2FT1DC2JH3J%2F397700885554%2F96rGlfmibIGlgcZRskXaIFfN&trigger_id=398738663015.47445629121.803a0bc887a14d10d2c447fce8b6703c";
    let signature = b"v0=a2114d57b48eac39b9ad189dd8316235a7b4a8d21a10bd27519666489c69b503";
    let message = SignedMessage {
        body,
        timestamp,
        signature,
    };
    let res = verify_signed_message(&message, &signing_secret);
    match res {
        Err(..) => {
            assert!(false);
        }
        Ok(x) => {
            assert!(x);
        }
    }
}
