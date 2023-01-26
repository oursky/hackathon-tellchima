use tell_machi::services::slack_service;

#[actix_web::main]
async fn main() {
    let _ = slack_service::send_message(
        "If you have something to post, please /tell-machi. Publishes daily at 5pm.",
    )
    .await;
}
