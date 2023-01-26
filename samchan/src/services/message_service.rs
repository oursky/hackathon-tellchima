use actix_web::web;
use diesel::prelude::*;

use crate::{
    models::db::{Message, NewMessage},
    models::errors::ServiceError,
    services::slack_service,
    Pool,
};

pub fn create_message(text: &str, pool: web::Data<Pool>) -> Result<Message, ServiceError> {
    use crate::schema::messages::dsl::*;
    let message = NewMessage::new(text);
    let mut conn = pool.get().unwrap();
    let inserted_message = diesel::insert_into(messages)
        .values(&message)
        .get_result::<Message>(&mut conn);
    match inserted_message {
        Ok(x) => {
            dbg!(&x);
            return Ok(x);
        }
        _ => {
            return Err(ServiceError::InternalServerError);
        }
    }
}

pub async fn publish_message(pool: web::Data<Pool>) {
    use crate::schema::messages::dsl::*;
    let mut conn = pool.get().unwrap();
    let pending_messages = messages
        .filter(published.eq(false))
        .get_results::<Message>(&mut conn);
    println!("{:?}", pending_messages);
    for message in pending_messages.unwrap().iter() {
        slack_service::send_message(&message.body).await.unwrap();
        diesel::update(messages.filter(id.eq(message.id)))
            .set(published.eq(true))
            .execute(&mut conn)
            .unwrap();
    }
}

pub async fn delete_message(
    message_id: i32,
    message_code: &str,
    pool: web::Data<Pool>,
) -> Result<(), ServiceError> {
    use crate::schema::messages::dsl::*;
    let mut conn = pool.get().unwrap();
    let result = diesel::delete(
        messages
            .filter(id.eq(&message_id))
            .filter(code.eq(message_code)),
    )
    .execute(&mut conn);
    match result {
        Err(_) => Err(ServiceError::InternalServerError),
        _ => Ok(()),
    }
}
