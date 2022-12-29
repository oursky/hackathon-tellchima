use actix_web::web;
use diesel::{
    r2d2::{self, ConnectionManager},
    PgConnection,
};
use dotenvy::dotenv;
use tell_machi::{message_service::publish_message, Pool};

#[actix_web::main]
async fn main() {
    dotenv().ok();
    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    let pool: Pool = r2d2::Pool::builder()
        .build(manager)
        .expect("Failed to create pool.");
    publish_message(web::Data::new(pool)).await;
}
