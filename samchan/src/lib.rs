use diesel::{
    r2d2::{self, ConnectionManager},
    PgConnection,
};
use rand::Rng;

pub mod errors;
pub mod message_service;
pub mod models;
pub mod schema;
pub mod slack_service;

pub type Pool = r2d2::Pool<ConnectionManager<PgConnection>>;

pub fn generate_random_string(len: u32) -> String {
    const CHARSET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                            abcdefghijklmnopqrstuvwxyz\
                            0123456789)(*&^%$#@!~";
    let mut rng = rand::thread_rng();

    let password: String = (0..len)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect();

    password
}
