use diesel::{
    r2d2::{self, ConnectionManager},
    PgConnection,
};

pub mod schema;
pub mod utils;

pub mod services {
    pub mod message_service;
    pub mod slack_service;
}

pub mod handlers {
    pub mod command_handler;
    pub mod hello_handler;
}

pub mod models {
    pub mod db;
    pub mod errors;
}

pub type Pool = r2d2::Pool<ConnectionManager<PgConnection>>;
