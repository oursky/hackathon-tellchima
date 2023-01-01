use diesel::{Insertable, Queryable};
use serde::{Deserialize, Serialize};

use crate::generate_random_string;

use super::schema::*;

#[derive(Debug, Serialize, Deserialize, Queryable, Insertable)]
#[diesel(table_name = messages)]
pub struct Message {
    pub id: i32,
    pub code: String,
    pub body: String,
    pub published: bool,
}

#[derive(Insertable)]
#[diesel(table_name = messages)]
pub struct NewMessage {
    pub code: String,
    pub body: String,
    pub published: bool,
}

impl NewMessage {
    pub fn new(text: &str) -> Self {
        NewMessage {
            code: generate_random_string(8),
            body: text.to_string(),
            published: false,
        }
    }
}
