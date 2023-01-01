// @generated automatically by Diesel CLI.

diesel::table! {
    messages (id) {
        id -> Int4,
        code -> Varchar,
        body -> Text,
        published -> Bool,
    }
}
