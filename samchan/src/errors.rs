use derive_more::Display;

#[derive(Debug, Display)]
pub enum ServiceError {
    #[display(fmt = "Internal Server Error")]
    InternalServerError,
}
