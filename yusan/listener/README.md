# listener

## Dependencies

- dbmate
- esy

## Setup

```sh
make secret
make db
```

## Migrations

```sh
make migration NAME="create_message_table"
make migrate-up
make migrate-down
```
