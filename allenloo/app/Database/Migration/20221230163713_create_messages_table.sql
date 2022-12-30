-- migrate:up
CREATE TABLE messages (
  id SERIAL,
  text TEXT NOT NULL,
  published BOOLEAN NOT NULL DEFAULT false
)

-- migrate:down
DROP TABLE messages
