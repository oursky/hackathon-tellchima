# Tell Chima

## Introduction

This is an app for handling Slack slash command (more details [here](https://api.slack.com/interactivity/slash-commands)),
for user to publish anonymous message on Slack.

This app is written in Haskell with [Warp](https://hackage.haskell.org/package/warp), without using any extra framework.

## System Overview

```
                                      +--------------+  Webhook   +------------+  Store message /
  +--------+   Slack slash command    |              | ---------> |            |  Update status    +----+
  |  User  |  --------------------->  | Slack server |  Response  | Web server | ----------------> | DB |
  +--------+                          |              | <--------- |            |                   +----+
                                      +--------------+            +------------+
                                         ^        ^                    |    ^
                     View Channel        |        |    Post message    |    | trigger action
  +--------+           messages          |        |  on Slack channel  |    | (eg. publish messages on Slack)
  |  User  | <---------------------------+        +--------------------+    |
  +--------+                                                                |
                                                                   +------------+
                                                                   |  Cron job  |
                                                                   |  Service   |
                                                                   +------------+
```

## Development

### Prerequisites

- Docker + Docker compose
  - Please refer to installation guide [here](https://docs.docker.com/get-docker/)
- Cabal
  - For Unix system, install with GHCup via `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
  - See more detailed guide [here](haskell.org/cabal/#install-upgrade)
- libpq
  - Required by PostgreSQL driver, see more detailed instruction [here](#install-libpq)
- Dbmate
  - Please refer to installation guide [here](https://github.com/amacneil/dbmate#installation)

### Database migration

Database migration is handled by standalone database migration tool [Dbmate](https://github.com/amacneil/dbmate)

#### To apply all migration

```bash
make migrate-up
```

#### To rollback 1 migration

```bash
make migrate-down
```

#### To create migration

```bash
NAME={your_migration_name} make create-migration
```

### Environment variable

| Name                              | Description                                                                      |
| --------------------------------- | -------------------------------------------------------------------------------- |
| `API_KEY`                         | API secret for calling web server to trigger action                              |
| `DB_HOST`                         | Database host                                                                    |
| `DB_PORT`                         | Database port                                                                    |
| `DB_USER`                         | Database username                                                                |
| `DB_PASS`                         | Database password                                                                |
| `DB_NAME`                         | Database name                                                                    |
| `DB_URL`                          | Database URL for migration                                                       |
| `SLACK_COMMAND_SIGNING_SECRET`    | Slack signing secret for webhook request verification                            |
| `SLACK_COMMAND_NAME`              | Slack command name for adding message to app, used for generating Slack messages |
| `SLACK_POST_MESSAGE_API_ENDPOINT` | Slack endpoint for posting message on channel                                    |
| `SLACK_CHANNEL_ID`                | Channel ID of Slack channel to post message                                      |
| `SLACK_API_TOKEN`                 | API token for calling Slack API to post message                                  |

### Setup

- Install prerequisites mentioned in [previous section](#prerequisites)
- Configure the app by setting environment variable in `.env`, which can be found
  in project root. See [environment variable section](#environment-variable) for more details
  - To get started, copy template `.env.example` by `cp .env.example .env`
- Start docker containers by running
  ```bash
  docker-compose up -d
  ```
  This should start Postgres DB on `localhost:5432`
- Run DB migration with Dbmate
  ```bash
  make migrate-up
  ```
- Run web server by running
  ```
  cabal run
  ```

To test Slack webhook, please refer to [section below](#setup-slack-webhook) for setup guide.

### Setup Slack webhook

For local development, you will need to setup reverse proxy to your local web server.
For example you can use [ngrok](https://ngrok.com/)

Then configure Slack slash command webhook URL (see [Slack slash command guide](https://api.slack.com/interactivity/slash-commands))
with `{proxy_url}/webhook/tell-chima`.
Remember to configure `SLACK_COMMAND_SIGNING_SECRET` in environment variable properly for webhook verification

### Trigger post message to Slack

Simply send a `POST` request to corresponding endpoint, with `Bearer` authorization header without body.
Token is `API_KEY` in environment variable

| Action                    | Endpoint path         |
| ------------------------- | --------------------- |
| Publish messages on Slack | /message/publish      |
| Post reminder on Slack    | /action/post-reminder |

### Install libpq

For Mac

```bash
brew install libpq
```

For Debian Linux

```bash
sudo apt-get install libpq-dev
```

For other platform, please search for `libpq` with your pacakge manager

#### Caveat

For M1 Mac, brew install library in different path (comparing to Intel Mac). Haskell compiler may not be able to
find and link to the library. To fix this, look for `LDFLAGS` and `CPPFLAGS` in installation output to find
out where the library is installed. When running / building the application, include the extra lib / include directory.

For example,

```bash
# LDFLAGS="-L/opt/homebrew/opt/libpq/lib"
# CPPFLAGS="-I/opt/homebrew/opt/libpq/include"
cabal run --extra-lib-dirs=/opt/homebrew/opt/libpq/lib --extra-include-dirs=/opt/homebrew/opt/libpq/include
```
