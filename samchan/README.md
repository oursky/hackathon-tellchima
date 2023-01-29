# Description

It is a tellchima written in Rust, using the [actix-web](https://github.com/actix/actix-web) web 
app framework and the [diesel](https://github.com/diesel-rs/diesel) ORM.

# Development Rust Environment Setup

## Using nix

Using nix ensures our versions of dependencies like rust, diesel, openssl and pkg-config are the same.

However, I failed to build `diesel` using the stdenv provided by `nixpkgs` because of the binutils 
version, so I chose to exclude it and depends on our host machine one instead to escape from it.

- Install nix

https://nixos.org/download.html

- Enable flake command

```sh
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.config
```

- Start nix shell

```sh
# If you use bash, which what stdenv based on
nix develop
# If you use other shell, such as zsh 
nix develop -c zsh
```

## Without using nix

- Install Rust

https://www.rust-lang.org/tools/install

- Install Diesel-CLI

https://diesel.rs/guides/getting-started

# Starting our main web application

## Setting up db and running migration

```
docker-compose up -d
diesel setup
```

## Starting Server

```
cargo run --bin tell-machi
```

# Sending Boardcast Reminder

```
cargo run --bin boardcast_reminder
```

# Sending Boardcast Reminder

```
cargo run --bin publish_message
```
