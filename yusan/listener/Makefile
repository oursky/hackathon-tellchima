GENERATE_SECRET ?= curl -s https://raw.githubusercontent.com/oursky/devsecops-secret/v1.0/generate-secret.sh | bash -s --
DBMATE ?= env -S $$(cat .env) dbmate --no-dump-schema

.PHONY: secret
secret:
	$(GENERATE_SECRET) .env.example .env

.PHONY: db
db:
	$(DBMATE) create

.PHONY: migration
migration:
	$(DBMATE) new $${NAME}

.PHONY: migrate-up
migrate-up:
	$(DBMATE) up

.PHONY: migrate-down
migrate-down:
	$(DBMATE) down

.PHONY: ngrok
ngrok:
	NIXPKGS_ALLOW_UNFREE=1 nix-shell -p ngrok --run 'ngrok http 8080'
