GENERATE_SECRET ?= curl -s https://raw.githubusercontent.com/oursky/devsecops-secret/v1.0/generate-secret.sh | bash -s --
DBMATE ?= env -S $$(cat .env) dbmate --no-dump-schema

.PHONY: secret
secret:
	$(GENERATE_SECRET) .env.example .env
