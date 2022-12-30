DBMATE := dbmate -e "DB_URL" -d app/Database/Migration --no-dump-schema

.PHONY: create-migration
create-migration:
	@$(DBMATE) new migration $(NAME)

.PHONY: migrate-up
migrate-up:
	@$(DBMATE) up

.PHONY: migrate-down
migrate-down:
	@$(DBMATE) down
