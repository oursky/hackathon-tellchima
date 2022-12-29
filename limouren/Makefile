PGDATA ?= ./postgres-data
PGPORT ?= 5433

DBNAME ?= tellchima_dev

.PHONY: db-init
db-init:
	initdb -D $(PGDATA) -U postgres

.PHONY: db
db:
	postgres -D $(PGDATA) -p $(PGPORT)

.PHONY: psql
psql:
	psql -U postgres -d $(DBNAME) -p $(PGPORT)
