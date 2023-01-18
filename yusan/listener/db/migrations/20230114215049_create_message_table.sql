-- migrate:up

create table message (
    id integer primary key autoincrement not null,
    code text default (hex(randomblob(4))),
    body text not null
);

-- migrate:down

drop table message;
