create table blocks (
    height integer primary key check (height > 0),
    block_id blob not null,
    reference blob not null,
    version int not null,
    block_timestamp timestamp not null,
    generator_address text not null,
    generator_public_key blob not null,
    base_target int not null,
    generation_signature blob not null,
    block_data_bytes blob,
    cumulative_score text not null
);

create index block_generator_address_index on blocks(generator_address);

create table feature_votes(
    feature_id int,
    height integer references blocks(height) on delete cascade,
    primary key (feature_id, height)
);

create table waves_balances (
    address text,
    regular_balance bigint not null check (regular_balance >= 0),
    height integer references blocks(height) on delete cascade,
    primary key (address, height)
);

create table asset_info (
    asset_id blob primary key,
    issuer public_key_type,
    decimals int2 not null check (decimals between 0 and 32767),
    name blob not null,
    description blob not null,
    height integer references blocks(height) on delete cascade
);

create index asset_info_height_index on asset_info(height);

create table asset_quantity (
    asset_id blob references asset_info(asset_id) on delete cascade,
    quantity_change bigint not null,
    reissuable boolean not null,
    height integer references blocks(height) on delete cascade,
    primary key (asset_id, height)
);

create index asset_quantity_height_index on asset_quantity(height);

create table asset_balances (
    address text,
    asset_id blob references asset_info(asset_id) on delete cascade,
    balance bigint check (balance >= 0),
    height integer references blocks(height) on delete cascade,
    primary key (address, asset_id, height)
);

create index asset_balances_height_index on asset_balances(height);

create table lease_info (
    lease_id blob primary key,
    sender_public_key blob not null,
    sender_address text not null,
    recipient text not null,
    amount bigint check (amount >= 0),
    height integer references blocks(height) on delete cascade
);

create index lease_info_height_index on lease_info(height);

create table lease_status (
    lease_id blob references lease_info(lease_id) on delete cascade,
    active boolean not null,
    height integer references blocks(height) on delete cascade
);

create index lease_status_height_index on lease_status(height);
create index lease_status_lease_id_index on lease_status(lease_id);

create table lease_balances (
    address text,
    lease_in bigint not null,
    lease_out bigint not null,
    height integer references blocks(height) on delete cascade,

    constraint non_negative_lease_in check (height < 462000 or lease_in >= 0),
    constraint non_negative_lease_out check (height < 462000 or lease_out >= 0),

    primary key (address, height)
);

create index lease_balances_height_index on lease_balances(height);

create table filled_quantity (
    order_id blob,
    filled_quantity bigint not null check (filled_quantity >= 0),
    fee bigint not null check (fee >= 0),
    height integer references blocks(height) on delete cascade,

    primary key (order_id, height)
);

create index filled_quantity_height_index on filled_quantity(height);
create index filled_quantity_order_id_index on filled_quantity(order_id);

create table transactions (
    tx_id blob primary key,
    tx_type int2 not null,
    tx_bytes blob not null,
    height integer references blocks(height) on delete cascade
);

create index transactions_height_index on transactions(height);

create table address_transaction_ids (
    address text,
    tx_id blob,
    height integer references blocks(height) on delete cascade,

    foreign key (tx_id) references transactions(tx_id) on delete cascade
);

create index address_transaction_ids_tx_id_index on address_transaction_ids(tx_id);
create index address_transaction_ids_height_index on address_transaction_ids(height);

create table payment_transactions (
    tx_hash blob primary key,
    height integer references blocks(height) on delete cascade
);

create index payment_transactions_height_index on payment_transactions(height);

create table aliases (
    alias blob primary key,
    address text,
    height integer references blocks(height) on delete cascade
);

create index aliases_of_address_index on aliases(address);
create index aliases_height_index on aliases(height);
