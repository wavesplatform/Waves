begin;

create domain public_key_type as bytea not null;
create domain digest_type as text not null;
create domain signature_type as text not null;
create domain amount_type as bigint not null check (value >= 0);
create domain height_type as int not null check (value > 0);

create domain address_type as text not null check (
    value ~ '^[1-9A-HJ-NP-Za-km-z]{35,36}$' or
    value ~ '^address:[1-9A-HJ-NP-Za-km-z]{35,36}$'
);

create domain address_or_alias as text not null check (
    value ~ '^[1-9A-HJ-NP-Za-km-z]{35,36}$' or
    value ~ '^address:[1-9A-HJ-NP-Za-km-z]{35,36}$' or
    value ~ '^alias:[A-Z]:[0-9a-z@_.-]{4,30}$'
);

create type tx_type_id_type as enum(
    'genesis',
    'payment',
    'issue',
    'transfer',
    'reissue',
    'burn',
    'exchange',
    'lease',
    'lease_cancel',
    'create_alias'
);

create table blocks (
    height height_type primary key,
    block_id signature_type,
    block_timestamp timestamp not null,
    generator_address address_type,
    block_data_bytes bytea not null,
    score bigint not null,
    tx_count int not null
);

create table waves_balances (
    address address_type,
    regular_balance amount_type,
    height height_type references blocks(height),
    primary key (address, height)
);

create index waves_balances_height_index on waves_balances(height);
create index regular_balance_index on waves_balances(regular_balance);

create table asset_info (
    asset_id digest_type primary key,
    issuer public_key_type,
    decimals int2 not null,
    name bytea not null,
    description bytea not null,
    height height_type references blocks(height)
);

create index asset_info_height_index on asset_info(height);

create table asset_quantity (
    asset_id digest_type references asset_info(asset_id),
    total_quantity numeric not null,
    reissuable boolean not null,
    height height_type references blocks(height),
    primary key (asset_id, height)
);

create index asset_quantity_height_index on asset_quantity(height);

create table asset_balances (
    address address_type,
    asset_id digest_type references asset_info(asset_id),
    balance amount_type,
    height height_type references blocks(height),
    primary key (address, asset_id, height)
);

create index asset_balances_height_index on asset_balances(height);

create table lease_info (
    lease_id digest_type primary key,
    sender public_key_type,
    recipient address_or_alias,
    amount amount_type,
    height height_type references blocks(height)
);

create index lease_info_height_index on lease_info(height);

create table lease_status (
    lease_id digest_type references lease_info(lease_id),
    active boolean not null,
    height height_type references blocks(height)
);

create index lease_status_height_index on lease_status(height);
create index lease_status_lease_id_index on lease_status(lease_id);

create table lease_balances (
    address address_type,
    lease_in bigint not null,
    lease_out bigint not null,
    height height_type references blocks(height),

    constraint non_negative_lease_in check (height < 462000 or lease_in >= 0),
    constraint non_negative_lease_out check (height < 462000 or lease_out >= 0),

    primary key (address, height)
);

create index lease_balances_height_index on lease_balances(height);

create table filled_quantity (
    order_id digest_type,
    filled_quantity amount_type,
    fee amount_type,
    height height_type references blocks(height),

    primary key (order_id, height)
);

create index filled_quantity_height_index on filled_quantity(height);

create table transaction_offsets (
    tx_id digest_type,
    signature signature_type,
    tx_type tx_type_id_type not null,
    height height_type references blocks(height),

    primary key (tx_id, signature)
);

create index transaction_offsets_height_index on transaction_offsets(height);

create table transactions (
    tx_id digest_type,
    signature signature_type,
    tx_type tx_type_id_type not null,
    tx_json json not null,
    height height_type references blocks(height),

    primary key (tx_id, signature)
);

create index transactions_height_index on transactions(height);

create table address_transaction_ids (
    address address_type,
    tx_id digest_type,
    signature signature_type,
    height height_type references blocks(height),

    foreign key (tx_id, signature) references transactions(tx_id, signature)
);

create index address_transaction_ids_height_index on address_transaction_ids(height);
create index address_transaction_ids_tx_id_index on address_transaction_ids(tx_id, signature);
create index address_transaction_ids_address_index on address_transaction_ids(address);

create table payment_transactions (
    tx_hash digest_type primary key,
    height height_type references blocks(height)
);

create index payment_transactions_height_index on payment_transactions(height);

create table exchange_transactions (
    tx_id digest_type primary key,
    amount_asset_id text references asset_info(asset_id),
    price_asset_id text references asset_info(asset_id),
    amount amount_type,
    price amount_type,
    height height_type references blocks(height),

    constraint valid_pair check (
        (amount_asset_id is not null or price_asset_id is not null) and amount_asset_id != price_asset_id
    )
);

create index exchange_transactions_height_index on exchange_transactions(height);

create table transfer_transactions (
    tx_id digest_type primary key,
    sender address_type,
    recipient address_or_alias,
    asset_id text references asset_info(asset_id),
    amount amount_type,
    fee_asset_id text references asset_info(asset_id),
    fee amount_type,
    height height_type references blocks(height)
);

create index transfer_transactions_height_index on transfer_transactions(height);

create table aliases (
    alias bytea primary key,
    address address_type,
    height height_type references blocks(height)
);

create index aliases_of_address_index on aliases(address);
create index aliases_height_index on aliases(height);

commit;