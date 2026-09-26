Create table asset_group(`id` blob(16) not null, `asset_type` int not null, `local_id` int not null, `runtime_asset_id` blob(16) not null unique, primary key(`id`, `asset_type`, `local_id`));
Create table user_asset(`source_path` text, `group_id` blob(16) unique, `last_processed` timestamp not null, primary key(`source_path`), foreign key(`group_id`) references asset_group(`id`));
