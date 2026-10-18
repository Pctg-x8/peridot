-- アセットグループ
Create table asset_group(
    `id` blob(16) not null,
    `asset_type` int not null,
    `local_id` int not null,
    `runtime_asset_id` blob(16) not null unique,
    `name` text,
    primary key(`id`, `asset_type`, `local_id`)
);

-- アセットグループの情報
Create table asset_group_info(
    `id` blob(16) not null,
    `name` text not null,
    primary key(`id`)
);

-- ロード可能なアセットの識別子とアセットグループのひも付きを管理する
-- そのうちUnityのResourcesみたいな制約を加えようと思っていて（そうすると不要なアセットをエンジンの判断のみで自動でstripできる）、スクリプトからロードできるアセット（エンジンの判断でstripできない）をここに登録する
-- 今のところは全部ロード可能なので全部登録される
Create table loadable_asset(
    `identifier` text not null,
    `group_id` blob(16) unique,
    primary key(`identifier`),
    foreign key(`group_id`) references asset_group(`id`)
);


-- ユーザー生成のアセットファイルとアセットグループのひも付きを管理する
-- last_processedでアセットが最新の状態でビルドされているかを検出する
-- 開発中のみで使われるテーブルで、リリースビルドするときにはomitする（ユーザーマシン固有の情報が入ってるため）
Create table dev_user_asset(
    `source_path` text,
    `group_id` blob(16) unique,
    `last_processed` timestamp not null,
    `is_new_insertion` boolean not null default true,
    primary key(`source_path`),
    foreign key(`group_id`) references asset_group(`id`)
);
