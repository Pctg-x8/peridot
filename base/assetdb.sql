-- アセットグループ
Create table asset_group(
    `id` blob(16) not null,
    `name` text not null,
    primary key(`id`)
);

-- アセット
Create table asset(
    `id` blob(16) not null primary key,
    `type` int not null,
    `name` text,
    `group_id` blob(16) not null,
    `local_id` int not null,
    unique (`group_id`, `local_id`)
    foreign key (`group_id`) references asset_group(`id`)
);

-- アセットの依存関係
Create table asset_dependency(
    `asset_id` blob(16) not null,
    `required_asset_id` blob(16) not null,
    primary key (`asset_id`, `required_asset_id`),
    foreign key (`asset_id`) references asset(`id`),
    foreign key (`required_asset_id`) references asset(`id`)
);

-- ロード可能なアセットの識別子とアセットグループのひも付きを管理する
-- そのうちUnityのResourcesみたいな制約を加えようと思っていて（そうすると不要なアセットをエンジンの判断のみで自動でstripできる）、スクリプトからロードできるアセット（エンジンの判断でstripできない）をここに登録する
-- 今のところは全部ロード可能なので全部登録される
Create table loadable_asset(
    `identifier` text not null,
    `group_id` blob(16) not null unique,
    primary key(`identifier`),
    foreign key(`group_id`) references asset_group(`id`)
);

-- ユーザー生成のアセットファイルとアセットグループのひも付きを管理する
-- last_processedでアセットが最新の状態でビルドされているかを検出する
-- 開発中のみで使われるテーブルで、リリースビルドするときにはomitする（ユーザーマシン固有の情報が入ってるため）
Create table dev_user_asset(
    `source_path` text,
    `group_id` blob(16) not null unique,
    `last_processed` timestamp not null,
    `is_new_insertion` boolean not null default true, -- upsertで新規にinsertされたときを判定するためのカラム on conflict do updateではfalseをいれるようにする
    `is_last_processed_renewed` boolean not null default false, -- upsert時のlast_processedの更新を記録するカラム ここに条件式を書いてreturningで返す
    primary key(`source_path`),
    foreign key(`group_id`) references asset_group(`id`)
);
