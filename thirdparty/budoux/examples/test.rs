use peridot_tp_budoux::{JsonModel, parse};

fn main() {
    let model = JsonModel::new(
        serde_json::from_str(include_str!("../source-repo/budoux/models/ja_knbc.json")).unwrap(),
    );

    let sentence = "Hello, world!";
    let chunks = parse(&model, sentence);
    println!("{:?}", chunks);

    let sentence = "今日はいい天気";
    let chunks = parse(&model, sentence);
    println!("{:?}", chunks);

    let sentence = "ネコたちのお茶会";
    let chunks = parse(&peridot_tp_budoux::embedded::ja_knbc::MODEL, sentence);
    println!("{:?}", chunks);
}
