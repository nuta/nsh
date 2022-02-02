use std::io::Read;

use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(long)]
    parse_completion: bool,
}

pub fn main() {
    let args = Args::parse();

    if args.parse_completion {
        let mut buf = Vec::new();
        std::io::stdin().read_to_end(&mut buf).unwrap();
        let text = std::str::from_utf8(&buf).expect("non-utf8 input");
        let comp = nsh_completion::man::ManParser::new().parse(text);
        println!("{:#?}", comp);
    }
}
