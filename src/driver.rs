extern crate clap;
extern crate elang;

use clap::{Arg, App};
use std::path::PathBuf;

use elang::{pipeline, outer};
use elang::pipeline::{CompileOptions, OutputType};

fn main() {
    let matches = App::new("Elang Compiler")
        .version("0.1")
        .author("Paul CACHEUX <paulcacheux@gmail.com>")
        .arg(Arg::with_name("ast").long("ast").help("Dump AST"))
        .arg(Arg::with_name("ir").long("ir").help("Dump IR"))
        .arg(Arg::with_name("opt")
                 .short("O")
                 .help("Activate optimizations"))
        .arg(Arg::with_name("INPUT")
                 .help("Input file")
                 .required(true)
                 .index(1))
        .arg(Arg::with_name("OUTPUT")
                 .help("Output file")
                 .long("output")
                 .short("o")
                 .value_name("FILE")
                 .takes_value(true))
        .arg(Arg::with_name("output_type")
                 .help("Output type")
                 .long("type")
                 .short("t")
                 .takes_value(true)
                 .possible_values(&["check", "llvm", "run", "exec"]))
        .get_matches();

    let input_path = PathBuf::from(matches.value_of("INPUT").unwrap());
    let global_dir = input_path.parent().unwrap().to_path_buf();

    let options = CompileOptions {
        global_dir: global_dir,
        print_ast: matches.is_present("ast"),
        print_ir: matches.is_present("ir"),
        opt: matches.is_present("opt"),
        output_type: OutputType::new(matches.value_of("output_type").unwrap_or("check")).unwrap(),
        output_path: matches.value_of("OUTPUT").map(PathBuf::from),
    };

    let tu = pipeline::process_main_path(input_path.clone(), &options);
    if let Err(err) = outer::main_outer(tu, input_path.to_str().unwrap(), &options) {
        println!("{}", err);
        std::process::exit(1);
    }
}
