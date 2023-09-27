#[macro_use]
extern crate pest_derive;
extern crate pest;

use inkwell::context::Context;
use inkwell::passes::PassManager;

mod compiler;
mod parser;

// const MULTIPLE: &str = "
// [hello world!]\n
// [multiple lists [with recursion]]";
// const SIMPLE: &str = "[log [+ 6 9]]";
// const DOUBLE: &str = "[log [+ 1 [+ 6 9]]]";
// const LOG: &str = "[log 123]";
// const FUNCTION: &str = "
// [fn add [a b] [+ a b]]
// [log [+ 123 [add 123 123]]]
// [log true]
// [log false]";
const IF: &str = "[log [if true [+ 1 2] 2]]";

fn main() {
    let program = IF;
    let debug = true;

    let parser = parser::LoonParser {};
    let root_node = parser.run(program);

    let context = Context::create();
    let module = context.create_module("tmp");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);
    // fpm.add_instruction_combining_pass();
    // fpm.add_reassociate_pass();
    // fpm.add_gvn_pass();
    // fpm.add_cfg_simplification_pass();
    // fpm.add_basic_alias_analysis_pass();
    // fpm.add_promote_memory_to_register_pass();
    // fpm.add_instruction_combining_pass();
    // fpm.add_reassociate_pass();
    fpm.initialize();

    let compiler = compiler::LoonCompiler {
        context: &context,
        builder,
        fpm,
        module,
        debug,
    };

    compiler.compile(&root_node);

    if debug {
        println!("{}", program);
        println!("---------------------");
        println!("{}", compiler.get_code());
        println!("---------------------");
    }

    compiler.execute();
}
