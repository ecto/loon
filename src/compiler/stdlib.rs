use super::{Function, LoonCompiler, LoonType, Prototype};

mod branch;
mod log;
mod math;
mod vars;

pub fn inject<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    branch::inject(compiler, main_function);
    log::inject(compiler, main_function);
    math::inject(compiler, main_function);
    vars::inject(compiler, main_function);
}
