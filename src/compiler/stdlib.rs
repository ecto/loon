use super::{Function, LoonCompiler, LoonType, Prototype};

mod log;
mod math;
mod vars;

pub fn inject<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    log::inject(compiler, main_function);
    math::inject(compiler, main_function);
    vars::inject(compiler, main_function);
}
