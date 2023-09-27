use super::{Function, LoonCompiler, LoonType, Prototype};
use inkwell::values::AnyValue;

pub fn inject<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    main_function.scope.variables.insert(
        "true".to_string(),
        Box::new(
            compiler
                .context
                .f64_type()
                .const_float(1.0)
                .as_any_value_enum(),
        ),
    );

    main_function.scope.variables.insert(
        "false".to_string(),
        Box::new(
            compiler
                .context
                .f64_type()
                .const_float(0.0)
                .as_any_value_enum(),
        ),
    );
}
