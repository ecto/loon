use super::{Function, LoonCompiler, LoonType, Prototype};
use inkwell::values::AnyValue;

pub fn inject<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    inject_add(compiler, main_function);
}

fn inject_add<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    let function = compiler.compile_prototype(&Prototype {
        name: "+".to_string(),
        args: vec!["a".to_string(), "b".to_string()],
        external: false,
        return_type: LoonType::Number,
    });

    let entry = compiler.context.append_basic_block(function, "entry");
    compiler.builder.position_at_end(entry);

    let ty = compiler.context.f64_type();
    let lhs = compiler
        .builder
        .build_load(
            ty,
            function.get_first_param().unwrap().into_pointer_value(),
            "lhs",
        )
        .into_float_value();
    let rhs = compiler
        .builder
        .build_load(
            ty,
            function.get_nth_param(1).unwrap().into_pointer_value(),
            "rhs",
        )
        .into_float_value();
    let body = compiler.builder.build_float_add(lhs, rhs, "tmpadd");
    compiler.builder.build_return(Some(&body));

    main_function
        .scope
        .variables
        .insert("+".to_string(), Box::new(function.as_any_value_enum()));
}
