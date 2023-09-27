use super::{Function, LoonCompiler, LoonType, Prototype};
use inkwell::{values::AnyValue, FloatPredicate};

pub fn inject<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    inject_if(compiler, main_function);
}

// TODO could choose to push down compile_node to the branches somehow
fn inject_if<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    let function = compiler.compile_prototype(&Prototype {
        name: "if".to_string(),
        args: vec!["cond".to_string(), "then".to_string(), "else".to_string()],
        external: false,
        return_type: LoonType::Number,
    });

    let entry = compiler.context.append_basic_block(function, "entry");
    compiler.builder.position_at_end(entry);

    let cond = compiler
        .builder
        .build_load(
            compiler.context.f64_type(),
            function.get_first_param().unwrap().into_pointer_value(),
            "cond",
        )
        .into_float_value();
    let cond = compiler.builder.build_float_compare(
        FloatPredicate::ONE,
        cond,
        // rhs is discarded
        compiler.context.f64_type().const_float(0.0),
        "ifcond",
    );

    let then_bb = compiler.context.append_basic_block(function, "then");
    let else_bb = compiler.context.append_basic_block(function, "else");
    let cont_bb = compiler.context.append_basic_block(function, "ifcont");

    compiler
        .builder
        .build_conditional_branch(cond, then_bb, else_bb);

    compiler.builder.position_at_end(then_bb);
    let then_val = compiler
        .builder
        .build_load(
            compiler.context.f64_type(),
            function.get_nth_param(1).unwrap().into_pointer_value(),
            "then",
        )
        .into_float_value();
    compiler.builder.build_unconditional_branch(cont_bb);

    // build else block
    compiler.builder.position_at_end(else_bb);
    let else_val = compiler
        .builder
        .build_load(
            compiler.context.f64_type(),
            function.get_nth_param(2).unwrap().into_pointer_value(),
            "else",
        )
        .into_float_value();
    compiler.builder.build_unconditional_branch(cont_bb);

    // emit merge block
    compiler.builder.position_at_end(cont_bb);
    let phi = compiler
        .builder
        .build_phi(compiler.context.f64_type(), "iftmp");

    phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
    compiler.builder.build_return(Some(&phi.as_basic_value()));

    main_function
        .scope
        .variables
        .insert("if".to_string(), Box::new(function.as_any_value_enum()));
}
