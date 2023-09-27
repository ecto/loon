use inkwell::{module::Linkage, values::AnyValue, values::FunctionValue, AddressSpace};

use crate::compiler::{Function, LoonCompiler, LoonType, Prototype};

fn inject_printf<'ctx>(
    compiler: &LoonCompiler<'ctx>,
    _main_function: &Function,
) -> FunctionValue<'ctx> {
    let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
    let printf_type = compiler
        .context
        .i32_type()
        .fn_type(&[str_type.into()], true);
    let printf = compiler
        .module
        .add_function("printf", printf_type, Some(Linkage::External));
    printf
}

pub fn inject<'ctx>(compiler: &LoonCompiler<'ctx>, main_function: &mut Function<'ctx>) {
    let printf = inject_printf(compiler, main_function);

    compiler.builder.position_at_end(main_function.scope.entry);
    let format_string = (*compiler)
        .builder
        .build_global_string_ptr("%d\n", "format")
        .as_pointer_value();

    let function = compiler.compile_prototype(&Prototype {
        name: "log".to_string(),
        args: vec!["value".to_string()],
        external: false,
        return_type: LoonType::Nil,
    });
    let entry = compiler.context.append_basic_block(function, "entry");

    compiler.builder.position_at_end(entry);

    let value = compiler
        .builder
        .build_load(
            compiler.context.f64_type(),
            function.get_first_param().unwrap().into_pointer_value(),
            "value",
        )
        .into_float_value();
    let value =
        compiler
            .builder
            .build_float_to_signed_int(value, compiler.context.i64_type(), "tmpint");
    let value = compiler.builder.build_int_to_ptr(
        value,
        compiler.context.i8_type().ptr_type(AddressSpace::default()),
        "tmpptr",
    );

    compiler
        .builder
        .build_call(printf, &[format_string.into(), value.into()], "tmpcall");
    compiler.builder.build_return(None);

    main_function
        .scope
        .variables
        .insert("log".to_string(), Box::new(function.as_any_value_enum()));
}
