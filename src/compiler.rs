use super::parser;

use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, FunctionValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;

/// Defines the prototype (name and parameters) of a function.
#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub external: bool,
    pub return_type: LoonType,
}

/// Defines a user-defined or external function.
#[derive(Debug)]
pub struct Function<'ctx> {
    pub prototype: Prototype,
    pub body: *const parser::LoonNode,
    pub scope: Scope<'ctx>,
}

#[derive(Debug)]
pub struct Scope<'ctx> {
    variables: HashMap<String, AnyValueEnum<'ctx>>,
    // parent: Option<Box<Scope<'ctx>>>,
    entry: BasicBlock<'ctx>,
}

pub struct LoonCompiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub module: Module<'ctx>,
    // pub function: &'ctx Function,
    // pub fn_value: FunctionValue<'ctx>,
}

#[derive(Debug)]
pub enum LoonType {
    Number,
    Nil,
}

impl<'ctx> LoonCompiler<'ctx> {
    pub fn compile(&mut self, node: &parser::LoonNode) {
        let proto = Prototype {
            name: "main".to_string(),
            args: vec![],
            external: false,
            return_type: LoonType::Nil,
        };
        let function = self.compile_prototype(&proto);
        let main_function = Function {
            prototype: proto,
            body: node,
            scope: Scope {
                variables: HashMap::new(),
                // parent: None,
                entry: self.context.append_basic_block(function, "entry"),
            },
        };

        // TODO allocate args
        self.compile_node(node, &main_function.scope);

        // self.fpm.run_on(&function);

        self.builder.build_return(None);
    }

    pub fn get_code(&self) -> String {
        self.module.to_string()
    }

    pub fn execute(&self) {
        let ee = self
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        unsafe {
            let compiled_fn = ee
                .get_function::<unsafe extern "C" fn() -> f64>("main")
                .unwrap_or_else(|err| panic!("Failed to find function main: {}", err));
            compiled_fn.call();
        }
    }

    // / Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(
        &self,
        name: &str,
        function: &FunctionValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = function.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    fn compile_fn(&mut self, node: &parser::LoonNode) -> FunctionValue<'ctx> {
        let proto = Prototype {
            name: node.children[1].text.clone(),
            args: node.children[2]
                .children
                .iter()
                .map(|arg| arg.text.clone())
                .collect(),
            external: false,
            return_type: LoonType::Number,
        };
        let function = self.compile_prototype(&proto);
        let mut scope = Scope {
            variables: HashMap::new(),
            // parent: None,
            entry: self.context.append_basic_block(function, "entry"),
        };

        // TODO
        // if function.body.is_none() {
        //     return function;
        // }

        scope.variables.reserve(proto.args.len());

        self.builder.position_at_end(scope.entry);

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].as_str();
            let alloca = self.create_entry_block_alloca(arg_name, &function);

            self.builder.build_store(alloca, arg);

            scope
                .variables
                .insert(proto.args[i].clone(), alloca.as_any_value_enum());
        }

        let body = self.compile_node(node.children.last().unwrap(), &scope);

        // TODO
        self.builder.position_at_end(scope.entry);
        self.builder.build_return(Some(&body.into_float_value()));

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            function
        } else {
            println!(
                "Invalid generated function: {:?}",
                function.print_to_string()
            );
            unsafe {
                function.delete();
            }
            panic!();
        }
    }

    fn compile_prototype(&self, proto: &Prototype) -> FunctionValue<'ctx> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        // self.context.f64_type().fn_type(args_types, false);
        let fn_type = match proto.return_type {
            LoonType::Nil => self.context.void_type().fn_type(args_types, false),
            LoonType::Number => self.context.f64_type().fn_type(args_types, false),
        };
        let fn_val = self.module.add_function(
            proto.name.as_str(),
            fn_type,
            if proto.external {
                Some(Linkage::External)
            } else {
                None
            },
        );

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        fn_val
    }

    fn call_fn(&mut self, node: &parser::LoonNode, scope: &Scope<'ctx>) -> AnyValueEnum<'ctx> {
        let fn_name = node.children[0].text.clone();
        let args = &node.children[1..];
        let fun = self
            .module
            .get_function(fn_name.as_str())
            .unwrap_or_else(|| panic!("Could not find function: {}", fn_name)); // TODO pretty error

        let mut compiled_args = Vec::with_capacity(args.len());

        for arg in args {
            compiled_args.push(self.compile_node(arg, scope));
        }

        let argsv: Vec<BasicMetadataValueEnum> = compiled_args
            .iter()
            .by_ref()
            .map(|&val| val.into_float_value().into())
            .collect();

        self.builder.position_at_end(scope.entry);

        let value = self
            .builder
            .build_call(fun, argsv.as_slice(), "tmp")
            .try_as_basic_value()
            .left()
            .unwrap();

        value.into()
    }

    fn compile_node(&mut self, node: &parser::LoonNode, scope: &Scope<'ctx>) -> AnyValueEnum<'ctx> {
        match node.kind {
            // find variable in scope
            parser::LoonNodeKind::Symbol => {
                match node.text.as_str() {
                    "true" => self.context.f64_type().const_float(1.0).as_any_value_enum(),
                    "false" => self.context.f64_type().const_float(0.0).as_any_value_enum(),
                    _ => {
                        // self.context.f64_type().const_float(0.0).as_any_value_enum()
                        let pointer = scope
                            .variables
                            .get(node.text.as_str())
                            .unwrap_or_else(|| panic!("Could not find variable: {}", node.text))
                            .into_pointer_value();

                        // TODO store variable type in scope
                        // let pointer_type = pointer.get_type();
                        let pointer_type = self.context.f64_type();
                        self.builder
                            .build_load(pointer_type, pointer, node.text.as_str())
                            .as_any_value_enum()
                    }
                }
                // unreachable!("{:?}", node),
            }

            parser::LoonNodeKind::Number => self
                .context
                .f64_type()
                .const_float(node.text.parse::<f64>().unwrap())
                .as_any_value_enum(),

            parser::LoonNodeKind::Program => {
                let mut ret = self.context.f64_type().const_float(0.0).as_any_value_enum();
                for child in &node.children {
                    ret = self.compile_node(child, scope);
                }
                ret
            }

            // Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
            //     Some(var) => Ok(self
            //         .builder
            //         .build_load(*var, name.as_str())
            //         .into_float_value()),
            //     None => Err("Could not find a matching variable."),
            // },
            parser::LoonNodeKind::List => {
                match node.children[0].text.as_str() {
                    "fn" => self.compile_fn(node).as_any_value_enum(),

                    "+" => {
                        let lhs = self
                            .compile_node(&node.children[1], scope)
                            .into_float_value();
                        let rhs = self
                            .compile_node(&node.children[2], scope)
                            .into_float_value();
                        self.builder.position_at_end(scope.entry);
                        let body = self.builder.build_float_add(lhs, rhs, "tmpadd");
                        // self.builder.build_return(Some(&body));
                        body.as_any_value_enum()
                    }

                    "log" => {
                        let value = self.compile_node(&node.children[1], scope);
                        let value = value.into_float_value();
                        let value = self.builder.build_float_to_signed_int(
                            value,
                            self.context.i64_type(),
                            "tmpint",
                        );
                        let value = self.builder.build_int_to_ptr(
                            value,
                            self.context.i8_type().ptr_type(AddressSpace::default()),
                            "tmpptr",
                        );

                        self.builder.position_at_end(scope.entry);
                        // let format_string = self.context.const_string(b"%f\n", false);
                        let format_string = self
                            .builder
                            .build_global_string_ptr("%d\n", "format")
                            .as_pointer_value();
                        // .as_basic_value_enum();

                        let str_type = self.context.i8_type().ptr_type(AddressSpace::default());
                        let printf_type = self.context.i32_type().fn_type(&[str_type.into()], true);
                        let printf = self.module.add_function(
                            "printf",
                            printf_type,
                            Some(Linkage::External),
                        );
                        // let printf_prototype = self.compile_prototype(&Prototype {
                        //     name: "printf".to_string(),
                        //     args: vec!["format".to_string()],
                        //     external: true,
                        //     return_type: LoonType::Nil,
                        // });

                        let call = self.builder.build_call(
                            printf,
                            &[format_string.into(), value.into()],
                            "tmpcall",
                        );
                        call.as_any_value_enum()
                    }

                    _ => self.call_fn(node, scope),
                }
            }
        }
    }
}
