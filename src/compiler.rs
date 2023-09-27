use super::parser;

use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassManager;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, FunctionValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};

mod stdlib;

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
pub struct Function<'a> {
    pub prototype: Prototype,
    pub body: *const parser::LoonNode,
    pub scope: Scope<'a>,
}

#[derive(Debug)]
pub struct Scope<'a> {
    variables: HashMap<String, Box<AnyValueEnum<'a>>>,
    // parent: Option<Box<Scope>>,
    entry: BasicBlock<'a>,
}

#[derive(Debug)]
pub enum LoonType {
    Number,
    Nil,
}

#[derive(Debug)]
pub struct LoonCompiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub fpm: PassManager<FunctionValue<'ctx>>,
    pub module: Module<'ctx>,
    pub debug: bool,
}

impl<'ctx> LoonCompiler<'ctx> {
    pub fn compile(&self, node: &parser::LoonNode) {
        let proto = Prototype {
            name: "main".to_string(),
            args: vec![],
            external: false,
            return_type: LoonType::Nil,
        };
        let function = self.compile_prototype(&proto);
        let scope = Scope {
            variables: HashMap::new(),
            // parent: None,
            entry: self.context.append_basic_block(function, "entry"),
        };
        let mut main_function = Function {
            prototype: proto,
            body: node,
            scope,
        };

        stdlib::inject(self, &mut main_function);
        self.compile_node(node, &main_function.scope);
        self.fpm.run_on(&function);
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

    fn compile_fn(&self, node: &parser::LoonNode) -> FunctionValue<'ctx> {
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
            scope.variables.insert(
                proto.args[i].clone(),
                Box::new(arg.into_pointer_value().as_any_value_enum()),
            );
        }

        let body = self.compile_node(node.children.last().unwrap(), &scope);

        self.builder.position_at_end(scope.entry);

        match proto.return_type {
            LoonType::Nil => {
                self.builder.build_return(None);
            }
            LoonType::Number => match body.get_type() {
                AnyTypeEnum::FloatType(_) => {
                    self.builder.build_return(Some(&body.into_float_value()));
                }
                AnyTypeEnum::PointerType(_) => {
                    self.builder.build_return(Some(&body.into_pointer_value()));
                }
                _ => panic!("Invalid return type"),
            },
        }

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
        let ret_type = self.context.f64_type().ptr_type(AddressSpace::default());
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
            arg.into_pointer_value().set_name(proto.args[i].as_str());
        }

        fn_val
    }

    fn call_fn(&self, node: &parser::LoonNode, scope: &Scope<'ctx>) -> AnyValueEnum<'ctx> {
        if self.debug {
            println!("call_fn: {:?}", node);
        }

        let fn_name = node.children[0].text.clone();
        let args = &node.children[1..];
        let fun = self
            .module
            .get_function(fn_name.as_str())
            .unwrap_or_else(|| panic!("Could not find function: {}", fn_name)); // TODO pretty error

        self.builder.position_at_end(scope.entry);

        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .map(|arg| {
                let node = self.compile_node(arg, scope);
                match node.get_type() {
                    AnyTypeEnum::FloatType(_) => {
                        let alloca = self.builder.build_alloca(self.context.f64_type(), "arg");
                        self.builder.build_store(alloca, node.into_float_value());
                        alloca.as_basic_value_enum().into()
                    }
                    AnyTypeEnum::PointerType(_) => node.into_pointer_value().into(),
                    _ => panic!("Invalid argument type"),
                }
            })
            .collect();

        let value = self
            .builder
            .build_call(fun, compiled_args.as_slice(), "tmp")
            .as_any_value_enum();

        value.into()
    }

    fn compile_node(&self, node: &parser::LoonNode, scope: &Scope<'ctx>) -> AnyValueEnum<'ctx> {
        if self.debug {
            println!("compile_node: {:?}", node);
        }

        match node.kind {
            parser::LoonNodeKind::Symbol => scope
                .variables
                .get(node.text.as_str())
                .unwrap_or_else(|| panic!("Could not find variable: {}", node.text))
                .as_any_value_enum(),

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

            parser::LoonNodeKind::List => match node.children[0].text.as_str() {
                "fn" => self.compile_fn(node).as_any_value_enum(),
                _ => self.call_fn(node, scope),
            },
        }
    }
}
