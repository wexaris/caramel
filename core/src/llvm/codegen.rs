use crate::source::code_source::CodeSourceType;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn parse_module(context: &'ctx Context, module: crate::ast::Module) -> Module<'ctx> {
        let source_name = match module.origin.source_type() {
            CodeSourceType::File(path) => path.to_str().expect("Invalid UTF-8 path"),
            CodeSourceType::String => "_anonymous_string",
        };
        let codegen = Self {
            context,
            module: context.create_module(source_name),
            builder: context.create_builder(),
        };

        for decl in &module.decls {
            codegen.parse_decl(decl);
        }

        codegen.module
    }

    fn parse_decl(&self, decl: &crate::ast::Decl) {
        match decl {
            crate::ast::Decl::Func(decl) => {
                self.parse_decl_func(decl);
            }
        }
    }

    fn parse_decl_func(&self, decl: &crate::ast::FuncDecl) -> Option<FunctionValue<'ctx>> {
        let func = self.parse_func_prototype(&decl.proto);

        // Create entry basic block
        let bb = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(bb);

        for (i, val) in func.get_param_iter().enumerate() {
            let param = &decl.proto.params[i];
            let alloc = self.build_entry_block_alloca(func, &param.ty.ty, &param.id.name);
            self.builder.build_store(alloc, val).unwrap();

            // TODO: Save variable allocation to scope stack
        }

        self.parse_block(&decl.block);

        // Validate function
        if !func.verify(true) {
            println!(
                "Invalid function definition: {}",
                func.get_name().to_string_lossy()
            );
            func.print_to_stderr();
            unsafe { func.delete() };
            return None;
        }

        Some(func)
    }

    fn parse_func_prototype(&self, decl: &crate::ast::FuncPrototype) -> FunctionValue<'ctx> {
        let fn_name = decl.id.name.as_str();

        // Collect function parameter types
        let fn_param_types = decl
            .params
            .iter()
            .map(|param| param.ty.ty.to_llvm_type(&self.context).into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        // Create function definition
        let fn_type = decl
            .return_ty
            .ty
            .to_llvm_type(&self.context)
            .fn_type(&fn_param_types, false);

        // Add function to module
        let fn_val = self.module.add_function(fn_name, fn_type, None);

        // Set function attributes
        for (i, param) in fn_val.get_param_iter().enumerate() {
            let name = &decl.params[i].id.name;
            param.set_name(name.as_str());
        }

        fn_val
    }

    fn parse_block(&self, block: &crate::ast::Block) {
        for stmt in &block.stmts {
            self.parse_stmt(stmt);
        }
    }

    fn parse_stmt(&self, stmt: &crate::ast::Stmt) {
        match stmt {
            crate::ast::Stmt::Return(stmt) => self.parse_return(stmt),
            crate::ast::Stmt::Expr(expr) => {
                self.parse_expr(&expr.borrow());
            }
        }
    }

    fn parse_return(&self, stmt: &crate::ast::Return) {
        match &stmt.expr {
            Some(expr) => {
                let val = self.parse_expr(&expr.borrow());
                self.builder
                    .build_return(Some(&val))
                    .expect("Failed to build return statement");
            }
            None => {
                self.builder
                    .build_return(None)
                    .expect("Failed to build return statement");
            }
        }
    }

    fn parse_expr(&self, expr: &crate::ast::Expr) -> BasicValueEnum {
        match expr {
            crate::ast::Expr::FuncCall(_) => self.context.i32_type().const_int(0, false).into(),
            crate::ast::Expr::BinaryOp(_) => unimplemented!("Binary operations not yet supported"),
            crate::ast::Expr::VarAccess(_) => unimplemented!("Variable access not yet supported"),
            crate::ast::Expr::Lit(lit) => self.parse_literal(lit).into(),
        }
    }

    fn parse_literal(&self, lit: &crate::ast::Literal) -> IntValue {
        match lit.ty {
            crate::ast::ValueType::Integer => {
                let val = lit
                    .raw_str
                    .parse::<i64>()
                    .expect("Failed to parse integer literal");
                self.context.i32_type().const_int(val as u64, true)
            }
            crate::ast::ValueType::Real => unimplemented!("Real literals not yet supported"),
            crate::ast::ValueType::String => unimplemented!("String literals not yet supported"),
            crate::ast::ValueType::Char => unimplemented!("Char literals not yet supported"),
            crate::ast::ValueType::Bool(val) => {
                self.context.bool_type().const_int(val as u64, false)
            }
        }
    }

    fn build_entry_block_alloca(
        &self,
        func: FunctionValue,
        ty: &crate::ast::TypeType,
        name: &str,
    ) -> PointerValue {
        let builder = self.context.create_builder();

        let entry = func
            .get_first_basic_block()
            .expect("Function must have at least one basic block");
        match entry.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(entry),
        }

        let llvm_ty = ty.to_llvm_type(&self.context);
        builder
            .build_alloca(llvm_ty, name)
            .expect("Failed to create alloca")
    }
}

trait ToLLVMType {
    fn to_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx>;
}

impl ToLLVMType for crate::ast::TypeType {
    fn to_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            crate::ast::TypeType::I64 => context.i64_type().as_basic_type_enum(),
            crate::ast::TypeType::I32 => context.i32_type().as_basic_type_enum(),
            crate::ast::TypeType::I16 => context.i16_type().as_basic_type_enum(),
            crate::ast::TypeType::I8 => context.i8_type().as_basic_type_enum(),
            crate::ast::TypeType::U64 => unimplemented!("Unsigned types not yet supported"),
            crate::ast::TypeType::U32 => unimplemented!("Unsigned types not yet supported"),
            crate::ast::TypeType::U16 => unimplemented!("Unsigned types not yet supported"),
            crate::ast::TypeType::U8 => unimplemented!("Unsigned types not yet supported"),
            crate::ast::TypeType::F64 => context.f64_type().as_basic_type_enum(),
            crate::ast::TypeType::F32 => context.f32_type().as_basic_type_enum(),
            crate::ast::TypeType::String => unimplemented!("Array types not yet supported"),
            crate::ast::TypeType::Char => context.i8_type().as_basic_type_enum(),
            crate::ast::TypeType::Bool => context.bool_type().as_basic_type_enum(),
            crate::ast::TypeType::Custom(_) => unimplemented!("Custom types not yet supported"),
        }
    }
}
