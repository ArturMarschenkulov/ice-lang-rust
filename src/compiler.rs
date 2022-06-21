//use crate::ast::{Stmt, Expr};
//extern crate llvm_sys as llvm;
//use std::collections::HashMap;
//use std::ffi::CString;
//use crate::token::{TokenKind, LiteralKind, Token};
//use crate::token::{PunctuatorKind};
//
//
//pub fn get_asm_from_ast(statements: &Vec<Box<Stmt>>) {
//    Compiler::new().compile_stmts(statements);
//}
//
//struct Compiler {
//    context: llvm::prelude::LLVMContextRef,
//    builder: llvm::prelude::LLVMBuilderRef,
//    module:  llvm::prelude::LLVMModuleRef,
//
//    //named_values: HashMap<String, Option<llvm::prelude::LLVMValueRef>>,
//    named_values: HashMap<Token, Option<llvm::prelude::LLVMValueRef>>,
//
//}
//impl Drop for Compiler {
//    fn drop(&mut self) -> () {
//        unsafe {
//
//            let out_file = CString::new("out.ll").unwrap();
//            llvm::core::LLVMPrintModuleToFile(self.module, out_file.as_ptr(), std::ptr::null_mut());
//
//            llvm::core::LLVMDisposeBuilder(self.builder);
//            llvm::core::LLVMDisposeModule(self.module);
//            llvm::core::LLVMContextDispose(self.context);
//        }
//    }
//}
//impl Compiler {
//    fn new() -> Self {
//        unsafe {
//            let context = llvm::core::LLVMContextCreate();
//            let builder = llvm::core::LLVMCreateBuilderInContext(context);
//            let module = llvm::core::LLVMModuleCreateWithName(b"ice_test_module\0".as_ptr() as * const _);
//
//            let int_type = llvm::core::LLVMInt64TypeInContext(context);
//            let function_type = llvm::core::LLVMFunctionType(int_type, std::ptr::null_mut(), 0, 0);
//            let main_function = llvm::core::LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);
//
//
//            let entry_name = CString::new("entry").unwrap();
//            let basic_block = llvm::core::LLVMAppendBasicBlockInContext(context, main_function, entry_name.as_ptr());
//            llvm::core::LLVMPositionBuilderAtEnd(builder, basic_block);
//
//            Self {
//                context,
//                builder,
//                module,
//                named_values: HashMap::new(),
//            }
//        }
//        //unimplemented!()
//    }
//
//    fn compile_stmts(&mut self, statements: &Vec<Box<Stmt>>) -> () {
//        unsafe {
//            let int_type = llvm::core::LLVMInt64TypeInContext(self.context);
//            let mut return_value = llvm::core::LLVMConstInt(int_type, 0, 0); // return value on empty program
//            for stmt in statements {
//                if let Some(rv) = self.compile_stmt(stmt) {
//                    return_value = rv;
//                }
//            }
//            llvm::core::LLVMBuildRet(self.builder, return_value);
//        }
//    }
//    unsafe fn compile_stmt(&mut self, statement: &Box<Stmt>) -> Option<llvm::prelude::LLVMValueRef> {
//        match **statement {
//            Stmt::VarDeclaration(ref name, ref init) => {
//                match init {
//                    Some(expr) => {
//                        let val = self.compile_expr(expr);
//                        self.named_values.insert(name.clone(), Some(val.clone()));
//                    },
//                    None => {
//                        self.named_values.insert(name.clone(), None);
//                    }
//                }
//                None
//            },
//            Stmt::Expression(ref expr) => {
//                Some(self.compile_expr(expr))
//            },
//            Stmt::Println(ref expr) => {
//                let res = self.compile_expr(expr);
//                println!("{:?}", Some(res))
//            }
//            ref x => unimplemented!("{}", format!("{:?} not implemented", &x))
//        }
//    }
//    unsafe fn compile_expr(&mut self, expression: &Box<Expr>) -> llvm::prelude::LLVMValueRef {
//
//        match **expression {
//            Expr::Literal(ref literal) => {
//
//                match literal {
//                    LiteralKind::Integer(ref integer) => {
//                        let int_type = llvm_sys::core::LLVMInt64TypeInContext(self.context);
//                        llvm_sys::core::LLVMConstInt(int_type, *integer as u64, 1)
//                    },
//                    x => unimplemented!("{}", format!("{:?} not implemented", &x))
//                }
//
//
//                //unimplemented!()
//            },
//            Expr::Symbol(ref symbol) => {
//                self.named_values.get(&symbol).unwrap().unwrap()
//            },
//            Expr::Binary(ref l, ref t, ref r) => {
//                let lhs = self.compile_expr(l);
//                let rhs = self.compile_expr(r);
//
//                use TokenKind::*;
//                use PunctuatorKind::*;
//                match t.kind.clone() {
//                    Punctuator(Plus)  => llvm::core::LLVMBuildAdd (self.builder, lhs, rhs, CString::new("addtmp").unwrap().as_ptr()),
//                    Punctuator(Minus) => llvm::core::LLVMBuildSub (self.builder, lhs, rhs, CString::new("subtmp").unwrap().as_ptr()),
//                    Punctuator(Star)  => llvm::core::LLVMBuildMul (self.builder, lhs, rhs, CString::new("multmp").unwrap().as_ptr()),
//                    Punctuator(Slash) => llvm::core::LLVMBuildUDiv(self.builder, lhs, rhs, CString::new("divtmp").unwrap().as_ptr()),
//                    x => unimplemented!("{}", format!("{:?} not implemented", &x))
//                }
//            }
//            ref expr => unimplemented!("{}", format!("{:?} not implemented", &expr))
//        }
//    }
//}
