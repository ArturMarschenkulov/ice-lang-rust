
use super::ast::*;

pub trait DebugTreePrinter {
    fn print_debug_tree(&self);
}

impl<T> DebugTreePrinter for Vec<T>
where
    T: DebugTreePrinter,
{
    fn print_debug_tree(&self) {
        for tree_s in self {
            debug_tree::defer_print!();
            tree_s.print_debug_tree();
        }
    }
}

impl DebugTreePrinter for Project {
    fn print_debug_tree(&self) {
        debug_tree::defer_print!();
        debug_tree::add_branch!("Project: ");
        for module in &self.modules {
            module.print_debug_tree();
        }
    }
}
impl DebugTreePrinter for Module {
    fn print_debug_tree(&self) {
        debug_tree::defer_print!();
        debug_tree::add_branch!("Module: ");
        for item in &self.items {
            item.print_debug_tree();
        }
    }
}

impl DebugTreePrinter for Expr {
    fn print_debug_tree(&self) {
        match &self.kind {
            ExprKind::Literal(lit) => {
                debug_tree::add_branch!("Lit {:?}", lit);
            }
            ExprKind::Grouping(expr) => {
                debug_tree::add_branch!("Group: ");
                expr.print_debug_tree();
            }
            ExprKind::BinaryInfix(expr_l, op, expr_r) => {
                debug_tree::add_branch!("Binary: {:?}", op.name.kind);
                expr_l.print_debug_tree();
                expr_r.print_debug_tree();
            }
            ExprKind::UnaryPrefix(op, expr) => {
                debug_tree::add_branch!("Unary: {:?}", op.name.kind);
                expr.print_debug_tree();
            }
            ExprKind::Symbol { name, path } => {
                let s = if path.is_some() {
                    let s = path
                        .clone()
                        .unwrap()
                        .iter()
                        .map(|t| t.name().to_owned())
                        .collect::<Vec<String>>();
                    Some(s)
                } else {
                    None
                };
                debug_tree::add_branch!("Symbol: {}, {:?}", name.name.kind.as_str(), s);
            }
            ExprKind::Block(stmts) => {
                debug_tree::add_branch!("Block: ");
                for ast_s in stmts {
                    ast_s.print_debug_tree();
                }
            }
            ExprKind::If(comp, then_branch, else_branch) => {
                debug_tree::add_branch!("If: ");
                comp.print_debug_tree();
                then_branch.print_debug_tree();
                if let Some(block) = else_branch {
                    block.print_debug_tree();
                }
            }
            ExprKind::While(comp, body) => {
                debug_tree::add_branch!("While: ");
                comp.print_debug_tree();
                body.print_debug_tree();
            }
            ExprKind::For(init, comp, expr, body) => {
                debug_tree::add_branch!("For: ");
                init.print_debug_tree();
                comp.print_debug_tree();
                expr.print_debug_tree();
                body.print_debug_tree();
            }
            ExprKind::FnCall(callable, parameters) => {
                debug_tree::add_branch!("FnCall: ");
                callable.print_debug_tree();
                {
                    debug_tree::add_branch!("parameters: ");
                    for _be1 in parameters {
                        _be1.print_debug_tree();
                    }
                }
            }
        }
    }
}

impl DebugTreePrinter for Stmt {
    fn print_debug_tree(&self) {
        match &self.kind {
            StmtKind::Var { var, ty, init } => {
                debug_tree::add_branch!("SVarDeclaration: ");
                debug_tree::add_leaf!("T: {:?}", var.name.kind);
                debug_tree::add_leaf!("T: {:?}", ty);
                match init.as_ref() {
                    Some(i) => i.print_debug_tree(),
                    None => debug_tree::add_leaf!("T: {:?}", init),
                }
                // init.as_ref().unwrap().print();
            }
            StmtKind::Expression(expr) => {
                debug_tree::add_branch!("SExpression: ");
                expr.print_debug_tree();
            }
            StmtKind::ExpressionWithoutSemicolon(expr) => {
                debug_tree::add_branch!("SExpressionWithoutSemicolon: ");
                expr.print_debug_tree();
            }
            StmtKind::Item(item) => {
                item.print_debug_tree();
            }
            StmtKind::NoOperation => {
                debug_tree::add_branch!("SNoOp");
            }
        }
    }
}

impl DebugTreePrinter for Ty {
    fn print_debug_tree(&self) {
        match &self.kind {
            TyKind::Simple(s) => {
                debug_tree::add_branch!("Simple: {:?}", s.name.kind);
            }
            TyKind::Tuple(tup) => {
                debug_tree::add_branch!("Tuple: ");
                for ty in tup {
                    ty.print_debug_tree();
                }
            }
        }
    }
}
impl DebugTreePrinter for Item {
    fn print_debug_tree(&self) {
        match &self.kind {
            ItemKind::Fn {
                name,
                params,
                ret,
                body,
            } => {
                debug_tree::add_branch!("SFnDeclaration: ");
                debug_tree::add_leaf!("name: {:?}", name.name.kind);
                {
                    debug_tree::add_branch!("parameters: ");

                    for param in params {
                        param.ty.print_debug_tree();
                    }
                }
                {
                    debug_tree::add_branch!("return type: ");
                    match ret {
                        Some(r) => r.print_debug_tree(),
                        None => (),
                    }
                }
                body.print_debug_tree();
            }
            ItemKind::Struct { name, fields } => {
                debug_tree::add_branch!("SStructDeclaration: ");
                debug_tree::add_leaf!("T: {:?}", name.name.kind);
                {
                    debug_tree::add_branch!("fields: ");
                    for field in fields {
                        field.ty.print_debug_tree();
                    }
                }
            }
        }
    }
}