use crate::parser::ast::{Field, Identifier, ItemKind, Parameter, Project, Ty};

// Note: Variables, functions and types should have different namespaces

#[derive(Debug)]
enum SymbolKind {
    Fn {
        name: Identifier,
        params: Vec<Parameter>,
        ret: Ty,
    },
    Struct {
        name: Identifier,
        fields: Vec<Field>,
    },
}
impl SymbolKind {
    fn name(&self) -> &str {
        match self {
            SymbolKind::Fn { name, .. } => name.name(),
            SymbolKind::Struct { name, .. } => name.name(),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    types: Vec<SymbolKind>,
    fns: Vec<SymbolKind>,
}

impl SymbolTable {
    fn insert(&mut self, symbol: ItemKind) -> Result<(), String> {
        if self.types.iter().any(|s| s.name() == symbol.name()) {
            return Err(format!(
                "Symbol name {} already exists in the type symbol table",
                symbol.name()
            ));
        }
        if self.fns.iter().any(|s| s.name() == symbol.name()) {
            return Err(format!(
                "Symbol name {} already exists in the function symbol table",
                symbol.name()
            ));
        }

        let symbol = gen_symbol(&symbol);
        Ok(match symbol {
            SymbolKind::Fn { .. } => self.fns.push(symbol),
            SymbolKind::Struct { .. } => self.types.push(symbol),
        })
    }
}

fn gen_symbol(item: &ItemKind) -> SymbolKind {
    match item {
        ItemKind::Fn {
            name,
            params,
            ret,
            body: _,
        } => SymbolKind::Fn {
            name: name.clone(),
            params: params.clone(),
            ret: ret.clone().unwrap_or(Ty::unit()),
        },
        ItemKind::Struct { name, fields } => SymbolKind::Struct {
            name: name.clone(),
            fields: fields.clone(),
        },
        _ => todo!(),
    }
}
pub fn check_project(project: &Project) -> Result<SymbolTable, String> {
    let mut symbol_table = SymbolTable {
        types: Vec::new(),
        fns: Vec::new(),
    };
    for module in &project.modules {
        for item in &module.items {
            let _ = match symbol_table.insert(item.kind.clone()) {
                Ok(symbol) => symbol,
                Err(err) => {
                    panic!("Error: {}", err);
                }
            };
        }
    }
    Ok(symbol_table)
}
