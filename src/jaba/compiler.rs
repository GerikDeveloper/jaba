use crate::jaba::codegen::CodeGen;
use crate::jaba::errors_handler::ErrorsHandler;
use crate::jaba::lex_analyzer::{LexAnalyzer};
use crate::jaba::parser::Parser;
use crate::jaba::src_driver::SrcDriver;
use crate::jaba::table::{Cat, ScopeType, Table, Type};

pub struct Compiler {
    src_path: String,
    output_path: String,
}

impl Compiler {
    pub fn new(src_path: String, output_path: String) -> Self {
        return Self {
            src_path,
            output_path,
        };
    }

    pub async fn compile(&self) {
        if let Some(src_driver) = SrcDriver::new(self.src_path.clone()).await {
            let mut codegen = CodeGen::new();
            codegen.init();
            let lex_analyzer = LexAnalyzer::new(src_driver);
            let mut table = Table::new();

            table.open_scope(&ScopeType::Module);

            table.enter(String::from("s8"), Cat::Tipo, Type::S8, 0);
            table.enter(String::from("e8"), Cat::Tipo, Type::E8, 0);
            table.enter(String::from("s16"), Cat::Tipo, Type::S16, 0);
            table.enter(String::from("e16"), Cat::Tipo, Type::E16, 0);
            table.enter(String::from("bool"), Cat::Tipo, Type::Bool, 0);

            table.open_scope(&ScopeType::Module);

            let mut parser = Parser::new(lex_analyzer, table, codegen);

            parser.parse();

            let mut table = parser.get_table();
            let mut codegen = parser.get_codegen();

            table.close_scope(&mut codegen);

            table.close_scope(&mut codegen);

            codegen.end();

            codegen.commit(self.output_path.clone()).await;

            ErrorsHandler::notice_0000();
        } else {
            ErrorsHandler::error_0001();
        }
    }
}