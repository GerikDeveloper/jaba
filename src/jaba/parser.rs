use std::mem;
use std::ops::Deref;
use crate::jaba::codegen::{CodeGen, ExprCmd, ExprGenBuf, ImportGenBuf, StdModule};
use crate::jaba::errors_handler::ErrorsHandler;
use crate::jaba::lex_analyzer::{Lex, LexAnalyzer};
use crate::jaba::parser::SPCode::{High, Low, Prok, FikDDRB, FikPORTB, SkrKar};
use crate::jaba::table::{Cat, EntryRef, ScopeType, Table, Type, TypeRange, TypeSize};


pub(crate) mod SPCode { //StdProc
    pub(crate) const SkrKar: i32 = 1;
    pub(crate) const Prok: i32 = 2;
    pub(crate) const Low: i32 = 3;
    pub(crate) const High: i32 = 4;
    pub(crate) const FikDDRB: i32 = 5; //Fiksu
    pub(crate) const FikPORTB: i32 = 6; //Fiksu
}

pub(crate) struct Parser {
    lex_analyzer: LexAnalyzer,
    table: Table,
    codegen: CodeGen,
    cur_fn_name: Option<String>,
}

impl Parser {
    pub(crate) fn get_table(&self) -> Table { return self.table.clone(); }

    pub(crate) fn get_codegen(&self) -> CodeGen { return self.codegen.clone(); }

    pub(crate) fn new(lex_analyzer: LexAnalyzer, table: Table, codegen: CodeGen) -> Self {
        return Self {
            lex_analyzer,
            table,
            codegen,
            cur_fn_name: None,
        };
    }

    fn mismatched(&self) {
        ErrorsHandler::error_0100(Some(self.lex_analyzer.get_glob_pos()));
    }

    fn check(&mut self, lex: Lex, expected: Option<String>) {
        if mem::discriminant(&self.lex_analyzer.get_lex()) != mem::discriminant(&lex) {
            ErrorsHandler::error_0010(
                expected,
                None,
                self.lex_analyzer.get_glob_pos(),
            );
        } else {
            self.lex_analyzer.next_lex();
        }
    }

    fn uzante(&mut self) -> EntryRef {
        //DUMMY CHECK
        self.check(Lex::Uzi, Some(String::from("['uzi']")));

        let mut res: EntryRef = None;
        //Dummy Check
        if let Lex::Name(name) = self.lex_analyzer.get_lex() {
            res = self.table.new_name(name.clone(), Cat::Mod);
            match name.as_str() {
                "elen" => {
                    self.codegen.import_buf.gen(StdModule::ElEn);
                    self.table.enter_sp(SkrKar);
                    self.table.enter_sp(FikDDRB);
                    self.table.enter_sp(FikPORTB);
                }
                "tempo" => {
                    self.codegen.import_buf.gen(StdModule::Tempo);
                    self.table.enter_sp(Prok);
                }
                "matem" => {
                    self.codegen.import_buf.gen(StdModule::Matem);
                    self.table.enter_sp(Low);
                    self.table.enter_sp(High);
                }
                _ => {
                    ErrorsHandler::error_0210();
                }
            }
        } else {
            ErrorsHandler::error_0211(self.lex_analyzer.get_glob_pos());
        }
        self.lex_analyzer.next_lex();
        self.check(Lex::Semi, Some(String::from("[';']")));
        return res;
    }

    fn uzantej(&mut self) {
        while self.lex_analyzer.get_lex() == Lex::Uzi {
            self.uzante();
        }
    }

    fn konst_expr(&mut self, decl_typ: Type) -> i32 { //nonesp //['-'] (name | num | malv | vera)
        let flag: bool = if self.lex_analyzer.get_lex() == Lex::Minus {
            self.lex_analyzer.next_lex();
            true
        } else { false };
        let mut val: i32 = 0;
        if let Lex::Name(name) = self.lex_analyzer.get_lex() {
            let ent = self.table.find(
                name,
                Some(self.lex_analyzer.get_glob_pos()),
            );
            match ent.borrow().cat {
                Cat::Konst => {
                    if ent.borrow().typ != decl_typ { self.mismatched(); }
                    if flag && decl_typ == Type::Bool {
                        ErrorsHandler::error_0310(self.lex_analyzer.get_glob_pos());
                    }
                    let ent_val = (if flag { -1 } else { 1 }) * ent.borrow().val;
                    let range = TypeRange::from(decl_typ);
                    if ent_val >= range.min &&
                        ent_val <= range.max { val = ent_val; } else {
                        ErrorsHandler::error_0102(self.lex_analyzer.get_glob_pos());
                    }
                }
                Cat::Bar => {
                    ErrorsHandler::error_0410(self.lex_analyzer.get_glob_pos());
                }
                _ => {
                    ErrorsHandler::error_0010(
                        Some(String::from("[nomo de konst var]")),
                        None,
                        self.lex_analyzer.get_glob_pos()
                    );
                }
            };
        } else if let Lex::Num(mut num) = self.lex_analyzer.get_lex() {
            let range = TypeRange::from(decl_typ);
            num *= if flag { -1 } else { 1 };
            if num >= range.min && num <= range.max { val = num; } else {
                ErrorsHandler::error_0102(self.lex_analyzer.get_glob_pos());
            }
        } else if self.lex_analyzer.get_lex() == Lex::Malv ||
            self.lex_analyzer.get_lex() == Lex::Vera {
            if decl_typ == Type::Bool {
                if !flag {
                    val = if self.lex_analyzer.get_lex() == Lex::Malv { 0 } else { 1 };
                } else { ErrorsHandler::error_0310(self.lex_analyzer.get_glob_pos()); }
            } else { self.mismatched(); }
        } else {
            ErrorsHandler::error_0010(
                Some(String::from("[nomo, 'vera', 'malv', num]")),
                None,
                self.lex_analyzer.get_glob_pos(),
            );
        }
        self.lex_analyzer.next_lex();
        return val;
    }

    fn konst(&mut self) {
        //Dummy check
        self.check(Lex::Konst, Some(String::from("[konst]")));
        if let Lex::Name(name) = self.lex_analyzer.get_lex() {
            let ent_ref = self.table.new_name(name, Cat::Bar);
            if let Some(ent) = ent_ref {
                self.lex_analyzer.next_lex();
                self.check(Lex::Colon, Some(String::from("[':']")));
                if let Lex::Name(name) = self.lex_analyzer.get_lex() {
                    let typ_ent = self.table.find(
                        name.clone(),
                        Some(self.lex_analyzer.get_glob_pos()),
                    );
                    if typ_ent.borrow().cat != Cat::Tipo {
                        ErrorsHandler::error_0101(
                            name,
                            self.lex_analyzer.get_glob_pos(),
                        );
                    }
                    let decl_typ = typ_ent.borrow().typ.clone();
                    self.lex_analyzer.next_lex();
                    self.check(Lex::Ass, Some(String::from("['=']")));
                    let val = self.konst_expr(decl_typ.clone());
                    let mut ent_mut = ent.borrow_mut();
                    ent_mut.typ = decl_typ;
                    ent_mut.val = val;
                    ent_mut.cat = Cat::Konst;
                } else {
                    ErrorsHandler::error_0010(
                        Some(String::from("[nomo]")),
                        None,
                        self.lex_analyzer.get_glob_pos(),
                    );
                }
            } else { ErrorsHandler::dev_error_0610(); }
        } else {
            ErrorsHandler::error_0010(
                Some(String::from("[nomo]")),
                None,
                self.lex_analyzer.get_glob_pos(),
            );
        }
        self.check(Lex::Semi, Some(String::from("[';']")));
    }

    fn deklaracioj(&mut self) {
        'decl:
        loop {
            match self.lex_analyzer.get_lex() {
                Lex::Konst => {
                    self.konst();
                }
                _ => {
                    break 'decl;
                }
            }
        }
    }

    //Expression

    fn expression(&mut self, buf: &mut ExprGenBuf) -> Type {
        return match self.lex_analyzer.get_lex() {
            Lex::Not => {
                self.lex_analyzer.next_lex();
                let typ = self.term(buf);
                match typ {
                    Type::Num(_) | Type::S8 | Type::E8 | Type::S16 | Type::E16 | Type::Bool => {}
                    _ => { self.mismatched(); }
                }
                buf.gen(ExprCmd::Not);
                typ.clone()
            }
            Lex::Minus => {
                self.lex_analyzer.next_lex();
                let typ = self.term(buf);
                match typ {
                    Type::Num(_) | Type::S8 | Type::E8 | Type::S16 | Type::E16 => {}
                    _ => { self.mismatched(); }
                }
                buf.gen(ExprCmd::Neg);
                typ.clone()
            }
            _ => {
                let typ = self.term(buf);
                match typ {
                    Type::Num(_) | Type::S8 | Type::E8 | Type::S16 | Type::E16 => {
                        let op = self.lex_analyzer.get_lex();
                        match op {
                            Lex::EQ | Lex::NE | Lex::GT | Lex::GE | Lex::LT | Lex::LE => {
                                self.lex_analyzer.next_lex();
                                let arg_typ = self.term(buf);
                                let mut res_typ = Type::None;
                                if let Some(ct) = Type::get_common_type(typ, arg_typ) {
                                    res_typ = ct;
                                    if let Type::Num(rng) = res_typ {
                                        if let Some(mt) = rng.get_min_type() {
                                            res_typ = mt;
                                        } else {
                                            self.mismatched();
                                            return Type::None;
                                        }
                                    }
                                    //Dummy check
                                    if res_typ == Type::None { ErrorsHandler::dev_error_0103(); }
                                    match op {
                                        Lex::EQ => { buf.gen(ExprCmd::EQ); }
                                        Lex::NE => { buf.gen(ExprCmd::NE); }
                                        Lex::GT => { buf.gen(ExprCmd::GT); }
                                        Lex::GE => { buf.gen(ExprCmd::GE); }
                                        Lex::LT => { buf.gen(ExprCmd::LT); }
                                        Lex::LE => { buf.gen(ExprCmd::LE); }
                                        _ => {
                                            //dummy cond
                                        }
                                    }
                                    buf.apply(&mut self.codegen, &res_typ);
                                    buf.clear();
                                    Type::Bool
                                } else {
                                    self.mismatched();
                                    Type::None
                                }
                            }
                            Lex::Plus |
                            Lex::Minus |
                            Lex::BAnd |
                            Lex::BOr |
                            Lex::Xor |
                            Lex::LS |
                            Lex::RS |
                            Lex::ROL |
                            Lex::ROR => {
                                //CAN BE NEG!!!
                                self.lex_analyzer.next_lex();
                                let arg_typ = self.term(buf);
                                match arg_typ {
                                    //DUMMY CHECK MB
                                    Type::Num(_) | Type::S8 | Type::E8 | Type::S16 | Type::E16 => {
                                        if let Some(mut ct) = Type::get_common_type(
                                            typ,
                                            arg_typ,
                                        ) {
                                            match op {
                                                Lex::Plus => { buf.gen(ExprCmd::Add); }
                                                Lex::Minus => { buf.gen(ExprCmd::Sub); }
                                                Lex::BAnd => { buf.gen(ExprCmd::And); }
                                                Lex::BOr => { buf.gen(ExprCmd::Or); }
                                                Lex::Xor => { buf.gen(ExprCmd::Xor); }
                                                Lex::LS => { buf.gen(ExprCmd::LS); }
                                                Lex::RS => { buf.gen(ExprCmd::RS); }
                                                Lex::ROL => { buf.gen(ExprCmd::ROL); }
                                                Lex::ROR => { buf.gen(ExprCmd::ROR); }
                                                _ => {
                                                    //dummy cond
                                                }
                                            }
                                            loop {
                                                let rep_op = self.lex_analyzer.get_lex();
                                                match rep_op {
                                                    Lex::Plus |
                                                    Lex::Minus |
                                                    Lex::BAnd |
                                                    Lex::BOr |
                                                    Lex::Xor |
                                                    Lex::LS |
                                                    Lex::RS |
                                                    Lex::ROL |
                                                    Lex::ROR => {
                                                        self.lex_analyzer.next_lex();
                                                        let arg_typ = self.term(buf);
                                                        match arg_typ {
                                                            Type::Num(_) |
                                                            Type::S8 |
                                                            Type::E8 |
                                                            Type::S16 |
                                                            Type::E16 => {
                                                                if let Some(nct) =
                                                                    Type::get_common_type(
                                                                        ct.clone(),
                                                                        arg_typ,
                                                                    ) {
                                                                    ct = nct;
                                                                    match op {
                                                                        Lex::Plus => {
                                                                            buf.gen(
                                                                                ExprCmd::Add
                                                                            );
                                                                        }
                                                                        Lex::Minus => {
                                                                            buf.gen(
                                                                                ExprCmd::Sub
                                                                            );
                                                                        }
                                                                        Lex::BAnd => {
                                                                            buf.gen(
                                                                                ExprCmd::And
                                                                            );
                                                                        }
                                                                        Lex::BOr => {
                                                                            buf.gen(
                                                                                ExprCmd::Or
                                                                            );
                                                                        }
                                                                        Lex::Xor => {
                                                                            buf.gen(
                                                                                ExprCmd::Xor
                                                                            );
                                                                        }
                                                                        Lex::LS => {
                                                                            buf.gen(
                                                                                ExprCmd::LS
                                                                            );
                                                                        }
                                                                        Lex::RS => {
                                                                            buf.gen(
                                                                                ExprCmd::RS
                                                                            );
                                                                        }
                                                                        Lex::ROL => {
                                                                            buf.gen(
                                                                                ExprCmd::ROL
                                                                            );
                                                                        }
                                                                        Lex::ROR => {
                                                                            buf.gen(
                                                                                ExprCmd::ROR
                                                                            );
                                                                        }
                                                                        _ => {
                                                                            //dummy cond
                                                                        }
                                                                    }
                                                                } else {
                                                                    self.mismatched();
                                                                    return Type::None;
                                                                }
                                                            }
                                                            _ => {
                                                                self.mismatched();
                                                                return Type::None;
                                                            }
                                                        }
                                                    },
                                                    _ => {
                                                        break;
                                                    }
                                                }
                                            }
                                            ct
                                        } else {
                                            self.mismatched();
                                            Type::None
                                        }
                                    }
                                    _ => {
                                        self.mismatched();
                                        Type::None
                                    }
                                }
                            }
                            _ => {
                                //FOR CODEGEN
                                typ
                            }
                        }
                    }
                    Type::Bool => {
                        let op = self.lex_analyzer.get_lex();
                        match op {
                            Lex::LAnd | Lex::LOr | Lex::Xor => {
                                self.lex_analyzer.next_lex();
                                if self.term(buf) == Type::Bool {
                                    match op {
                                        Lex::LOr => { buf.gen(ExprCmd::Or); }
                                        Lex::LAnd => { buf.gen(ExprCmd::And); }
                                        Lex::Xor => { buf.gen(ExprCmd::Xor); }
                                        _ => {
                                            //dummy cond
                                        }
                                    }
                                    let mut iter_op = self.lex_analyzer.get_lex();
                                    while iter_op == Lex::LOr ||
                                        iter_op == Lex::LAnd ||
                                        iter_op == Lex::Xor {
                                        self.lex_analyzer.next_lex();
                                        if self.term(buf) == Type::Bool {
                                            match iter_op {
                                                Lex::LOr => { buf.gen(ExprCmd::Or); }
                                                Lex::LAnd => { buf.gen(ExprCmd::And); }
                                                Lex::Xor => { buf.gen(ExprCmd::Xor); }
                                                _ => {
                                                    //dummy cond
                                                }
                                            }
                                        } else {
                                            self.mismatched();
                                            return Type::None;
                                        }
                                        iter_op = self.lex_analyzer.get_lex();
                                    }
                                    buf.apply(&mut self.codegen, &Type::Bool);
                                    buf.clear();
                                    Type::Bool
                                } else {
                                    self.mismatched();
                                    Type::None
                                }
                            }
                            _ => {
                                Type::Bool
                            }
                        }
                    }
                    _ => {
                        self.mismatched();
                        Type::None
                    }
                }
            }
        }
    }

    fn term(&mut self, buf: &mut ExprGenBuf) -> Type {
        match self.lex_analyzer.get_lex() {
            Lex::Name(name) => {
                let ent = self.table.find(
                    name,
                    Some(self.lex_analyzer.get_glob_pos())
                );
                match ent.borrow().cat {
                    Cat::Konst | Cat::Var | Cat::StdProc => {
                        match ent.borrow().typ {
                            Type::S8 | Type::E8 | Type::S16 | Type::E16 | Type::Bool => {
                                match ent.borrow().cat {
                                    Cat::Konst => {
                                        buf.gen(ExprCmd::LdConst(ent.borrow().val));
                                    }
                                    Cat::Var => {
                                        buf.gen(ExprCmd::LdVar(ent.borrow().val));
                                    }
                                    Cat::StdProc => {
                                        self.lex_analyzer.next_lex();

                                        self.prep_fn(ent.borrow().val);

                                        buf.gen(ExprCmd::SP(ent.borrow().val));

                                        return ent.borrow().typ.clone();
                                    }
                                    _ => {
                                        //Dummy cond
                                    }
                                }
                                self.lex_analyzer.next_lex();
                                return ent.borrow().typ.clone();
                            }
                            _ => {
                                if ent.borrow().cat == Cat::StdProc {
                                    self.mismatched();
                                    return Type::None;
                                }
                            }
                        }
                    }
                    _ => {}
                };
            }
            Lex::Num(num) => {
                buf.gen(ExprCmd::LdConst(num));
                self.lex_analyzer.next_lex();
                return Type::Num(TypeRange::new(num, num));
            }
            Lex::Malv | Lex::Vera => {
                buf.gen(ExprCmd::LdConst(
                    if self.lex_analyzer.get_lex() == Lex::Vera { 1 } else { 0 }
                ));
                self.lex_analyzer.next_lex();
                return Type::Bool;
            }
            Lex::LPar => {
                self.lex_analyzer.next_lex();
                let res = self.expression(buf);
                self.check(Lex::RPar, Some(String::from("[')']")));
                return res;
            }
            _ => {}
        }
        ErrorsHandler::error_0010(
            None,
            None,
            self.lex_analyzer.get_glob_pos()
        );
        self.lex_analyzer.next_lex();
        return Type::None;
    }

    //

    fn lig_prod(&mut self) { //lig production
        //DUMMY CHECK
        self.check(Lex::Lig, Some(String::from("['lig']")));
        if let Lex::Name(name) = self.lex_analyzer.get_lex() {
            let ent_ref = self.table.new_name(name, Cat::Bar);
            if let Some(ent) = ent_ref {
                let mut decl_typ: Option<Type> = None;
                if self.lex_analyzer.get_next_lex() == Lex::Colon {
                    if let Lex::Name(name) = self.lex_analyzer.get_next_lex() {
                        let typ_ent = self.table.find(
                            name.clone(),
                            Some(self.lex_analyzer.get_glob_pos()),
                        );
                        if typ_ent.borrow().cat != Cat::Tipo {
                            ErrorsHandler::error_0101(
                                name,
                                self.lex_analyzer.get_glob_pos(),
                            );
                        }
                        decl_typ = Some(typ_ent.borrow().typ.clone());
                        self.lex_analyzer.next_lex();
                    } else {
                        ErrorsHandler::error_0010(
                            Some(String::from("[nomo]")),
                            None,
                            self.lex_analyzer.get_glob_pos(),
                        );
                    }
                }

                self.check(Lex::Ass, Some(String::from("['=']")));
                let mut expr_gen_buf = ExprGenBuf::new();
                let typ = self.expression(&mut expr_gen_buf);

                //Compute result type
                let mut res_typ = Type::None;
                if let Some(decl_typ) = decl_typ {
                    if let Some(res) = Type::get_common_type(typ, decl_typ) {
                        res_typ = res;
                    } else {
                        self.mismatched();
                    }
                } else {
                    match typ {
                        Type::Num(rng) => {
                            if let Some(res) = rng.get_min_type() { res_typ = res; } else {
                                self.mismatched();
                            }
                        }
                        Type::S8 | Type::E8 | Type::S16 | Type::E16 | Type::Bool => {
                            res_typ = typ;
                        }
                        _ => { self.mismatched(); }
                    }
                }
                //

                if !expr_gen_buf.is_empty() {
                    expr_gen_buf.apply(&mut self.codegen, &res_typ);
                    expr_gen_buf.clear(); //dummy clear
                }

                let res_typ_sz = &TypeSize::from(&res_typ);
                let ptr = self.codegen.gen_form_var(res_typ_sz);

                let mut ent_mut = ent.borrow_mut();
                ent_mut.typ = res_typ;
                ent_mut.cat = Cat::Var;
                ent_mut.val = ptr;
            } else { ErrorsHandler::dev_error_0610(); }
        } else {
            ErrorsHandler::error_0010(
                Some(String::from("[nomo]")),
                None,
                self.lex_analyzer.get_glob_pos(),
            );
        }
        self.check(Lex::Semi, Some(String::from("[';']")));
    }

    fn prep_fn(&mut self, code: i32) {
        self.check(Lex::LPar, Some(String::from("['(']")));

        let decl_typ = ImportGenBuf::get_fn_arg_type(code);

        let mut arg_typ = Type::None;

        if self.lex_analyzer.get_lex() != Lex::RPar {
            let mut buf = ExprGenBuf::new();
            arg_typ = self.expression(&mut buf);
            if let Some(res_typ) =
                Type::get_common_type(decl_typ.clone(), arg_typ.clone()) {
                arg_typ = res_typ.clone();
                buf.apply(&mut self.codegen, &res_typ); //dummy res_typ notation
                buf.clear(); //dummy clear
            } else {
                self.mismatched();
                return; //Dummy return
            }
        } else if arg_typ != decl_typ {
            self.mismatched();
            return;
        }

        self.check(Lex::RPar, Some(String::from("[')']")));
    }

    fn name_proc(&mut self) {
        if let Lex::Name(mut name) = self.lex_analyzer.get_lex() {

            let ent_ref = self.table.find(
                name.clone(),
                Some(self.lex_analyzer.get_glob_pos()),
            );

            self.lex_analyzer.next_lex();

            match ent_ref.borrow().cat {
                Cat::Var => {
                    self.check(Lex::Ass, Some(String::from("['=']")));
                    let mut buf = ExprGenBuf::new();
                    let typ = self.expression(&mut buf);

                    //Check result type
                    if let Some(res) = Type::get_common_type(
                        typ,
                        ent_ref.borrow().typ.clone()
                    ) {

                        buf.apply(&mut self.codegen, &res);
                        buf.clear(); //Dummy clear

                        let typ_sz = &TypeSize::from(&res);
                        self.codegen.gen_st_var(typ_sz, ent_ref.borrow().val);

                        self.check(Lex::Semi, Some(String::from("[';']")));
                        return;
                    } else {
                        self.mismatched();
                    }
                }
                Cat::StdProc => {

                    self.prep_fn(ent_ref.borrow().val);

                    self.codegen.gen_rcall(&name);

                    self.check(Lex::Semi, Some(String::from("[';']")));

                    if ent_ref.borrow().typ != Type::None {
                        ErrorsHandler::warning_0710(self.lex_analyzer.get_glob_pos());
                        self.codegen.gen_form_var(&TypeSize::from(&ent_ref.borrow().typ));
                    }

                    return;
                }
                _ => {}
            };
        }
        ErrorsHandler::error_0010(
            Some(String::from("[nomo]")),
            None,
            self.lex_analyzer.get_glob_pos(),
        );
    }

    fn red(&mut self) {
        //Dummy check
        self.check(Lex::Red, Some(String::from("['red']")));
        self.check(Lex::Semi, Some(String::from("[';']")));
        let drop_sz = self.table.compute_fn_stack_drop_sz(
            &if let Some(fn_name) = &self.cur_fn_name {
                fn_name.clone()
            } else {
                ErrorsHandler::error_0711(self.lex_analyzer.get_glob_pos());
                String::new()
            }
        );
        let mut cur = self.table.get_next_bar(self.table.top.clone());
        let mut gen_ret = true;
        while self.lex_analyzer.get_lex() == Lex::RBrac {
            let sc_typ =
                ScopeType::from(Table::get_entry(cur.clone()).borrow().val);
            if sc_typ == ScopeType::Block {
                if self.lex_analyzer.get_next_lex() != Lex::RBrac {
                    ErrorsHandler::warning_0712(self.lex_analyzer.get_glob_pos());
                }
            } else { gen_ret = sc_typ != ScopeType::FN; break; }
            self.lex_analyzer.next_lex();
            cur = self.table.get_next_bar(cur);
        }
        if drop_sz != 0 { self.codegen.gen_drop(drop_sz); }
        if gen_ret { self.codegen.gen_rjmp(&String::from("end")); }
    }

    fn statement(&mut self, sc_typ: &ScopeType) {
        self.check(Lex::LBrac, Some(String::from("['{']")));

        if self.lex_analyzer.get_lex() == Lex::RBrac {
            ErrorsHandler::warning_0810(self.lex_analyzer.get_glob_pos());
        }

        self.table.open_scope(sc_typ);

        loop {
            match self.lex_analyzer.get_lex() {
                Lex::LBrac => { self.statement(&ScopeType::Block); }
                Lex::Lig => { self.lig_prod(); }
                Lex::Name(_) => { self.name_proc(); }
                Lex::Se => { self.se_alie(); }
                Lex::Buk => { self.buk(); }
                Lex::Red => { self.red(); }
                Lex::RBrac => { break; }
                _ => {
                    ErrorsHandler::error_0010(
                        None,
                        None,
                        self.lex_analyzer.get_glob_pos()
                    );
                }
            }
        }

        self.table.close_scope(&mut self.codegen);

        //DUMMY CHECK
        self.check(Lex::RBrac, Some(String::from("['}']")));
    }

    fn se_alie(&mut self) {
        //DUMMY CHECK
        self.check(Lex::Se, Some(String::from("['se']")));

        let malvlbl = &self.codegen.gen_delayed_label();

        let mut buf = ExprGenBuf::new();
        let typ = self.expression(&mut buf);
        if typ != Type::Bool { self.mismatched(); }
        //Dummy application
        if !buf.is_empty() {
            buf.apply(&mut self.codegen, &typ);
            buf.clear();
        }
        //
        ExprGenBuf::gen_brf(&mut self.codegen, malvlbl);
        self.statement(&ScopeType::Se);

        if self.lex_analyzer.get_lex() == Lex::Alie {
            let endlbl = &self.codegen.gen_delayed_label();
            self.codegen.gen_rjmp(endlbl);
            self.codegen.apply_label(malvlbl);
            self.lex_analyzer.next_lex();
            self.statement(&ScopeType::Alie);
            self.codegen.apply_label(endlbl);
        } else { self.codegen.apply_label(malvlbl); }
    }

    fn buk(&mut self) {
        self.check(Lex::Buk, Some(String::from("['buk']")));
        let endlbl = &self.codegen.gen_delayed_label();
        let iterlbl = &self.codegen.gen_label();
        if self.lex_analyzer.get_lex() != Lex::LBrac {
            let mut buf = ExprGenBuf::new();
            let typ = self.expression(&mut buf);
            if typ != Type::Bool { self.mismatched(); }
            //Dummy application
            if !buf.is_empty() {
                buf.apply(&mut self.codegen, &typ);
                buf.clear();
            }
            //
            ExprGenBuf::gen_brf(&mut self.codegen, endlbl);
        }
        self.statement(&ScopeType::Buk);
        self.codegen.gen_rjmp(iterlbl);
        self.codegen.apply_label(endlbl);
    }

    fn funkcio(&mut self) {
        self.check(Lex::FN, Some(String::from("['fn']")));
        if let Lex::Name(fn_name) = self.lex_analyzer.get_lex() {
            if fn_name != String::from("baza") {
                ErrorsHandler::error_0713(self.lex_analyzer.get_glob_pos());
            }
            self.cur_fn_name = Some(fn_name.clone());
            self.table.new_name(fn_name, Cat::Bar);
        } else {
            ErrorsHandler::error_0010(
                Some(String::from("[nomo]")),
                None,
                self.lex_analyzer.get_glob_pos(),
            );
        }
        self.lex_analyzer.next_lex();
        self.check(Lex::LPar, Some(String::from("['(']")));
        self.check(Lex::RPar, Some(String::from("[')']")));
        self.codegen.apply_label(&String::from("baza"));
        self.statement(&ScopeType::FN);
        self.codegen.gen_rjmp(&String::from("end"));
        //self.codegen.gen_ret();
    }

    pub(crate) fn parse(&mut self) {
        self.uzantej();
        self.deklaracioj();
        if self.lex_analyzer.get_lex() == Lex::FN {
            self.funkcio();
        } else {
            ErrorsHandler::error_0010(
                Some(String::from("['fn']")),
                None,
                self.lex_analyzer.get_glob_pos()
            );
        }
        self.check(Lex::EOF, Some(String::from("[FDD]")));
    }
}