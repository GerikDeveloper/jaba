use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use lazy_static::lazy_static;
use crate::jaba::codegen::{CodeGen, ImportGenBuf};
use crate::jaba::errors_handler::ErrorsHandler;
use crate::jaba::src_driver::GlobPos;

pub(crate) type EntryRef = Option<Rc<RefCell<Entry>>>;

#[derive(PartialEq)]
pub(crate) enum Cat {
    Konst,
    Var,
    Tipo,
    Fn,
    Bar,
    Mod,
}

#[derive(PartialEq, Clone, Hash, Eq, Debug)]
pub(crate) enum Type {
    None,
    Num(TypeRange),
    S8,
    E8,
    S16,
    E16,
    Bool,
}

impl Type {
    pub(crate) fn get_common_type(a: Type, b: Type) -> Option<Type> {
        return if let Type::Num(mut rng) = a {
            if let Type::Num(arg_rng) = b {
                rng.combine(arg_rng);
                Some(Type::Num(rng))
            } else if rng.compatible(TypeRange::from(b.clone())) {
                Some(b)
            } else {
                None
            }
        } else if let Type::Num(arg_rng) = b {
            if arg_rng.compatible(TypeRange::from(a.clone())) {
                Some(a)
            } else {
                None
            }
        } else {
            if a == b { Some(a.clone()) } else {
                None
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub(crate) enum TypeSize {
    Reg,
    Word,
}

impl TypeSize {
    pub(crate) fn from(typ: &Type) -> Self {
        return match typ {
            Type::None => {
                ErrorsHandler::dev_error_0102();
                TypeSize::Reg
            }
            Type::Num(_) => {
                ErrorsHandler::dev_error_0102();
                TypeSize::Reg
            }
            Type::S8 => { TypeSize::Reg }
            Type::E8 => { TypeSize::Reg }
            Type::S16 => { TypeSize::Word }
            Type::E16 => { TypeSize::Word }
            Type::Bool => { TypeSize::Reg }
        }
    }
}

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
pub(crate) struct TypeRange {
    pub(crate) min: i32,
    pub(crate) max: i32,
}

lazy_static! {
    static ref TYPE_RANGE_TABLE: HashMap<Type, TypeRange> = HashMap::from([
        (Type::S8, TypeRange { min: 0, max: 255,}),
        (Type::E8, TypeRange { min: -128, max: 127, }),
        (Type::S16, TypeRange { min: 0, max: 65535, }),
        (Type::E16, TypeRange { min: -32768, max: 32767, }),
        (Type::Bool, TypeRange { min: 0, max: 1, }),
    ]);
}



impl TypeRange {
    pub fn new(min: i32, max: i32) -> Self {
        return Self { min, max };
    }

    pub fn from(typ: Type) -> Self {
        let mut res = Self::new(0, 0);
        if let Some(range) = TYPE_RANGE_TABLE.get(&typ) { res = range.clone(); }
        else { ErrorsHandler::dev_error_0611(); }
        return res;
    }

    pub(crate) fn compatible(&self, with: Self) -> bool {
        return (with.max >= self.max) && (with.min <= self.min);
    }

    pub(crate) fn combine(&mut self, with: Self) {
        if with.max > self.max { self.max = with.max; }
        if with.min < self.min { self.min = with.min; }
    }

    pub(crate) fn get_min_type(&self) -> Option<Type> {
        return if self.min < 0 {
            if self.compatible(Self::from(Type::E8)) { Some(Type::E8) } else if self.compatible(Self::from(Type::E16)) { Some(Type::E16) } else { None }
        } else {
            if self.compatible(Self::from(Type::S8)) { Some(Type::S8) } else if self.compatible(Self::from(Type::S16)) { Some(Type::S16) } else { None }
        }
    }
}

#[derive(PartialEq)]
pub(crate) struct Entry {
    pub(crate) name: String,
    pub(crate) cat: Cat,
    pub(crate) typ: Type,
    pub(crate) val: i32,
    pub(crate) prev: EntryRef,
}

impl Entry {
    fn default() -> Self {
        return Self {
            name: String::new(),
            cat: Cat::Bar,
            typ: Type::None,
            val: 0,
            prev: None,
        };
    }
}

//Scope types
const MODULE_SCOPE: i32 = 0;
const BLOCK_SCOPE: i32 = 1;
const FN_SCOPE: i32 = 2;
const SE_SCOPE: i32 = 3;
const ALIE_SCOPE: i32 = 4;
const BUK_SCOPE: i32 = 5;
//

#[derive(PartialEq)]
pub(crate) enum ScopeType {
    Module,
    Block,
    FN,
    Se,
    Alie,
    Buk,
}

impl ScopeType {
    pub(crate) fn from(code: i32) -> Self {
        return match code {
            MODULE_SCOPE => ScopeType::Module,
            BLOCK_SCOPE => ScopeType::Block,
            FN_SCOPE => ScopeType::FN,
            SE_SCOPE => ScopeType::Se,
            ALIE_SCOPE => ScopeType::Alie,
            BUK_SCOPE => ScopeType::Buk,
            _ => {
                ErrorsHandler::dev_error_0612();
                ScopeType::Block
            }
        };
    }

    pub(crate) fn to_code(&self) -> i32 {
        return match self {
            ScopeType::Module => { MODULE_SCOPE }
            ScopeType::Block => { BLOCK_SCOPE }
            ScopeType::FN => { FN_SCOPE }
            ScopeType::Se => { SE_SCOPE }
            ScopeType::Alie => { ALIE_SCOPE }
            ScopeType::Buk => { BUK_SCOPE }
        };
    }
}

#[derive(Clone)]
pub(crate) struct FnData {
    name: String,
    is_impl: bool,
    is_used: bool,
    pub(crate) arg_typ: Type,
}

impl FnData {
    fn new(name: String, arg_typ: Type) -> Self {
        return Self {
            name,
            is_impl: false,
            is_used: false,
            arg_typ,
        };
    }

    fn default() -> Self {
        return Self::new(String::new(), Type::None);
    }
}

#[derive(Clone)]
pub(crate) struct Table {
    fns: Vec<FnData>,
    pub(crate) top: EntryRef,
    bottom: EntryRef,
}

impl Table {
    pub(crate) fn new() -> Self {
        return Self {
            fns: Vec::new(),
            top: None,
            bottom: None,
        };
    }

    pub(crate) fn enter(&mut self, name: String, cat: Cat, typ: Type, val: i32) {
        self.top = Some(
            Rc::new(RefCell::new(
                Entry {
                    name,
                    cat,
                    typ,
                    val,
                    prev: self.top.clone(),
                }
            ))
        );
    }

    //arg typ, res typ
    pub(crate) fn use_fn(&mut self, name: String, glob_pos: GlobPos) -> (Type, Type) {
        let ent = self.find(name, Some(glob_pos));
        return (
            if let Some(fn_data) = self.fns.get_mut(ent.borrow().val as usize) {
                fn_data.is_used = true;
                fn_data.arg_typ.clone()
            } else { ErrorsHandler::dev_error_0720(); Type::None },
            ent.borrow().typ.clone(),
        );
    }

    pub(crate) fn get_fn_data(&mut self, name: String) -> Option<FnData> {
        let fn_id = if let Some(ent) = self.find_some(name.clone()) {
            if ent.borrow().cat != Cat::Fn { return None; }; ent.borrow().val
        } else { return None; };
        return if let Some(fn_data) = self.fns.get(fn_id as usize) {
            Some(fn_data.clone())
        } else { None }
    }

    pub(crate) fn decl_fn(&mut self, name: String, arg_typ: Type, res_typ: Type) -> EntryRef {
        let val = self.fns.len();
        self.fns.push(FnData::new(name.clone(), arg_typ));
        let res = self.new_name(
            name,
            Cat::Fn,
        );

        let mut ent = Self::get_entry(res.clone());
        ent.borrow_mut().typ = res_typ;
        ent.borrow_mut().val = val as i32;
        return res;
    }

    pub(crate) fn impl_fn(
        &mut self, name: String, arg_typ: Type, res_typ: Type, glob_pos: GlobPos,
    ) {
        let fn_id = if let Some(decl) = self.find_some(name.clone()) {
            if decl.borrow().cat != Cat::Fn {
                ErrorsHandler::error_0717(Some(glob_pos.clone()));
            }
            if decl.borrow().typ != res_typ { ErrorsHandler::error_0718(glob_pos.clone()); }
            if let Some(fn_data) = self.fns.get(decl.borrow().val as usize) {
                if fn_data.is_impl { ErrorsHandler::error_0721(glob_pos.clone()); }
                if fn_data.arg_typ != arg_typ { ErrorsHandler::error_0719(glob_pos); }
            } else { ErrorsHandler::dev_error_0720(); }
            decl.borrow().val
        } else {
            Self::get_entry(self.decl_fn(name, arg_typ, res_typ)).borrow().val
        };
        if let Some(fn_data) = self.fns.get_mut(fn_id as usize) {
            fn_data.is_impl = true;
        } else { ErrorsHandler::dev_error_0720(); }
    }

    pub(crate) fn check_fns(&mut self) {
        for fun in &self.fns {
            if fun.is_used && !fun.is_impl { ErrorsHandler::error_0722(fun.name.clone()); }
            if !fun.is_used { ErrorsHandler::warning_0723(fun.name.clone()); }
        }
    }

    pub(crate) fn new_name(&mut self, name: String, cat: Cat) -> EntryRef {
        let mut ent = self.get_top();
        while ent.borrow().cat != Cat::Bar && ent.borrow().name != name {
            let prev = Self::get_entry(ent.borrow().prev.clone());
            ent = prev;
        }
        if ent.borrow().cat == Cat::Bar {
            self.enter(name, cat, Type::None, 0);
        } else {
            ErrorsHandler::error_0614();
        }
        return self.top.clone();
    }

    pub(crate) fn open_scope(&mut self, sc_typ: &ScopeType) {
        self.enter(String::new(), Cat::Bar, Type::None, sc_typ.to_code());
        if self.get_top().borrow().prev.is_none() { self.bottom = self.top.clone(); }
    }

    pub(crate) fn get_entry(ent_ref: EntryRef) -> Rc<RefCell<Entry>> {
        return if let Some(ent) = ent_ref { ent } else {
            ErrorsHandler::dev_error_0615();
            //DummyReturn
            Rc::new(RefCell::new(Entry::default()))
        }
    }

    fn get_top(&self) -> Rc<RefCell<Entry>> {
        return Self::get_entry(self.top.clone());
    }

    fn get_bottom(&self) -> Rc<RefCell<Entry>> {
        return Self::get_entry(self.bottom.clone());
    }



    pub(crate) fn close_scope(&mut self, codegen: &mut CodeGen) {
        let mut drop_sz: i32 = 0;
        loop {
            let mut break_flag = false;
            match self.get_top().borrow().cat {
                Cat::Var => {
                    drop_sz += match TypeSize::from(&self.get_top().borrow().typ) {
                        TypeSize::Reg => { 1 }
                        TypeSize::Word => { 2 }
                    };
                }
                Cat::Bar => { break_flag = true; }
                _ => {}
            }
            self.top = self.get_top().borrow().prev.clone();
            if break_flag { break; }
        }
        if drop_sz != 0 { codegen.gen_drop(drop_sz); }
    }

    pub(crate) fn get_next_bar(&self, begin: EntryRef) -> EntryRef {
        let mut cur = begin;
        loop {
            let ent = Self::get_entry(cur.clone());
            let bent = ent.borrow();
            if bent.cat == Cat::Bar { break; } else { cur = bent.prev.clone(); }
        }
        return cur;
    }

    pub(crate) fn drop_fn_stack(&mut self, codegen: &mut CodeGen, fn_name: &String) {
        let ent = self.find(fn_name.clone(), None);
        if ent.borrow().cat != Cat::Fn { ErrorsHandler::error_0717(None); }
        let res_typ = &ent.borrow().typ;
        let arg_typ = &if let Some(fn_data) =
            self.get_fn_data(fn_name.clone()) { fn_data.arg_typ } else {
            ErrorsHandler::dev_error_0720();
            Type::None
        };
        let mut shift = 0;
        if *res_typ != Type::None {
            shift -= match TypeSize::from(res_typ) {
                TypeSize::Reg => { 1 }
                TypeSize::Word => { 2 }
            };
        }
        if *arg_typ != Type::None {
            shift -= match TypeSize::from(arg_typ) {
                TypeSize::Reg => { 1 }
                TypeSize::Word => { 2 }
            };
        }
        shift -= 2; //ret addr
        codegen.gen_set_sp_rel_bp(shift);
    }

    pub(crate) fn find_some(&mut self, name: String) -> Option<Rc<RefCell<Entry>>> {
        {
            self.get_bottom().borrow_mut().name = name.clone();
        }
        let mut ent = self.get_top();
        while ent.borrow().name != name {
            let prev = ent.borrow().prev.clone();
            ent = Self::get_entry(prev);
        }
        if Rc::ptr_eq(&ent, &self.get_bottom()) {
            return None;
        }
        return Some(ent);
    }

    pub(crate) fn find(&mut self, name: String, glob_pos: Option<GlobPos>) -> Rc<RefCell<Entry>> {
        return if let Some(res) = self.find_some(name) { res } else {
            ErrorsHandler::error_0616(glob_pos);
            Rc::new(RefCell::new(Entry::default()))
        }
    }
}