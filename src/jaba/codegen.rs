use std::collections::{HashMap, HashSet};
use std::path::Path;
use lazy_static::lazy_static;
use phf::phf_map;
use tokio::fs::{create_dir_all, File, OpenOptions};
use tokio::io::AsyncWriteExt;
use crate::jaba::errors_handler::ErrorsHandler;
use crate::jaba::parser::SPCode;
use crate::jaba::table::{Type, TypeSize};

pub(crate) enum ExprCmd {
    Cpy,
    Swap,
    FN(String, Type, Type),
    LdVar(i32),
    LdConst(i32),
    Neg,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Not,
    LS,
    RS,
    ROL,
    ROR,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
}

pub(crate) struct ExprGenBuf {
    cmds: Vec<ExprCmd>,
}

impl ExprGenBuf {
    pub(crate) fn new() -> Self { return Self { cmds: Vec::new(), }; }
    //single operator
    fn gen_sn_op(
        codegen: &mut CodeGen,
        typ_sz: &TypeSize,
        low_reg_cmd: impl Fn(&mut CodeGen, &str),
        high_reg_cmd: impl Fn(&mut CodeGen, &str),
        rev: bool,
    ) {
        match typ_sz {
            TypeSize::Reg => {
                codegen.gen_pop("r16");
                low_reg_cmd(codegen, "r16");
                codegen.gen_push("r16");
            }
            TypeSize::Word => {
                codegen.gen_pop("r16"); //0 - LOW
                codegen.gen_pop("r17"); //0 - HIGH

                low_reg_cmd(codegen, if rev { "r17" } else { "r16" });
                high_reg_cmd(codegen, if rev { "r16" } else { "r17" });

                codegen.gen_push("r17");
                codegen.gen_push("r16");
            }
        }
    }
    //

    //double operator
    fn gen_db_op(
        codegen: &mut CodeGen,
        typ_sz: &TypeSize,
        low_reg_cmd: impl Fn(&mut CodeGen, &str, &str), //first reg, second reg
        high_reg_cmd: impl Fn(&mut CodeGen, &str, &str), //first reg, second reg
    ) {
        match typ_sz {
            TypeSize::Reg => {
                codegen.gen_pop("r17");
                codegen.gen_pop("r16");
                low_reg_cmd(codegen, "r16", "r17");
                codegen.gen_push("r16");
            }
            TypeSize::Word => {
                codegen.gen_pop("r18"); //1 - LOW
                codegen.gen_pop("r19"); //1 - HIGH
                codegen.gen_pop("r16"); //0 - LOW
                codegen.gen_pop("r17"); //0 - HIGH
                low_reg_cmd(codegen, "r16", "r18");
                high_reg_cmd(codegen, "r17", "r19");
                codegen.gen_push("r17");
                codegen.gen_push("r16");
            }
        }
    }
    //

    fn gen_cpy(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        match typ_sz {
            TypeSize::Reg => {
                codegen.gen_pop("r16");
                codegen.gen_push("r16");
                codegen.gen_push("r16");
            }
            TypeSize::Word => {
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");

                codegen.gen_push("r17");
                codegen.gen_push("r16");

                codegen.gen_push("r17");
                codegen.gen_push("r16");
            }
        }
    }

    fn gen_swap(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        match typ_sz {
            TypeSize::Reg => {
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");
                codegen.gen_push("r16");
                codegen.gen_push("r17");
            }
            TypeSize::Word => {
                //pop 0
                codegen.gen_pop("r16"); //0 - LOW
                codegen.gen_pop("r17"); //0 - HIGH
                //

                //pop 1
                codegen.gen_pop("r18"); //1 - LOW
                codegen.gen_pop("r19"); //1 - HIGH
                //

                //push 0
                codegen.gen_push("r17");
                codegen.gen_push("r16");
                //

                //push 1
                codegen.gen_push("r19");
                codegen.gen_push("r18");
                //
            }
        }
    }

    fn gen_fn(codegen: &mut CodeGen, name: &String, arg_typ: &Type, res_typ: &Type) {
        //prep bp
        if *arg_typ != Type::None {
            codegen.gen_pop("r16"); //r16 - LOW arg
            if TypeSize::from(&arg_typ) == TypeSize::Word {
                codegen.gen_pop("r17"); //r17 - HIGH arg
            }
        }
        codegen.gen_st_bp();
        codegen.gen_sp_to_bp();
        //reserved res mem
        if *res_typ != Type::None {
            codegen.gen_clr("r18");
            if TypeSize::from(&res_typ) == TypeSize::Word {
                codegen.gen_push("r18"); //r18 - HIGH res
            }
            codegen.gen_push("r18"); //r18 - LOW res
        }
        //
        if *arg_typ != Type::None {
            if TypeSize::from(&arg_typ) == TypeSize::Word {
                codegen.gen_push("r17"); //r17 - HIGH arg
            }
            codegen.gen_push("r16"); //r16 - LOW arg
        }
        //

        codegen.gen_rcall(name);

        if *arg_typ != Type::None {
            codegen.gen_pop("r18"); //r18 - LOW res
            if TypeSize::from(&arg_typ) == TypeSize::Word {
                codegen.gen_pop("r18"); //r18 - HIGH res
            }
        }

        if *res_typ != Type::None {
            codegen.gen_pop("r16"); //r16 - LOW res
            if TypeSize::from(&res_typ) == TypeSize::Word {
                codegen.gen_pop("r17"); //r17 - HIGH res
            }
        }

        codegen.gen_ld_bp();

        if *res_typ != Type::None {
            if TypeSize::from(&res_typ) == TypeSize::Word {
                codegen.gen_push("r17"); //r17 - HIGH res
            }
            codegen.gen_push("r16"); //r16 - LOW res
        }
    }

    fn gen_ld_var(codegen: &mut CodeGen, typ_sz: &TypeSize, ptr: i32) {
        if *typ_sz == TypeSize::Word {
            let hptr = ptr + 1;
            if hptr < 0 || hptr > 63 {
                codegen.gen_ldi("XL", CodeGen::low(hptr));
                codegen.gen_ldi("XH", CodeGen::high(hptr));
                codegen.gen_add("XL", "YL");
                codegen.gen_adc("XH", "YH");
                codegen.gen_ld("r16", "X");
            } else { codegen.gen_ldd("r16", "Y", hptr as u8); }
            codegen.gen_push("r16");
        }
        if ptr < 0 || ptr > 63 {
            codegen.gen_ldi("XL", CodeGen::low(ptr));
            codegen.gen_ldi("XH", CodeGen::high(ptr));
            codegen.gen_add("XL", "YL");
            codegen.gen_adc("XH", "YH");
            codegen.gen_ld("r16", "X");
        } else { codegen.gen_ldd("r16", "Y", ptr as u8); }
        codegen.gen_push("r16");
    }

    fn gen_ld_const(codegen: &mut CodeGen, typ_sz: &TypeSize, val: i32) {
        if *typ_sz == TypeSize::Word {
            codegen.gen_ldi("r16", CodeGen::high(val));
            codegen.gen_push("r16");
        }
        codegen.gen_ldi("r16", CodeGen::low(val));
        codegen.gen_push("r16");
    }

    fn gen_neg(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_ld_const(codegen, typ_sz, 0);
        Self::gen_swap(codegen, typ_sz);
        Self::gen_sub(codegen, typ_sz);
    }

    fn gen_add(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_db_op(
            codegen,
            typ_sz,
            CodeGen::gen_add,
            CodeGen::gen_adc,
        );
    }

    fn gen_sub(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_db_op(
            codegen,
            typ_sz,
            CodeGen::gen_sub,
            CodeGen::gen_sbc,
        );
    }

    fn gen_and(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_db_op(
            codegen,
            typ_sz,
            CodeGen::gen_and,
            CodeGen::gen_and,
        );
    }

    fn gen_or(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_db_op(
            codegen,
            typ_sz,
            CodeGen::gen_or,
            CodeGen::gen_or,
        );
    }

    fn gen_xor(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_db_op(
            codegen,
            typ_sz,
            CodeGen::gen_eor,
            CodeGen::gen_eor,
        );
    }

    fn gen_not(codegen: &mut CodeGen, typ: &Type) {
        let typ_sz = &TypeSize::from(typ);
        //bitwise
        Self::gen_sn_op(
            codegen,
            typ_sz,
            CodeGen::gen_com,
            CodeGen::gen_com,
            false,
        );
        //
        if *typ == Type::Bool {
            //logic
            Self::gen_ld_const(codegen, typ_sz, 1);
            Self::gen_and(codegen, typ_sz);
            //
        }
    }

    //repeatable double operation
    fn gen_rep_db_op(
        codegen: &mut CodeGen,
        typ_sz: &TypeSize,
        low_reg_cmd: impl Fn(&mut CodeGen, &str),
        high_reg_cmd: impl Fn(&mut CodeGen, &str),
        rev: bool,
    ) {
        match typ_sz {
            TypeSize::Reg => {
                //(r17, r16, ...) r17 - cnt, r16 - operated
                codegen.gen_pop("r17");
                codegen.gen_pop("r16");

                let endlbl = &codegen.gen_delayed_label();

                let startlbl = &codegen.gen_label();
                codegen.gen_cpi("r17", 0);

                codegen.gen_breq(endlbl);

                //iter
                low_reg_cmd(codegen, "r16");
                //

                codegen.gen_dec("r17");

                codegen.gen_rjmp(startlbl);

                codegen.apply_label(endlbl);

                codegen.gen_push("r16");
            }
            TypeSize::Word => {
                //(XL, XH, r16, r17, ...);
                //(XL, XH) - cnt,
                //(r16 - LOW, r17 - HIGH) - operated
                codegen.gen_pop("XL");
                codegen.gen_pop("XH");
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");
                codegen.gen_clr("r18");

                let endlbl = &codegen.gen_delayed_label();

                let iterlbl = &codegen.gen_label();

                codegen.gen_cp("XL", "r18");
                codegen.gen_cpc("XH", "r18");
                codegen.gen_breq(endlbl);

                //iter
                low_reg_cmd(codegen, if rev { "r17" } else { "r16" });
                high_reg_cmd(codegen, if rev { "r16" } else { "r17" });
                //

                codegen.gen_sbiw("XH:XL", 1);

                codegen.gen_rjmp(iterlbl);
                codegen.apply_label(endlbl);

                codegen.gen_push("r17");
                codegen.gen_push("r16");
            }
        }
    }
    //

    fn gen_shift(codegen: &mut CodeGen, typ: &Type, left: bool) {
        let pos_low_cmd = if left { CodeGen::gen_lsl } else { CodeGen::gen_lsr };
        let pos_high_cmd = if left { CodeGen::gen_rol } else { CodeGen::gen_ror };
        let neg_low_cmd = if left { CodeGen::gen_lsr } else { CodeGen::gen_lsl };
        let neg_high_cmd = if left { CodeGen::gen_ror } else { CodeGen::gen_rol };

        let typ_sz = &TypeSize::from(typ);

        match typ_sz {
            TypeSize::Reg => {
                //r17 - shift
                //r16 - shiftable

                let endlbl = &codegen.gen_delayed_label();
                let startlbl = &codegen.gen_delayed_label();

                Self::gen_cpy(codegen, typ_sz);
                Self::gen_ld_const(codegen, typ_sz, 0);
                Self::gen_eq(codegen, typ_sz);
                Self::gen_brf(codegen, startlbl);
                //0 part
                codegen.gen_pop("r17");
                codegen.gen_pop("r16");
                codegen.gen_rjmp(endlbl);
                //
                codegen.apply_label(startlbl);
                if *typ == Type::E8 {
                    //r17 - shift
                    //r16 - shiftable

                    let neglbl = &codegen.gen_delayed_label();

                    Self::gen_cpy(codegen, typ_sz);
                    Self::gen_ld_const(codegen, typ_sz, 0);
                    Self::gen_ge(codegen, typ);
                    Self::gen_brf(codegen, neglbl);

                    //>0 part
                    Self::gen_rep_db_op(
                        codegen,
                        typ_sz,
                        pos_low_cmd,
                        pos_high_cmd,
                        !left,
                    );
                    codegen.gen_rjmp(endlbl);
                    //

                    //<0 part
                    codegen.apply_label(neglbl);
                    Self::gen_neg(codegen, typ_sz);
                    Self::gen_rep_db_op(
                        codegen,
                        typ_sz,
                        neg_low_cmd,
                        neg_high_cmd,
                        left,
                    );
                    //
                } else {
                    Self::gen_rep_db_op(
                        codegen,
                        typ_sz,
                        pos_low_cmd,
                        pos_high_cmd,
                        !left,
                    );
                }
                codegen.apply_label(endlbl);
            }
            TypeSize::Word => {
                //(XL, XH, r16, r17, ...);
                //(XL, XH) - shift,
                //(r16 - LOW, r17 - HIGH) - shiftable

                let endlbl = &codegen.gen_delayed_label();
                let startlbl = &codegen.gen_delayed_label();

                Self::gen_cpy(codegen, typ_sz);
                Self::gen_ld_const(codegen, typ_sz, 0);
                Self::gen_eq(codegen, typ_sz);
                Self::gen_brf(codegen, startlbl);

                //0 part
                codegen.gen_pop("XL");
                codegen.gen_pop("XH");
                codegen.gen_rjmp(endlbl);
                //

                codegen.apply_label(startlbl);
                if *typ == Type::E16 {
                    let neglbl = &codegen.gen_delayed_label();

                    Self::gen_cpy(codegen, typ_sz);
                    Self::gen_ld_const(codegen, typ_sz, 0);
                    Self::gen_ge(codegen, typ);
                    Self::gen_brf(codegen, neglbl);

                    //>0 part
                    Self::gen_rep_db_op(
                        codegen,
                        typ_sz,
                        pos_low_cmd,
                        pos_high_cmd,
                        !left,
                    );
                    codegen.gen_rjmp(endlbl);
                    //

                    //<0 part
                    codegen.apply_label(neglbl);
                    Self::gen_neg(codegen, typ_sz);
                    Self::gen_rep_db_op(
                        codegen,
                        typ_sz,
                        neg_low_cmd,
                        neg_high_cmd,
                        left,
                    );
                    //
                } else {
                    Self::gen_rep_db_op(
                        codegen,
                        typ_sz,
                        pos_low_cmd,
                        pos_high_cmd,
                        !left,
                    );
                }
                codegen.apply_label(endlbl);
            }
        }
    }

    fn gen_ls(codegen: &mut CodeGen, typ: &Type) {
        Self::gen_shift(codegen, typ, true);
    }

    fn gen_rs(codegen: &mut CodeGen, typ: &Type) {
        Self::gen_shift(codegen, typ, false);
    }

    //sized (<=8 for reg and <=16 for word) (not negative)
    fn gen_sz_rot(codegen: &mut CodeGen, typ: &Type, left: bool) {
        let typ_sz = &TypeSize::from(typ);
        match typ_sz {
            TypeSize::Reg => {
                //(r17, r16, ...) r17 - cnt, r16 - operated

                codegen.gen_pop("r17");
                codegen.gen_pop("r16");

                codegen.gen_push("r16");
                codegen.gen_push("r17");

                codegen.gen_push("r16");
                codegen.gen_push("r17");

                //(r17, r16, r17, r16, ...)

                if left { Self::gen_ls(codegen, typ); }
                else { Self::gen_rs(codegen, typ); }

                //(c0, r17, r16, ...) //c0 - computed0

                codegen.gen_pop("r18");
                codegen.gen_pop("r17");
                codegen.gen_pop("r16");

                codegen.gen_push("r18");
                codegen.gen_push("r16");
                codegen.gen_push("r17");

                //(r17, r16, c0, ...)

                Self::gen_ld_const(codegen, typ_sz, 8); //reg - 8bit
                Self::gen_swap(codegen, typ_sz);

                //(r17, 8, r16, c0, ...)

                Self::gen_sub(codegen, typ_sz);

                //((8 - r17), r16, c0, ...)

                if left { Self::gen_rs(codegen, typ); }
                else { Self::gen_ls(codegen, typ); }

                //(c1, c0, ...)

                Self::gen_or(codegen, typ_sz);
            }
            TypeSize::Word => {
                //(r18, r19, r16, r17, ...)
                //(r18 - LOW, r19 - HIGH) - cnt,
                //(r16 - LOW, r17 - HIGH) - operated

                codegen.gen_pop("r18");
                codegen.gen_pop("r19");
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");

                codegen.gen_push("r17");
                codegen.gen_push("r16");
                codegen.gen_push("r19");
                codegen.gen_push("r18");

                codegen.gen_push("r17");
                codegen.gen_push("r16");
                codegen.gen_push("r19");
                codegen.gen_push("r18");

                //(r18, r19, r16, r17, r18, r19, r16, r17, ...)

                if left { Self::gen_ls(codegen, typ); }
                else { Self::gen_rs(codegen, typ); }

                //(c0L (r20), c0H (r21), r18, r19, r16, r17, ...)
                //c0L - LOW, c0H - HIGH - computed 0

                codegen.gen_pop("r20");
                codegen.gen_pop("r21");
                codegen.gen_pop("r18");
                codegen.gen_pop("r19");
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");

                codegen.gen_push("r21");
                codegen.gen_push("r20");
                codegen.gen_push("r17");
                codegen.gen_push("r16");
                codegen.gen_push("r19");
                codegen.gen_push("r18");

                //(r18, r19, r16, r17, c0L (r20), c0H (r21), ...)

                Self::gen_ld_const(codegen, typ_sz, 16); //word - 16bit
                Self::gen_swap(codegen, typ_sz);

                //(r18, r19, 0x10, 0x00, r16, r17, c0L, c0H, ...)

                Self::gen_sub(codegen, typ_sz);

                //((0x0010 - (r18, r19)), r16, r17, c0L, c0H, ...)

                if left { Self::gen_rs(codegen, typ); }
                else { Self::gen_ls(codegen, typ); }

                //(c1L, c1H, c0L, c0H, ...)
                //c1L - LOW, c1H - HIGH, c1 - computed1

                Self::gen_or(codegen, typ_sz);
            }
        }
    }

    fn gen_rot(codegen: &mut CodeGen, typ: &Type, left: bool) {
        //a <^ b == (a << b) | (a >> (size(a) - b))
        //a ^> b == (a >> b) | (a << (size(a) - b)
        let typ_sz = &TypeSize::from(typ);
        match typ_sz {
            TypeSize::Reg => {
                //r17 - shift
                //r16 - shiftable

                let endlbl = &codegen.gen_delayed_label();

                if *typ == Type::E8 {
                    //r17 - shift
                    //r16 - shiftable

                    let neglbl = &codegen.gen_delayed_label();

                    Self::gen_cpy(codegen, typ_sz);
                    Self::gen_ld_const(codegen, typ_sz, 0);
                    Self::gen_ge(codegen, typ);
                    Self::gen_brf(codegen, neglbl);

                    //>0 part
                    Self::gen_ld_const(codegen, typ_sz, 7); //7 == 0b0111
                    Self::gen_and(codegen, typ_sz);
                    Self::gen_sz_rot(codegen, typ, left);
                    codegen.gen_rjmp(endlbl);
                    //

                    //<0 part
                    codegen.apply_label(neglbl);
                    Self::gen_neg(codegen, typ_sz);
                    Self::gen_ld_const(codegen, typ_sz, 7); //7 == 0b0111
                    Self::gen_and(codegen, typ_sz);
                    Self::gen_sz_rot(codegen, typ, !left);
                    //
                } else {
                    Self::gen_ld_const(codegen, typ_sz, 7); //7 == 0b0111
                    Self::gen_and(codegen, typ_sz);
                    Self::gen_sz_rot(codegen, typ, left);
                }
                codegen.apply_label(endlbl);
            }
            TypeSize::Word => {
                //(XL, XH, r16, r17, ...);
                //(XL, XH) - shift,
                //(r16 - LOW, r17 - HIGH) - shiftable

                let endlbl = &codegen.gen_delayed_label();

                if *typ == Type::E16 {
                    let neglbl = &codegen.gen_delayed_label();

                    Self::gen_cpy(codegen, typ_sz);
                    Self::gen_ld_const(codegen, typ_sz, 0);
                    Self::gen_ge(codegen, typ);
                    Self::gen_brf(codegen, neglbl);

                    //>0 part
                    Self::gen_ld_const(codegen, typ_sz, 15); //15 == 0b1111
                    Self::gen_and(codegen, typ_sz);
                    Self::gen_sz_rot(codegen, typ, left);
                    codegen.gen_rjmp(endlbl);
                    //

                    //<0 part
                    codegen.apply_label(neglbl);
                    Self::gen_neg(codegen, typ_sz);
                    Self::gen_ld_const(codegen, typ_sz, 15); //15 == 0b1111
                    Self::gen_and(codegen, typ_sz);
                    Self::gen_sz_rot(codegen, typ, !left);
                    //
                } else {
                    Self::gen_ld_const(codegen, typ_sz, 15); //15 == 0b1111
                    Self::gen_and(codegen, typ_sz);
                    Self::gen_sz_rot(codegen, typ, left);
                }
                codegen.apply_label(endlbl);
            }
        }
    }

    fn gen_rol(codegen: &mut CodeGen, typ: &Type) {
        Self::gen_rot(codegen, typ, true);
    }

    fn gen_ror(codegen: &mut CodeGen, typ: &Type) {
        Self::gen_rot(codegen, typ, false);
    }

    //cmp
    //stack - (r17, r16, ...) on pos (n, n + 1, ..., RAMEND)

    //if eq: gen eq, if false - gen neq
    fn gen_neq_eq(codegen: &mut CodeGen, typ_sz: &TypeSize, eq: bool) {
        match typ_sz {
            TypeSize::Reg => {
                //(r17, r16, ...)
                codegen.gen_pop("r17");
                codegen.gen_pop("r16");
                codegen.gen_ldi("r18", if eq { 1 } else { 0 });
                codegen.gen_cpse("r16", "r17");
                codegen.gen_ldi("r18", if eq { 0 } else { 1 });
                codegen.gen_push("r18");
            }
            TypeSize::Word => {
                //(r18, r19, r16, r17, ...);
                //(r18 - LOW, r19 - HIGH) - w1 (word1),
                //(r16 - LOW, r17 - HIGH) - w0 (word0),
                codegen.gen_pop("r18");
                codegen.gen_pop("r19");
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");

                codegen.gen_ldi("r20", if eq { 0 } else { 1 });
                codegen.gen_cp("r16", "r18"); //cmp low
                codegen.gen_cpc("r17", "r19"); //cmp high
                let nelbl = &codegen.gen_delayed_label();
                codegen.gen_brne(nelbl);
                codegen.gen_ldi("r20", if eq { 1 } else { 0 });
                codegen.apply_label(nelbl);
                codegen.gen_push("r20");
            }
        }
    }

    fn gen_eq(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_neq_eq(codegen, typ_sz, true);
    }

    fn gen_ne(codegen: &mut CodeGen, typ_sz: &TypeSize) {
        Self::gen_neq_eq(codegen, typ_sz, false);
    }

    fn gen_cmp(
        codegen: &mut CodeGen,
        typ: &Type,
        unsigned: impl Fn(&mut CodeGen, &String),
        signed: impl Fn(&mut CodeGen, &String),
    ) {
        let typ_sz = &TypeSize::from(typ);
        match typ_sz {
            TypeSize::Reg => {
                //(r17, r16, ...)
                let endlbl = &codegen.gen_delayed_label();
                codegen.gen_pop("r17");
                codegen.gen_pop("r16");
                codegen.gen_ldi("r18", 1);
                codegen.gen_cp("r16", "r17");
                if *typ == Type::E8 { signed(codegen, endlbl); } else { unsigned(codegen, endlbl); }
                codegen.gen_clr("r18");
                codegen.apply_label(endlbl);
                codegen.gen_push("r18");
            }
            TypeSize::Word => {
                //(r18, r19, r16, r17, ...);
                //(r18 - LOW, r19 - HIGH) - w1 (word1),
                //(r16 - LOW, r17 - HIGH) - w0 (word0),
                let endlbl = &codegen.gen_delayed_label();
                codegen.gen_pop("r18");
                codegen.gen_pop("r19");
                codegen.gen_pop("r16");
                codegen.gen_pop("r17");
                codegen.gen_ldi("r20", 1);
                codegen.gen_cp("r16", "r18");
                codegen.gen_cpc("r17", "r19");
                if *typ == Type::E16 { signed(codegen, endlbl); } else { unsigned(codegen, endlbl); }
                codegen.gen_clr("r20");
                codegen.apply_label(endlbl);
                codegen.gen_push("r20");
            }
        }
    }

    fn gen_ge(codegen: &mut CodeGen, typ: &Type) {
        Self::gen_cmp(codegen, typ, CodeGen::gen_brsh, CodeGen::gen_brge);
    }

    fn gen_lt(codegen: &mut CodeGen, typ: &Type) {
        Self::gen_cmp(codegen, typ, CodeGen::gen_brlo, CodeGen::gen_brlt);
    }

    fn gen_gt(codegen: &mut CodeGen, typ: &Type) {
        //a > b == b < a
        let typ_sz = &TypeSize::from(typ);
        Self::gen_swap(codegen, typ_sz);
        Self::gen_lt(codegen, typ);
    }

    fn gen_le(codegen: &mut CodeGen, typ: &Type) {
        //a <= b == b >= a
        let typ_sz = &TypeSize::from(typ);
        Self::gen_swap(codegen, typ_sz);
        Self::gen_ge(codegen, typ);
    }

    //

    //branches
    //pops top bool elem from stack and checks it
    fn gen_brt(codegen: &mut CodeGen, lbl: &String) { //branch if true
        //(r16, ...)
        /*codegen.gen_pop("r16");
        codegen.gen_cpi("r16", 1);
        codegen.gen_breq(lbl); (-64 <= k <= 63) */
        let endlbl = &codegen.gen_delayed_label();
        codegen.gen_pop("r16");
        codegen.gen_ldi("r17", 1);
        codegen.gen_cpse("r16", "r17");
        codegen.gen_rjmp(endlbl);
        codegen.gen_rjmp(lbl);
        codegen.apply_label(endlbl);
    }

    pub(crate) fn gen_brf(codegen: &mut CodeGen, lbl: &String) { //branch if false
        //(r16, ...)
        /*codegen.gen_pop("r16");
        codegen.gen_cpi("r16", 1);
        codegen.gen_brne(lbl); (-64 <= k <= 63) */
        let endlbl = &codegen.gen_delayed_label();
        codegen.gen_pop("r16");
        codegen.gen_clr("r17");
        codegen.gen_cpse("r16", "r17");
        codegen.gen_rjmp(endlbl);
        codegen.gen_rjmp(lbl);
        codegen.apply_label(endlbl);
    }
    //

    pub(crate) fn gen(&mut self, cmd: ExprCmd) { self.cmds.push(cmd); }

    pub(crate) fn apply(&self, codegen: &mut CodeGen, typ: &Type) {
        if *typ == Type::None {
            for cmd in &self.cmds {
                if let ExprCmd::FN(name, arg, res) = cmd {
                    Self::gen_fn(codegen, name, arg, res);
                } else { ErrorsHandler::error_0100(None); }
            }
        } else {
            let typ_sz = &TypeSize::from(typ);
            for cmd in &self.cmds {
                match cmd {
                    ExprCmd::Cpy => { Self::gen_cpy(codegen, typ_sz); }
                    ExprCmd::Swap => { Self::gen_swap(codegen, typ_sz); }
                    ExprCmd::FN(name, arg, res) => {
                        Self::gen_fn(codegen, name, arg, res);
                    }
                    ExprCmd::LdVar(ptr) => { Self::gen_ld_var(codegen, typ_sz, *ptr); }
                    ExprCmd::LdConst(val) => { Self::gen_ld_const(codegen, typ_sz, *val); }
                    ExprCmd::Neg => { Self::gen_neg(codegen, typ_sz); }
                    ExprCmd::Add => { Self::gen_add(codegen, typ_sz); }
                    ExprCmd::Sub => { Self::gen_sub(codegen, typ_sz); }
                    ExprCmd::And => { Self::gen_and(codegen, typ_sz); }
                    ExprCmd::Or => { Self::gen_or(codegen, typ_sz); }
                    ExprCmd::Xor => { Self::gen_xor(codegen, typ_sz); }
                    ExprCmd::Not => { Self::gen_not(codegen, typ); }
                    ExprCmd::LS => { Self::gen_ls(codegen, typ); }
                    ExprCmd::RS => { Self::gen_rs(codegen, typ); }
                    ExprCmd::ROL => { Self::gen_rol(codegen, typ); }
                    ExprCmd::ROR => { Self::gen_ror(codegen, typ); }
                    ExprCmd::EQ => { Self::gen_eq(codegen, typ_sz); }
                    ExprCmd::NE => { Self::gen_ne(codegen, typ_sz); }
                    ExprCmd::LT => { Self::gen_lt(codegen, typ); }
                    ExprCmd::LE => { Self::gen_le(codegen, typ); }
                    ExprCmd::GT => { Self::gen_gt(codegen, typ); }
                    ExprCmd::GE => { Self::gen_ge(codegen, typ); }
                }
            }
        }
    }

    pub(crate) fn clear(&mut self) { self.cmds = Vec::new(); }

    pub(crate) fn is_empty(&mut self) -> bool { return self.cmds.is_empty(); }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub(crate) enum StdModule {
    ElEn,
    Tempo,
    Matem,
}

#[derive(Clone)]
pub(crate) struct ImportGenBuf {
    modules: HashSet<StdModule>
}

impl ImportGenBuf {
    fn new() -> Self {
        return Self { modules: HashSet::new(), }
    }

    pub(crate) fn gen(&mut self, module: StdModule) {
        self.modules.insert(module);
    }

    //std functions (labels starts with std
    fn gen_skr_kar(codegen: &mut CodeGen) {
        //
        codegen.clr_stsz();

        codegen.apply_label(&String::from("skr_kar"));

        codegen.gen_form_var(&TypeSize::Reg); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //


        codegen.gen_ret();
    }

    fn gen_prokrasto(codegen: &mut CodeGen) {
        //init
        codegen.clr_stsz();


        codegen.apply_label(&String::from("prokrasto"));

        let arg_ptr = codegen.gen_form_var(&TypeSize::Word); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //

        ExprGenBuf::gen_ld_var(codegen, &TypeSize::Word, arg_ptr);

        //(YL, YH, ...)
        codegen.gen_pop("ZL");
        codegen.gen_pop("ZH");
        let oiterlbl = codegen.gen_label(); //outer
        codegen.gen_ldi("XL", CodeGen::low(28168));
        codegen.gen_ldi("XH", CodeGen::high(28168));
        let iiterlbl = codegen.gen_label(); //inner
        codegen.gen_sbiw("XH:XL", 1);
        codegen.gen_brne(&iiterlbl);
        codegen.gen_sbiw("ZH:ZL", 1);
        codegen.gen_brne(&oiterlbl);

        //ret
        codegen.gen_ret();
        //
    }

    fn gen_malt(codegen: &mut CodeGen) {
        //init
        codegen.clr_stsz();

        codegen.apply_label(&String::from("malt"));

        let res_ptr = codegen.gen_form_var(&TypeSize::Reg); // res

        let arg_ptr = codegen.gen_form_var(&TypeSize::Word); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //

        ExprGenBuf::gen_ld_var(codegen, &TypeSize::Word, arg_ptr);

        //(r16, r17, ...)
        //r16 - LOW
        //r17 - HIGH
        codegen.gen_pop("r16");
        codegen.gen_pop("r17");
        codegen.gen_push("r16");
        //(r16, ...)
        codegen.gen_st_var(&TypeSize::Reg, res_ptr);

        //ret
        codegen.gen_ret();
        //
    }

    fn gen_alt(codegen: &mut CodeGen) {
        //init
        codegen.clr_stsz();

        codegen.apply_label(&String::from("alt"));

        let res_ptr = codegen.gen_form_var(&TypeSize::Reg); // res

        let arg_ptr = codegen.gen_form_var(&TypeSize::Word); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //

        ExprGenBuf::gen_ld_var(codegen, &TypeSize::Word, arg_ptr);

        //(r16, r17, ...)
        //r16 - LOW
        //r17 - HIGH
        codegen.gen_pop("r16");
        codegen.gen_pop("r17");
        codegen.gen_push("r17");
        //(r16, ...)

        codegen.gen_st_var(&TypeSize::Reg, res_ptr);

        //ret
        codegen.gen_ret();
        //
    }

    fn gen_gx_s16(codegen: &mut CodeGen) {
        //init
        codegen.clr_stsz();

        codegen.apply_label(&String::from("gx_s16"));

        let res_ptr = codegen.gen_form_var(&TypeSize::Word); // res

        let arg_ptr = codegen.gen_form_var(&TypeSize::Reg); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //

        ExprGenBuf::gen_ld_var(codegen, &TypeSize::Reg, arg_ptr);

        //(r16, ...)
        //r16 - LOW
        //r17 - HIGH
        codegen.gen_clr("r17");
        codegen.gen_pop("r16");
        codegen.gen_push("r17");
        codegen.gen_push("r16");
        //(r16, r17, ...)

        codegen.gen_st_var(&TypeSize::Word, res_ptr);

        //ret
        codegen.gen_ret();
        //
    }

    fn gen_fik_ddrb(codegen: &mut CodeGen) {
        //
        codegen.clr_stsz();

        codegen.apply_label(&String::from("fik_ddrb"));

        let arg_ptr = codegen.gen_form_var(&TypeSize::Reg); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //

        ExprGenBuf::gen_ld_var(codegen, &TypeSize::Reg, arg_ptr);

        codegen.gen_pop("r16");
        codegen.gen_out(CodeGen::DDRB, "r16");

        codegen.gen_ret();
    }

    fn gen_fik_portb(codegen: &mut CodeGen) {
        //
        codegen.clr_stsz();

        codegen.apply_label(&String::from("fik_portb"));

        let arg_ptr = codegen.gen_form_var(&TypeSize::Reg); // arg

        codegen.gen_form_var(&TypeSize::Word); //ret addr
        //

        ExprGenBuf::gen_ld_var(codegen, &TypeSize::Reg, arg_ptr);

        codegen.gen_pop("r16");
        codegen.gen_out(CodeGen::PORTB, "r16");

        codegen.gen_ret();
    }
    //

    //std modules
    fn gen_elen(codegen: &mut CodeGen) {
        Self::gen_skr_kar(codegen);
        Self::gen_fik_ddrb(codegen);
        Self::gen_fik_portb(codegen);
    }

    fn gen_tempo(codegen: &mut CodeGen) { Self::gen_prokrasto(codegen); }

    fn gen_matem(codegen: &mut CodeGen) {
        Self::gen_malt(codegen);
        Self::gen_alt(codegen);
        Self::gen_gx_s16(codegen);
    }
    //

    fn apply(&self, codegen: &mut CodeGen) {
        for module in &self.modules {
            match module {
                StdModule::ElEn => { Self::gen_elen(codegen); }
                StdModule::Tempo => { Self::gen_tempo(codegen); }
                StdModule::Matem => { Self::gen_matem(codegen); }
            }
        }
    }
}

macro_rules! cmd {
    ($codegen:ident, $($cmd:tt)*) => {{
        $codegen.cmd(format!($($cmd)*).as_str());
    }};
}

#[derive(Clone)]
pub(crate) struct CodeGen {
    lbl_cnt: usize,
    gen: String, //Generated
    stack_sz: i32, //tmp solution
    pub(crate) import_buf: ImportGenBuf,
}

impl CodeGen {
    pub fn new() -> Self {
        return Self {
            lbl_cnt: 0,
            gen: String::new(),
            stack_sz: 0,
            import_buf: ImportGenBuf::new(),
        };
    }

    fn write(&mut self, data: String) {
        self.gen += data.as_str();
    }

    fn cmd(&mut self, data: &str) {
        return self.write(String::from(format!("{data}\n")));
    }

    fn comment(&mut self, data: &str) { return self.write(format!(";{data}\n")); }

    fn low(val: i32) -> u8 { return val.to_le_bytes()[0]; }

    fn high(val: i32) -> u8 { return val.to_le_bytes()[1]; }

    //labels
    pub(crate) fn gen_label(&mut self) -> String {
        let res = self.gen_delayed_label();
        self.apply_label(&res);
        return res;
    }

    pub(crate) fn gen_delayed_label(&mut self) -> String {
        let res = format!("lbl{:0x}", self.lbl_cnt);
        self.lbl_cnt += 1;
        return res;
    }

    pub(crate) fn apply_label(&mut self, lbl: &String) { cmd!(self, "{lbl}:"); }
    //

    //branches
    pub(crate) fn gen_rjmp(&mut self, lbl: &String) { cmd!(self, "rjmp {lbl}"); }

    fn gen_breq(&mut self, lbl: &String) { cmd!(self, "breq {lbl}"); }

    fn gen_brne(&mut self, lbl: &String) { cmd!(self, "brne {lbl}"); }

    fn gen_brsh(&mut self, lbl: &String) { cmd!(self, "brsh {lbl}"); }

    fn gen_brge(&mut self, lbl: &String) { cmd!(self, "brge {lbl}"); }

    fn gen_brlo(&mut self, lbl: &String) { cmd!(self, "brlo {lbl}"); }

    fn gen_brlt(&mut self, lbl: &String) { cmd!(self, "brlt {lbl}"); }
    //

    //calls
    pub(crate) fn gen_rcall(&mut self, lbl: &String) { cmd!(self, "rcall {lbl}"); }

    pub(crate) fn gen_ret(&mut self) { cmd!(self, "ret"); }
    //

    //stack
    fn gen_push(&mut self, reg: &str) { cmd!(self, "push {reg}"); }

    fn gen_pop(&mut self, reg: &str) { cmd!(self, "pop {reg}"); }
    //

    //mem
    pub(crate) fn gen_sp_to_bp(&mut self) {
        self.gen_in("YL", Self::SPL);
        self.gen_in("YH", Self::SPH);
        self.gen_sbiw("YH:YL", 63);
    }

    fn gen_movw(&mut self, word: &str, src_word: &str) {
        cmd!(self, "movw {word}, {src_word}");
    }

    fn gen_mov(&mut self, reg: &str, src_reg: &str) {
        cmd!(self, "mov {reg}, {src_reg}");
    }

    fn gen_clr(&mut self, reg: &str) { cmd!(self, "clr {reg}"); }

    fn gen_ldi(&mut self, reg: &str, val: u8) { cmd!(self, "ldi {reg}, {:#04x}", val); }

    fn gen_ld(&mut self, reg: &str, ptr_reg: &str) { cmd!(self, "ld {reg}, {ptr_reg}"); }

    fn gen_ldd(&mut self, reg: &str, ptr_reg: &str, shift: u8) {
        if shift > 64 { ErrorsHandler::dev_error_0912(shift); }
        cmd!(self, "ldd {reg}, {ptr_reg}+{shift}");
    }

    fn gen_st(&mut self, ptr_reg: &str, reg: &str) { cmd!(self, "st {ptr_reg}, {reg}"); }

    fn gen_std(&mut self, ptr_reg: &str, reg: &str, shift: u8) {
        if shift > 64 { ErrorsHandler::dev_error_0913(shift); }
        cmd!(self, "std {ptr_reg}+{shift}, {reg}");
    }

    fn gen_out(&mut self, addr: i32, reg: &str) { cmd!(self, "out {:#04x}, {reg}", addr); }

    fn gen_in(&mut self, reg: &str, addr: i32) { cmd!(self, "in {reg}, {:#04x}", addr); }
    //

    //operations
    fn gen_add(&mut self, reg1: &str, reg2: &str) { cmd!(self, "add {reg1}, {reg2}"); }

    fn gen_adc(&mut self, reg1: &str, reg2: &str) { cmd!(self, "adc {reg1}, {reg2}"); }

    fn gen_sub(&mut self, reg1: &str, reg2: &str) { cmd!(self, "sub {reg1}, {reg2}"); }

    fn gen_sbc(&mut self, reg1: &str, reg2: &str) { cmd!(self, "sbc {reg1}, {reg2}"); }

    fn gen_and(&mut self, reg1: &str, reg2: &str) { cmd!(self, "and {reg1}, {reg2}"); }

    fn gen_or(&mut self, reg1: &str, reg2: &str) { cmd!(self, "or {reg1}, {reg2}"); }

    fn gen_eor(&mut self, reg1: &str, reg2: &str) { cmd!(self, "eor {reg1}, {reg2}"); }

    fn gen_com(&mut self, reg: &str) { cmd!(self, "com {reg}"); }

    fn gen_dec(&mut self, reg: &str) { cmd!(self, "dec {reg}"); }

    fn gen_sbiw(&mut self, reg: &str, val: u8) {
        if val > 63 { ErrorsHandler::dev_error_0910(val); }
        cmd!(self, "sbiw {reg}, {:#04x}", val);
    }

    fn gen_adiw(&mut self, reg: &str, val: u8) {
        if val > 63 { ErrorsHandler::dev_error_0911(val); }
        cmd!(self, "adiw {reg}, {:#04x}", val);
    }

    fn gen_lsl(&mut self, reg: &str) { cmd!(self, "lsl {reg}"); }

    fn gen_lsr(&mut self, reg: &str) { cmd!(self, "lsr {reg}"); }

    fn gen_rol(&mut self, reg: &str) { cmd!(self, "rol {reg}"); }

    fn gen_ror(&mut self, reg: &str) { cmd!(self, "ror {reg}"); }
    //

    //cmp
    fn gen_cpi(&mut self, reg1: &str, val: u8) { cmd!(self, "cpi {reg1}, {:#04x}", val); }

    fn gen_cp(&mut self, reg1: &str, reg2: &str) { cmd!(self, "cp {reg1}, {reg2}"); }

    fn gen_cpc(&mut self, reg1: &str, reg2: &str) { cmd!(self, "cpc {reg1}, {reg2}"); }

    fn gen_cpse(&mut self, reg1: &str, reg2: &str) { cmd!(self, "cpse {reg1}, {reg2}"); }
    //

    //asm notations
    fn gen_equ(&mut self, alias: &str, assignable: i32) {
        let low = Self::low(assignable);
        let high = Self::high(assignable);
        if high == 0 {
            cmd!(self, ".equ {alias} = 0x{:02x}{:02x}", high, low);
        } else { cmd!(self, ".equ {alias} = {:#04x}", low); }
    }

    fn gen_def(&mut self, alias: &str, assignable: &str) {
        cmd!(self, ".def {alias} = {assignable}");
    }

    fn gen_cseg(&mut self) { cmd!(self, ".cseg"); }

    fn gen_org(&mut self, addr: i32) { cmd!(self, ".org {:#06x}", addr); }
    //

    //others
    fn gen_nop(&mut self) { cmd!(self, "nop"); }
    //

    //tmp
    pub(crate) fn gen_st_var(&mut self, typ_sz: &TypeSize, ptr: i32) {
        self.gen_pop("r16");
        if ptr < 0 || ptr > 63 {
            self.gen_ldi("XL", CodeGen::low(ptr));
            self.gen_ldi("XH", CodeGen::high(ptr));
            self.gen_add("XL", "YL");
            self.gen_adc("XH", "YH");
            self.gen_st("X", "r16");
        } else { self.gen_std("Y", "r16", ptr as u8); }
        if *typ_sz == TypeSize::Word {
            self.gen_pop("r16");
            let hptr = ptr + 1;
            if hptr < 0 || hptr > 63 {
                self.gen_ldi("XL", CodeGen::low(hptr));
                self.gen_ldi("XH", CodeGen::high(hptr));
                self.gen_add("XL", "YL");
                self.gen_adc("XH", "YH");
                self.gen_st("X", "r16");
            } else { self.gen_std("Y", "r16", hptr as u8); }
        }
    }

    pub(crate) fn gen_st_bp(&mut self) {
        self.gen_push("YH"); //HIGH
        self.gen_push("YL"); //LOW
        self.stack_sz += 2;
    }

    pub(crate) fn gen_ld_bp(&mut self) {
        self.gen_pop("YL"); //LOW
        self.gen_pop("YH"); //HIGH
        self.stack_sz -= 2;
    }

    pub(crate) fn gen_form_var(&mut self, typ_sz: &TypeSize) -> i32 {
        self.stack_sz += match typ_sz { TypeSize::Reg => { 1 } TypeSize::Word => { 2 } };
        let ptr = 63 - self.stack_sz + 1; //Base pointer is shifted by 63
        return ptr;
    }

    fn gen_set_sp(&mut self, addr: i32) {
        self.gen_ldi("r16", Self::low(addr));
        self.gen_out(Self::SPL, "r16");
        self.gen_ldi("r16", Self::high(addr));
        self.gen_out(Self::SPH, "r16");
    }

    pub(crate) fn gen_bp_to_sp(&mut self) {
        self.gen_movw("XH:XL", "YH:YL");
        self.gen_adiw("XH:XL", 63);
        self.gen_out(Self::SPL, "XL");
        self.gen_out(Self::SPH, "XH");
    }

    pub(crate) fn gen_set_sp_rel_bp(&mut self, mut shift: i32) {
        shift += 63;
        self.gen_ldi("XL", CodeGen::low(shift));
        self.gen_ldi("XH", CodeGen::high(shift));
        self.gen_add("XL", "YL");
        self.gen_adc("XH", "YH");
        self.gen_out(Self::SPL, "XL");
        self.gen_out(Self::SPH, "XH");
    }

    pub(crate) fn gen_drop(&mut self, sz: i32) {
        self.stack_sz -= sz;
        self.gen_set_sp_rel_bp(self.stack_sz);
    }

    pub(crate) fn clr_stsz(&mut self) { self.stack_sz = 0; }

    pub(crate) fn inc_stsz(&mut self, delta: i32) { self.stack_sz += delta; }

    //

    const RAMEND: i32 = 0x025f;
    const SPL: i32 = 0x3d;
    const SPH: i32 = 0x3e;
    pub(crate) const DDRB: i32 = 0x17;
    pub(crate) const PORTB: i32 = 0x18;

    pub(crate) fn init(&mut self) {
        //WORDS - REG PAIRS
        self.gen_def("XH", "r27");
        self.gen_def("XL", "r26");
        self.gen_def("YH", "r29");
        self.gen_def("YL", "r28");
        self.gen_def("ZH", "r31");
        self.gen_def("ZL", "r30");
        //

        //SEG ORG
        self.gen_cseg();
        self.gen_org(0x00);
        //

        //INIT STACK
        self.gen_set_sp(Self::RAMEND);
        //

        ExprGenBuf::gen_fn(
            self,
            &String::from("baza"),
            &Type::None,
            &Type::None
        );

        self.gen_rjmp(&String::from("end"));
    }

    pub(crate) fn end(&mut self) {
        self.apply_label(&String::from("end"));
        self.gen_nop();
        self.gen_rjmp(&String::from("end"));
        let import_buf = self.import_buf.clone();
        import_buf.apply(self);
    }

    async fn open_output_file(output_path: String) -> Option<File> {
        if let Ok(file) = OpenOptions::new()
            .append(true)
            .truncate(true)
            .open(output_path.clone()).await {
            return Some(file);
        } else {
            if let Some(dir_path) =
                Path::new(output_path.clone().as_str()).parent() {
                if let Ok(_) = create_dir_all(dir_path).await {
                    if let Ok(file) =
                        File::create(output_path.clone()).await {
                        return Some(file);
                    }
                }
            }
        }
        return None;
    }

    pub(crate) async fn commit(&self, output_path: String) -> bool {
        if let Some(mut file) = Self::open_output_file(
            output_path
        ).await {
            if let Ok(_) = file.write_all(self.gen.as_bytes()).await { return true; }
        }
        return false;
    }
}