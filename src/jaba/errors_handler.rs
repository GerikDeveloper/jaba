use std::fmt::format;
use crate::jaba::src_driver::GlobPos;

pub(crate) struct ErrorsHandler {

}

impl ErrorsHandler {
    fn error(message: String, glob_pos: Option<GlobPos>) {
        if let Some(gpos) = glob_pos {
            println!(
                "eraro: {message} --> {}:{}:{}",
                gpos.src_path,
                gpos.line_pos + 1,
                gpos.ch_pos + 1,
            );
        } else {
            println!("eraro: {message}");
        }
        std::process::exit(-1);
    }

    fn warning(message: String, glob_pos: Option<GlobPos>) {
        if let Some(glob_pos) = glob_pos {
            println!(
                "averto: {message} --> {}:{}:{}",
                glob_pos.src_path,
                glob_pos.line_pos + 1,
                glob_pos.ch_pos + 1,
            );
        } else { println!("averto: {message}"); }
    }

    fn dev_error(message: String) {
        println!("evo eraro: {message}");
        std::process::exit(-1);
    }

    fn dev_warning(message: String) {
        println!("evo averto: {message}");
    }

    fn notice(message: String) { println!("sciigo: {message}"); }

    //Formed errors and warnings

    //general

    pub(crate) fn error_0010(expected: Option<String>, found: Option<String>, glob_pos: GlobPos) {
        if let Some(exp) = expected {
            if let Some(fnd) = found {
                Self::error(
                    format!("atendata {exp}, trovita {fnd}"),
                    Some(glob_pos),
                );
            } else {
                Self::error(
                    format!("atendata {exp}"),
                    Some(glob_pos),
                );
            }
        } else {
            if let Some(fnd) = found {
                Self::error(
                    format!("simbolo {fnd} ne estis atendita"),
                    Some(glob_pos),
                );
            } else {
                Self::error(
                    format!("simbolo ne estis atendita"),
                    Some(glob_pos),
                );
            }
        }
    }

    pub(crate) fn error_0011(glob_pos: GlobPos) {
        Self::error(
            String::from(
                format!("plurlinia kommento ne estis fermita")
            ),
            Some(glob_pos),
        );
    }

    //digits (lex)

    pub(crate) fn error_0020(radix: i32, glob_pos: GlobPos) {
        Self::error(
            String::from(
                format!("malvalida cifero por numero de bazo {radix}")
            ),
            Some(glob_pos),
        );
    }

    pub(crate) fn error_0021(glob_pos: GlobPos) {
        Self::error(
            String::from(
                format!("numero estas tro granda")
            ),
            Some(glob_pos),
        );
    }

    //

    //

    //types

    pub(crate) fn error_0100(glob_pos: Option<GlobPos>) {
        Self::error(
            String::from("malkongruaj tipoj"),
            glob_pos,
        );
    }

    pub(crate) fn error_0101(name: String, glob_pos: GlobPos) {
        Self::error(
            String::from(format!("{name} ne estas nomo de tipo")),
            Some(glob_pos),
        );
    }

    pub(crate) fn error_0102(glob_pos: GlobPos) {
        Self::error(
            String::from("tipa troo"),
            Some(glob_pos),
        );
    }

    //

    //was not found

    //modules
    pub(crate) fn error_0210() {
        Self::error(
            String::from("nomo de modulo ne trovita"),
            None,
        );
    }

    pub(crate) fn error_0211(glob_pos: GlobPos) {
        Self::error(
            String::from("atendata nomo de modulo"),
            Some(glob_pos),
        );
    }

    //

    //operators

    pub(crate) fn error_0310(glob_pos: GlobPos) {
        Self::error(
            String::from("ne elbas apliki '-' al konst bool esprimo"),
            Some(glob_pos),
        );
    }

    //

    //def
    pub(crate) fn error_0410(glob_pos: GlobPos) {
        Self::error(
            String::from("ne elbas defini konst uzante mem"),
            Some(glob_pos),
        );
    }
    //

    //fn & red
    pub(crate) fn error_0711(glob_pos: GlobPos) {
        Self::error(
            String::from("red voki ekstera funkcio"),
            Some(glob_pos),
        );
    }

    pub(crate) fn error_0717(glob_pos: Option<GlobPos>) {
        Self::error(
            String::from("ne povas redefini funkcio"),
            glob_pos,
        );
    }

    pub(crate) fn error_0718(glob_pos: GlobPos) {
        Self::error(
            String::from("malgxusta rezulta tipo en efektivigo"),
            Some(glob_pos),
        );
    }

    pub(crate) fn error_0719(glob_pos: GlobPos) {
        Self::error(
            String::from("malgxusta argumenta tipo en efektivigo"),
            Some(glob_pos),
        );
    }

    pub(crate) fn error_0721(glob_pos: GlobPos) {
        Self::error(
            String::from("ne povas redifini n"),
            Some(glob_pos),
        );
    }

    pub(crate) fn error_0722(name: String) {
        Self::error(
            format!("funkcio kun nomo '{name}' ne estis efektivigita"),
            None,
        );
    }

    pub(crate) fn error_0713() {
        Self::error(
            String::from("ne ekzistas baza funkcio"),
            None,
        );
    }

    pub(crate) fn error_0001() {
        Self::error(
            String::from("malsukcesis malfermi fnt-dosieron"),
            None,
        );
    }


    //table

    pub(crate) fn error_0614() {
        Self::error(
            String::from("nova difino de la nomo"),
            None
        );
    }

    pub(crate) fn error_0616(glob_pos: Option<GlobPos>) {
        ErrorsHandler::error(
            String::from("nomo ne estis deklarita en tiu amplekso"),
            glob_pos,
        );
    }

    //

    //

    //


    //dev errors

    //type

    pub(crate) fn dev_error_0102() {
        Self::dev_error(
            String::from("grandeco ne estis deklarita por tiu tipo")
        );
    }

    pub(crate) fn dev_error_0103() {
        Self::dev_error(
            String::from("kalkulita tipo de la esprimo estas neniu")
        );
    }

    //

    //table
    pub(crate) fn dev_error_0610() {
        Self::dev_error(String::from("nomo ne estis aldonita en la tabelo"));
    }

    pub(crate) fn dev_error_0611() {
        Self::dev_error(String::from("tipo havas neniun intervalon"));
    }

    pub(crate) fn dev_error_0612() {
        Self::dev_error(
            String::from("amplekso tipo ne estas asignita por cxi tiu kodo"),
        );
    }

    pub(crate) fn dev_error_0615() {
        Self::dev_error(String::from("tabelo provas trovi eniron en nul"));
    }
    //

    //fn & red

    pub(crate) fn dev_error_0714() {
        Self::dev_error(String::from("std funkcio havas neniun arg tipo"));
    }

    pub(crate) fn dev_error_0715() {
        Self::dev_error(String::from("std funkcio havas neniun red tipo"));
    }

    pub(crate) fn dev_error_0716() {
        Self::dev_error(String::from("std funkcio havas neniun nomo"));
    }

    pub(crate) fn dev_error_0720() {
        Self::dev_error(String::from("ne povas trovi fn datumoj"));
    }
    //

    //codegen
    pub(crate) fn dev_error_0910(val: u8) {
        Self::dev_error(String::from(
            format!("sbiw atendas konst kun valoro 0..=63, trovita: {val}")
        ));
    }

    pub(crate) fn dev_error_0911(val: u8) {
        Self::dev_error(String::from(
            format!("adiw atendas konst kun valoro 0..=63, trovita: {val}")
        ));
    }

    pub(crate) fn dev_error_0912(val: u8) {
        Self::dev_error(String::from(
            format!("ldd atendas konst kun valoro 0..=63, trovita: {val}")
        ));
    }

    pub(crate) fn dev_error_0913(val: u8) {
        Self::dev_error(String::from(
            format!("std atendas konst kun valoro 0..=63, trovita: {val}")
        ));
    }
    //

    //

    //warning

    //red
    pub(crate) fn warning_0710(glob_pos: GlobPos) {
        Self::warning(
            String::from("revenvaloro ne estas uzita"),
            Some(glob_pos),
        );
    }

    pub(crate) fn warning_0712(glob_pos: GlobPos) {
        Self::warning(
            String::from("fantomo kodo post red"),
            Some(glob_pos)
        );
    }

    pub(crate) fn warning_0723(name: String) {
        Self::warning(
            format!("funkcio kun nomo '{name}' neniam estas uzata"),
            None,
        );
    }
    //

    //scope
    pub(crate) fn warning_0810(glob_pos: GlobPos) {
        Self::warning(
            String::from("amplekso estas malplena"),
            Some(glob_pos)
        );
    }
    //

    //

    //notice
    pub(crate) fn notice_0000() {
        Self::notice(String::from("la kompilado estis sukcese finita"));
    }
    //

    //
}