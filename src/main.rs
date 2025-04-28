use JabaAVRCompiler::jaba::compiler::Compiler;

#[tokio::main]
async fn main() {
    let src_path = String::from("jaba/fnt/baza.jaba");
    let output_path = String::from("jaba/eligo/baza.asm");
    let compiler = Compiler::new(src_path, output_path);
    compiler.compile().await;
}
