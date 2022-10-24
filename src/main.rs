mod es;
use syn::{File, Item};

fn main() -> Result<(), syn::Error> {
    let t: File = syn::parse_str(
        "
        struct Test {}
        fn main() {
            let a = 1;
        }
        ",
    )?;
    for item in t.items {
        match item {
            Item::Fn(f) => {
                println!("fn {}", f.sig.ident);
            }
            Item::Struct(st) => {
                println!("struct {}", st.ident);
            }
            _ => {
                panic!("Branch not handled");
            }
        }
    }
    Ok(())
}
