macro_rules! trace {
    ( $( $x:expr ),* ) => {
        {
            if std::env::var("LISP_TRACE").is_ok() {
                println!("\x1B[36mTRACE\x1B[0m {}",
                   format!(
                       $( $x , )*
                   )
                );
            }
        }
    };
}
