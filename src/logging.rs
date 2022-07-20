macro_rules! trace {
    ( $( $x:expr ),* ) => {
        {
            println!("\x1B[36mTRACE\x1B[0m {}",
                format!(
                    $( $x , )*
                )
            );
        }
    };
}
