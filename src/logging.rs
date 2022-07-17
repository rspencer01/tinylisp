macro_rules! trace {
    ( $( $x:expr ),* ) => {
        {
            println!("TRACE {}",
                format!(
                    $( $x , )*
                )
            );
        }
    };
}
