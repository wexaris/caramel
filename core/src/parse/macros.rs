#![allow(unused_imports)]

macro_rules! expect {
    ( $this:ident, $( $ex:ident ).+ ) => {
        match $this.curr.token_type == $($ex).+ {
            true => Ok($this.bump()),
            _ => {
                Err(ParseError::UnexpectedToken(
                    $this.curr.clone(),
                    vec![ format!("{:?}", $($ex).+) ]
                ))
            }
        }
    };
    ( $this:ident, [ $( $ex:expr ),+ ] ) => {
        match [ $( $ex ),+ ].contains(&$this.curr.token_type) {
            true => Ok($this.bump()),
            _ => {
                Err(ParseError::UnexpectedToken(
                    $this.curr.clone(),
                    vec![ $( format!("{:?}", $ex) ),+ ]
                )),
            }
        }
    };
    ( $this:ident, $pattern:pat $(if $guard:expr)? $(,)? ) => {
        match $this.curr.token_type {
            $pattern $(if $guard)? => Ok($this.bump()),
            _ => {
                Err(ParseError::UnexpectedToken(
                    $this.curr.clone(),
                    vec![ String::from(stringify!($pattern)) ]
                ))
            }
        }
    };
}

macro_rules! consume {
    ( $this:ident, $( $ex:ident ).+ ) => {
        expect!($this, $( $ex ).+).is_ok()
    };
    ( $this:ident, [ $( $ex:expr ),+ ] ) => {
        expect!($this, [ $( $ex ),+ ]).is_ok()
    };
    ( $this:ident, $pattern:pat $(if $guard:expr)? $(,)? ) => {
        expect!($this, $pattern $(if $guard)?).is_ok()
    };
}
