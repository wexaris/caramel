#![allow(unused_imports)]

macro_rules! expect {
    ( $this:ident, $( $ex:ident ).+ ) => {
        match $this.curr.token_type == $($ex).+ {
            true => Ok($this.bump()),
            _ => {
                if $this.curr.is_invalid() {
                    Err(ParseError::InvalidSymbol(
                        $this.origin().get_substr_from_span(&$this.curr.span).to_owned(),
                        $this.curr.span.clone()
                    ))
                }
                else if $this.curr.is_eof() {
                    Err(ParseError::UnexpectedEof($this.curr.clone()))
                }
                else {
                    Err(ParseError::UnexpectedToken(
                        $this.curr.clone(),
                        vec![ format!("{:?}", $($ex).+) ]
                    ))
                }
            }
        }
    };
    ( $this:ident, [ $( $ex:expr ),+ ] ) => {
        match [ $( $ex ),+ ].contains(&$this.curr.token_type) {
            true => Ok($this.bump()),
            _ => {
                if $this.curr.is_invalid() {
                    Err(ParseError::InvalidSymbol(
                        $this.origin().get_substr_from_span(&$this.curr.span).to_owned(),
                        $this.curr.span.clone()
                    ))
                }
                else if $this.curr.is_eof() {
                    Err(ParseError::UnexpectedEof($this.curr.clone()))
                }
                else {
                    Err(ParseError::UnexpectedToken(
                        $this.curr.clone(),
                        vec![ $( format!("{:?}", $ex) ),+ ]
                    ))
                }
            }
        }
    };
    ( $this:ident, $pattern:pat $(if $guard:expr)? $(,)? ) => {
        match $this.curr.token_type {
            $pattern $(if $guard)? => Ok($this.bump()),
            _ => {
                if $this.curr.is_invalid() {
                    Err(ParseError::InvalidSymbol(
                        $this.origin().get_substr_from_span(&$this.curr.span).to_owned(),
                        $this.curr.span.clone()
                    ))
                }
                else if $this.curr.is_eof() {
                    Err(ParseError::UnexpectedEof($this.curr.clone()))
                }
                else {
                    Err(ParseError::UnexpectedToken(
                        $this.curr.clone(),
                        vec![ String::from(stringify!($pattern)) ]
                    ))
                }
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
