use crate::ast::{AssignStmt, Root, Expr, Ident, IdentList, IfStmt, Literal, Op, ReadStmt, Stmt, StmtList, WhileStmt, WriteStmt, SkipStmt, Value, BinaryOpExpr, UnaryOpExpr};
use crate::parse::{FilePos, Span, Token, TokenStream, TokenType};

pub struct Parser {
    ts: TokenStream,
    prev: Token,
    curr: Token,
    next: Token,
    idx: usize,
    error_count: u32,
}

macro_rules! check_tok {
    ($tok:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $tok.tt {
            $( $pattern )|+ $( if $guard )? => true,
            _ => false,
        }
    };
}

macro_rules! expect {
    ($parser:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $parser.curr.tt {
            $( $pattern )|+ $( if $guard )? => {
                let tok = $parser.curr.clone();
                $parser.bump();
                Ok(tok)
            },
            _ => {
                $parser.error_count += 1;
                Err(Error::UnexpectedToken($parser.curr.clone()))
            }
        }
    };
}

impl Parser {
    pub fn new(ts: TokenStream) -> Self {
        let mut parser = Parser {
            idx: 0,
            prev: Token::default(),
            curr: Token::default(),
            next: ts.get(0).map(|x| x.clone()).unwrap_or_default(),
            ts,
            error_count: 0,
        };
        parser.bump();
        parser
    }

    pub fn parse(&mut self) -> Root {
        let stmt_list = self.parse_stmt_list(|tok| tok.is_valid());
        Root { stmt_list }
    }

    fn parse_stmt_list<F: Fn(&Token) -> bool>(&mut self, check: F) -> StmtList {
        let start = self.curr_span_start();

        let mut items = vec![];
        while self.curr.is_valid() && check(&self.curr) {
            if let Some(stmt) = self.try_parse_stmt() {
                items.push(stmt);
            }
        }

        StmtList {
            span: self.span_since(start),
            items
        }
    }

    fn try_parse_stmt(&mut self) -> Option<Stmt> {
        match self.parse_stmt() {
            Ok(x) => x,
            Err(e) => {
                println!("{}", e);
                self.recover();
                None
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>> {
        Ok(Some(match self.curr.tt {
            TokenType::Skip     => Stmt::Skip(self.parse_skip()?),
            TokenType::Read     => Stmt::Read(self.parse_read()?),
            TokenType::Write    => Stmt::Write(self.parse_write()?),
            TokenType::If       => Stmt::If(self.parse_if()?),
            TokenType::While    => Stmt::While(self.parse_while()?),
            TokenType::Ident(_) => Stmt::Assign(self.parse_assign()?),
            TokenType::Semi     => { self.bump(); return Ok(None) }, // Empty stmt
            _ => {
                self.error_count += 1;
                return Err(Error::UnexpectedToken(self.curr.clone()))
            },
        }))
    }

    fn semi_or_end(&mut self) -> Result<()> {
        match self.curr.tt {
            TokenType::Semi => self.bump(),
            TokenType::Else | TokenType::Fi | TokenType::Od | TokenType::EOF => {},
            _ => {
                self.error_count += 1;
                return Err(Error::UnexpectedToken(self.curr.clone()))
            }
        }
        Ok(())
    }

    fn parse_skip(&mut self) -> Result<SkipStmt> {
        let start = self.curr_span_start();

        expect!(self, TokenType::Skip)?;
        self.semi_or_end()?;

        Ok(SkipStmt {
            span: self.span_since(start)
        })
    }

    fn parse_read(&mut self) -> Result<ReadStmt> {
        let start = self.curr_span_start();

        expect!(self, TokenType::Read)?;
        let ident_list = self.parse_ident_list()?;
        self.semi_or_end()?;

        Ok(ReadStmt {
            span: self.span_since(start),
            var_list: ident_list
        })
    }

    fn parse_write(&mut self) -> Result<WriteStmt> {
        let start = self.curr_span_start();

        expect!(self, TokenType::Write)?;
        let ident_list = self.parse_ident_list()?;
        self.semi_or_end()?;

        Ok(WriteStmt {
            span: self.span_since(start),
            var_list: ident_list
        })
    }

    fn parse_if(&mut self) -> Result<IfStmt> {
        let start = self.curr_span_start();

        expect!(self, TokenType::If)?;

        let check = self.parse_expr()?;

        expect!(self, TokenType::Then)?;

        let branch_true = self.parse_stmt_list(|tok| !check_tok!(tok, TokenType::Else | TokenType::Fi));

        let branch_false = if check_tok!(self.curr, TokenType::Else) {
            self.bump();
            Some(self.parse_stmt_list(|tok| !check_tok!(tok, TokenType::Fi)))
        } else { None };

        expect!(self, TokenType::Fi)?;

        Ok(IfStmt {
            span: self.span_since(start),
            check, branch_true, branch_false
        })
    }

    fn parse_while(&mut self) -> Result<WhileStmt> {
        let start = self.curr_span_start();

        expect!(self, TokenType::While)?;
        let check = self.parse_expr()?;
        expect!(self, TokenType::Do)?;
        let branch_true = self.parse_stmt_list(|tok| !check_tok!(tok, TokenType::Od));
        expect!(self, TokenType::Od)?;

        Ok(WhileStmt {
            span: self.span_since(start),
            check,
            branch_true
        })
    }

    fn parse_assign(&mut self) -> Result<AssignStmt> {
        let start = self.curr_span_start();

        let id = self.parse_ident()?;
        expect!(self, TokenType::Assign)?;
        let expr = self.parse_expr()?;
        self.semi_or_end()?;

        Ok(AssignStmt {
            span: self.span_since(start),
            id,
            expr
        })
    }

    fn parse_expr(&mut self)  -> Result<Box<Expr>> {
        self.parse_expr_inner(1)
    }

    fn parse_expr_inner(&mut self, prec: usize) -> Result<Box<Expr>> {
        let start = self.curr_span_start();

        // Parse left side of expression
        let mut lhs = self.parse_atom(prec)?;

        while self.is_bin_op() {
            let info = self.get_opinfo();
            if (info.prec as usize) < prec {
                break;
            }
            self.bump();

            // Parse right side of expression
            let prec = if info.assoc == OpAssoc::Left { info.prec as usize + 1 } else { info.prec as usize };
            let rhs = self.parse_expr_inner(prec)?;

            // Set LHS to complete expression
            lhs = Box::new(Expr::BinaryOp(BinaryOpExpr {
                span: self.span_since(start),
                op: info.op,
                lhs,
                rhs,
            }));
        }

        Ok(lhs)
    }

    fn parse_atom(&mut self, prec: usize) -> Result<Box<Expr>> {
        let start = self.curr_span_start();

        // Parse prefix operator
        if self.is_prefix_op() {
            let info = self.get_opinfo();
            if (info.prec as usize) >= prec {
                self.bump();

                // Parse rest of atom
                let prec = if info.assoc == OpAssoc::Left { info.prec as usize + 1 } else { info.prec as usize };
                let atom = self.parse_expr_inner(prec)?;

                return Ok(Box::new(Expr::UnaryOp(UnaryOpExpr {
                    span: self.span_since(start),
                    op: info.op,
                    expr: atom,
                })));
            }
        }

        let expr = match &self.curr.tt {
            TokenType::Ident(name) => {
                let expr = Expr::Var(Ident {
                    span: self.curr.span,
                    name: name.to_owned()
                });

                self.bump();
                Box::new(expr)
            },
            TokenType::Integer(val) => {
                let expr = Expr::Lit(Literal {
                    span: self.curr.span,
                    value: Value::Integer(*val)
                } );

                self.bump();
                Box::new(expr)
            }
            TokenType::Boolean(val) => {
                let expr = Expr::Lit(Literal {
                    span: self.curr.span,
                    value: Value::Boolean(*val)
                } );

                self.bump();
                Box::new(expr)
            }
            TokenType::LParen => {
                self.bump();
                let expr = self.parse_expr()?;
                expect!(self, TokenType::RParen)?;
                expr
            }
            _ => {
                self.error_count += 1;
                return Err(Error::SyntaxError(self.curr.span, "expected expression".to_string()))
            },
        };

        Ok(expr)
    }

    fn parse_ident_list(&mut self) -> Result<IdentList> {
        let start = self.curr_span_start();

        let mut items = vec![];
        loop {
            let id = self.parse_ident()?;
            items.push(id);

            if !check_tok!(self.curr, TokenType::Comma) {
                break;
            }

            self.bump();
        }

        Ok(IdentList {
            span: self.span_since(start),
            items,
        })
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let tok = expect!(self, TokenType::Ident(_))?;
        if let TokenType::Ident(name) = tok.tt {
            return Ok(Ident {
                span: tok.span,
                name
            });
        }
        unreachable!()
    }

    fn get_opinfo(&self) -> OpInfo {
        match self.curr.tt {
            TokenType::Neg    => OpInfo { op: Op::Neg, prec: 12, assoc: OpAssoc::Right },
            TokenType::Star   => OpInfo { op: Op::Mul, prec: 10, assoc: OpAssoc::Left },
            TokenType::Slash  => OpInfo { op: Op::Div, prec: 10, assoc: OpAssoc::Left },
            TokenType::Plus   => OpInfo { op: Op::Plus, prec: 9, assoc: OpAssoc::Left },
            TokenType::Minus  => OpInfo { op: Op::Minus, prec: 9, assoc: OpAssoc::Left },
            TokenType::Less   => OpInfo { op: Op::Less, prec: 8, assoc: OpAssoc::Left },
            TokenType::LessEq => OpInfo { op: Op::LessEq, prec: 8, assoc: OpAssoc::Left },
            TokenType::More   => OpInfo { op: Op::More, prec: 8, assoc: OpAssoc::Left },
            TokenType::MoreEq => OpInfo { op: Op::MoreEq, prec: 8, assoc: OpAssoc::Left },
            TokenType::Eq     => OpInfo { op: Op::Eq, prec: 7, assoc: OpAssoc::Left },
            TokenType::Neq    => OpInfo { op: Op::Neq, prec: 7, assoc: OpAssoc::Left },
            TokenType::And    => OpInfo { op: Op::And, prec: 3, assoc: OpAssoc::Left },
            TokenType::Or     => OpInfo { op: Op::Or, prec: 2, assoc: OpAssoc::Left },
            _ => panic!("token is not an operator!"),
        }
    }

    fn is_prefix_op(&self) -> bool {
        check_tok!(self.curr, TokenType::Neg)
    }

    fn is_bin_op(&self) -> bool {
        check_tok!(self.curr,
            TokenType::Eq | TokenType::Neq |
            TokenType::Star | TokenType::Slash |
            TokenType::Plus | TokenType::Minus |
            TokenType::Less | TokenType::LessEq |
            TokenType::More | TokenType::MoreEq |
            TokenType::And | TokenType::Or)
    }

    fn recover(&mut self) {
        static RECOVERY_CHECKPOINT: [TokenType; 5] = [
            TokenType::Skip,
            TokenType::Read,
            TokenType::Write,
            TokenType::If,
            TokenType::While,
        ];

        while !RECOVERY_CHECKPOINT.contains(&self.curr.tt) && self.idx < self.ts.len() {
            self.bump();
        }
    }

    fn bump(&mut self) {
        if self.idx < self.ts.len() {
            self.idx += 1;
        }

        std::mem::swap(&mut self.prev, &mut self.curr);
        std::mem::swap(&mut self.curr, &mut self.next);

        self.next = self.ts.get(self.idx)
            .map(|x| x.clone())
            .unwrap_or_else(|| {
                let end_span = Span::new(self.curr.span.hi, self.curr.span.hi);
                Token::new(TokenType::EOF, end_span)
            });
    }

    fn curr_span_start(&self) -> FilePos {
        self.curr.span.lo
    }

    fn span_since(&self, lo: FilePos) -> Span {
        self.prev.span.hi - lo
    }

    pub fn has_errors(&self) -> bool {
        self.error_count != 0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum OpAssoc { Left, Right }

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct OpInfo {
    op: Op,
    prec: u8,
    assoc: OpAssoc,
}

pub use result::*;

pub mod result {
    use std::fmt::{Display, Formatter};
    use crate::parse::{Span, Token};

    pub type Result<T> = std::result::Result<T, Error>;

    pub enum Error {
        SyntaxError(Span, String),
        UnexpectedToken(Token),
    }

    impl Display for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Error::SyntaxError(span, msg) => write!(f, "{}: syntax error; {}", span, msg),
                Error::UnexpectedToken(tk) => write!(f, "{}: unexpected token: {}", tk.span, tk.tt),
            }
        }
    }
}
