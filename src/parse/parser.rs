use crate::ast::{AssignStmt, Root, Expr, Ident, IdentList, IfStmt, Literal, Op, ReadStmt, Stmt, StmtList, WhileStmt, WriteStmt};
use crate::parse::{Token, TokenStream, TokenType};

pub struct Parser {
    ts: TokenStream,
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
        let mut list = StmtList::new();
        while check(&self.curr) {
            if let Some(stmt) = self.try_parse_stmt() {
                list.push(stmt);
            }
        }
        list
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
            TokenType::Skip     => { self.parse_skip()?; Stmt::Skip },
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
            TokenType::Else | TokenType::Fi | TokenType::Od | TokenType::Invalid => {},
            _ => {
                self.error_count += 1;
                return Err(Error::UnexpectedToken(self.curr.clone()))
            }
        }
        Ok(())
    }

    fn parse_skip(&mut self) -> Result<()> {
        expect!(self, TokenType::Skip)?;
        self.semi_or_end()?;
        Ok(())
    }

    fn parse_read(&mut self) -> Result<ReadStmt> {
        expect!(self, TokenType::Read)?;
        let ident_list = self.parse_ident_list()?;
        self.semi_or_end()?;
        Ok(ReadStmt { var_list: ident_list })
    }

    fn parse_write(&mut self) -> Result<WriteStmt> {
        expect!(self, TokenType::Write)?;
        let ident_list = self.parse_ident_list()?;
        self.semi_or_end()?;
        Ok(WriteStmt { var_list: ident_list })
    }

    fn parse_if(&mut self) -> Result<IfStmt> {
        expect!(self, TokenType::If)?;

        let check = self.parse_expr()?;

        expect!(self, TokenType::Then)?;

        let branch_true = self.parse_stmt_list(|tok| !check_tok!(tok, TokenType::Else | TokenType::Fi));

        let branch_false = if check_tok!(self.curr, TokenType::Else) {
            self.bump();
            let stmt_list = self.parse_stmt_list(|tok| !check_tok!(tok, TokenType::Fi));
            Some(stmt_list)
        } else { None };

        expect!(self, TokenType::Fi)?;

        Ok(IfStmt { check, branch_true, branch_false })
    }

    fn parse_while(&mut self) -> Result<WhileStmt> {
        expect!(self, TokenType::While)?;
        let check = self.parse_expr()?;
        expect!(self, TokenType::Do)?;
        let branch_true = self.parse_stmt_list(|tok| !check_tok!(tok, TokenType::Od));
        expect!(self, TokenType::Od)?;
        Ok(WhileStmt { check, branch_true })
    }

    fn parse_assign(&mut self) -> Result<AssignStmt> {
        let id = self.parse_ident()?;
        expect!(self, TokenType::Assign)?;
        let expr = self.parse_expr()?;
        self.semi_or_end()?;
        Ok(AssignStmt { id, expr })
    }

    fn parse_expr(&mut self)  -> Result<Box<Expr>> {
        self.parse_expr_inner(1)
    }

    fn parse_expr_inner(&mut self, prec: usize) -> Result<Box<Expr>> {
        // Parse left side of expression
        let mut lhs = self.parse_atom(prec)?;

        while self.is_bin_op() {
            let op = self.get_opinfo();
            if (op.prec as usize) < prec {
                break;
            }
            self.bump();

            // Parse right side of expression
            let new_prec = if op.assoc == OpAssoc::Left { op.prec as usize + 1 } else { op.prec as usize };
            let rhs = self.parse_expr_inner(new_prec)?;

            // Set LHS to complete expression
            lhs = Box::new(Expr::BinaryOp(op.op, lhs, rhs));
        }

        Ok(lhs)
    }

    fn parse_atom(&mut self, prec: usize) -> Result<Box<Expr>> {
        // Parse prefix operator
        if self.is_prefix_op() {
            let op = self.get_opinfo();
            if (op.prec as usize) >= prec {
                self.bump();

                // Parse rest of atom
                let new_prec = if op.assoc == OpAssoc::Left { op.prec as usize + 1 } else { op.prec as usize };
                let atom = self.parse_expr_inner(new_prec)?;

                return Ok(Box::new(Expr::UnaryOp(op.op, atom)));
            }
        }

        let expr = match &self.curr.tt {
            TokenType::Ident(name) => {
                let expr = Expr::Var(Ident { name: name.to_owned() });
                self.bump();
                Box::new(expr)
            },
            TokenType::Integer(val) => {
                let expr = Expr::Lit(Literal::Integer(val.clone()));
                self.bump();
                Box::new(expr)
            }
            TokenType::Boolean(val) => {
                let expr = Expr::Lit(Literal::Boolean(val.clone()));
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
        let mut list = IdentList::new();
        loop {
            let id = self.parse_ident()?;
            list.push(id);

            if !check_tok!(self.curr, TokenType::Comma) {
                break;
            }

            self.bump();
        }
        Ok(list)
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let tok = expect!(self, TokenType::Ident(_))?;
        if let TokenType::Ident(name) = tok.tt {
            return Ok(Ident { name });
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

        std::mem::swap(&mut self.curr, &mut self.next);

        self.next = self.ts.get(self.idx)
            .map(|x| x.clone())
            .unwrap_or_default();
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
