package edu.colorado.csci3155.mython

sealed trait Expr
case class ConstNum(f: Double) extends Expr
case class Ident(s: String) extends Expr
case class Plus(e1: Expr, e2: Expr ) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr


sealed trait CondExpr
case object ConstTrue extends CondExpr
case object ConstFalse extends CondExpr
case class Geq(e1: Expr, e2: Expr) extends CondExpr
case class Leq(e1: Expr, e2: Expr) extends CondExpr
case class Eq(e1: Expr, e2: Expr) extends CondExpr
case class And(c1: CondExpr, c2: CondExpr) extends CondExpr
case class Or(c1: CondExpr, c2: CondExpr) extends CondExpr
case class Not(c: CondExpr) extends CondExpr

sealed trait Declaration
sealed trait Statement

case class VarDecl(identifier: String, rhsExpr: Expr) extends Declaration
case class AssignStmt(identifier: String, rhsExpr: Expr) extends Statement
case class WhileStmt(cond: CondExpr, stmts: List[Statement]) extends Statement
case class IfThenElseStmt(cond: CondExpr, stmtsThen: List[Statement], stmtsElse: List[Statement]) extends Statement
case class ReturnStmt(retExpr: Expr) extends Statement

case class Program(decls: List[Declaration], stmts: List[Statement])