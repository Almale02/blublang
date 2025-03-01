package compile.parser

import compile.lexer.Token

sealed class Expr(val id: ExprId) {
    data class Number(val number: Token.Number): Expr(ExprId.Number)
    data class String(val string: kotlin.String): Expr(ExprId.String)
    data class Ident(val ident: Array<kotlin.String>): Expr(ExprId.Ident)
    data class Call(val base: Expr, val args: Array<Expr>): Expr(ExprId.Call)
    data class Binary(val left: Expr, val op: Token, val right: Expr): Expr(ExprId.Binary)
    data class StructCreate(val struct: Expr.Ident, val fields: Array<Pair<kotlin.String, Expr>>): Expr(ExprId.StructCreate)

    override fun toString(): kotlin.String {
        return when (this) {
            is Number -> "number literal"
            is String -> "string literal"
            is Ident -> "identifier"
            is Call -> "call expression"
            is Binary -> "binary expression"
            is StructCreate -> "struct create expression"
        }
    }
}
enum class ExprId {
    Number,
    String,
    Ident,
    Call,
    Binary,
    StructCreate
}
sealed class Stmt(val id: StmtId) {
    data class VarDecl(val name: String, val isMut: Boolean, val initValue: Expr?): Stmt(StmtId.VarDecl)
    data class If(val guard: Expr, val body: Array<Stmt>): Stmt(StmtId.If)
    data class For(val captureExpr: Expr, val iteratorExpr: Expr, val body: Array<Stmt>): Stmt(StmtId.For)
    data class FunctionDecl(
        val isExtern: Boolean,
        val isPub: Boolean,
        val name: String,
        val args: Array<ArgsDecl>,
        val body: Array<Stmt>,
        val returnType: AstType?
    ): Stmt(StmtId.FunctionDecl)
    data class StructDecl(
        val isExtern: Boolean,
        val isPub: Boolean,
        val name: String,
        val fields: Array<AstStructField>
    ): Stmt(StmtId.StructDecl)
    data class ExprStmt(val expr: Expr): Stmt(StmtId.Expr)
}
enum class StmtId(val string_repr: String) {
    VarDecl("var decl stmt"),
    If("if stmt"),
    For("for stmt"),
    FunctionDecl("function decl stmt"),
    StructDecl("struct decl stmt"),
    Expr("expr stmt")
}
data class ArgsDecl(val name: String, val type: AstType)
data class AstStructField(
    val isPub: Boolean,
    val name: String,
    val type: AstType,
    val defaultValue: Expr?,
)
sealed class AstType(val id: AstTypeId) {
    data class Symbol(val symbol: String): AstType(AstTypeId.Symbol)
    data class Array(val underlying: AstType): AstType(AstTypeId.Array)
    data class Pointer(val isMut: Boolean, val pointeeType: AstType): AstType(AstTypeId.Pointer)
}
enum class AstTypeId {
   Symbol,
   Array,
   Pointer,
}