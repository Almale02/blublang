package compile.code_analysis.code_scope

import compile.lexer.Token
import compile.types.TypeHandle

sealed class AnalysisExpr() {
    data class Number(val handle: CodeExprHandle): AnalysisExpr()
    object String: AnalysisExpr()
    data class Ident(val base: CodeExprHandle, val access: Array<kotlin.String>): AnalysisExpr()
    data class Call(val base: CodeExprHandle, val args: Array<CodeExprHandle>): AnalysisExpr()
    data class Binary(val lhs: CodeExprHandle, val op: Token, val rhs: CodeExprHandle): AnalysisExpr()
    data class FnArg(val name: kotlin.String, val argType: TypeHandle): AnalysisExpr()
    data class CaptureVar(val isMut: Boolean, val varType: TypeHandle, val name: kotlin.String): AnalysisExpr()
}
