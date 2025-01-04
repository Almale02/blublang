package compile.code_analysis.code_scope

import compile.parser.Stmt
import compile.types.TypeHandle
import compile.types.TypeInfo

sealed class AnalysisStmt(val astStmt: Stmt) {
    data class VarDecl(val stmt: Stmt.VarDecl, val initValue: CodeExprHandle): AnalysisStmt(stmt)
    data class If(val stmt: Stmt.If, val scopeHandle: CodeScopeHandle, val guard: CodeExprHandle): AnalysisStmt(stmt)
    data class For(val stmt: Stmt.For, val scopeHandle: CodeScopeHandle, val captureVars: Array<String>, val iterExprHandle: CodeExprHandle): AnalysisStmt(stmt)
    data class FunctionDecl(val stmt: Stmt.FunctionDecl, val funcInfo: TypeInfo.Fn, val scopeHandle: CodeScopeHandle): AnalysisStmt(stmt)
    data class StructDecl(val stmt: Stmt.StructDecl, val fields: Array<AnalysisStructField>): AnalysisStmt(stmt)
    data class ExprStmt(val stmt: Stmt.ExprStmt, val expr: CodeExprHandle): AnalysisStmt(stmt)
}
data class AnalysisStructField(val name: String, val typeHandle: TypeHandle, val initValue: CodeExprHandle?)

