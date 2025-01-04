package compile.code_analysis.ast_analizer

import compile.code_analysis.AstAnalyzer
import compile.code_analysis.CodeAnalyzer
import compile.parser.StmtId
import compilerError

object StmtTypes {
    val topLevelStmts: HashSet<StmtId> = hashSetOf(
        StmtId.StructDecl,
        StmtId.FunctionDecl,
    )
    val blockLevelStmts: HashSet<StmtId> = hashSetOf(
        StmtId.For,
        StmtId.If,
        StmtId.VarDecl,
        StmtId.Expr
    )
}

class ValidateTopLevelStmts: AstAnalyzer {
    override fun analyse(codeAnalyzer: CodeAnalyzer) {
        with(codeAnalyzer) {
            for (stmt in ast) {
                if (!StmtTypes.topLevelStmts.contains(stmt.id))
                    compilerError("expected top level stmt, but found: ${stmt.id.string_repr}")
            }
        }
    }
}