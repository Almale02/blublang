package compile.code_analysis.ast_analizer

import compile.code_analysis.AstAnalyzer
import compile.code_analysis.CodeAnalyzer
import compilerError

class ValidateBlockLevelStmt: AstAnalyzer {
    override fun analyse(codeAnalyzer: CodeAnalyzer) {
        for (block in AstBlocks.blocks) {
            for (stmt in block) {
                if (!StmtTypes.blockLevelStmts.contains(stmt.id))
                    compilerError("expected top level stmt, but found: ${stmt.id.string_repr}")
            }
        }
    }

}