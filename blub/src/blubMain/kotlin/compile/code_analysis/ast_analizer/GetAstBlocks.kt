package compile.code_analysis.ast_analizer

import compile.code_analysis.AstAnalyzer
import compile.code_analysis.CodeAnalyzer
import compile.parser.Stmt
import compilerError

object AstBlocks {
    var blocks: Array<Array<Stmt>> = arrayOf()
}

class GetAstBlocks: AstAnalyzer {
    override fun analyse(codeAnalyzer: CodeAnalyzer) {
        val blocks = arrayListOf<Array<Stmt>>()
        for (rootStmt in codeAnalyzer.ast) {
            when (rootStmt) {
                is Stmt.FunctionDecl -> {
                    blocks.add(rootStmt.body)
                    getBlocksRecursive(rootStmt.body, blocks)
                }
                is Stmt.StructDecl -> {
                    continue
                }
                else -> {
                    compilerError("unreachable")
                }
            }
        }
        AstBlocks.blocks = blocks.toTypedArray()
    }
}
private fun getBlocksRecursive(block: Array<Stmt>, blocks: ArrayList<Array<Stmt>>) {
    for (stmt in block) {
        when (stmt) {
            is Stmt.If -> {
                blocks.add(stmt.body)
                getBlocksRecursive(stmt.body, blocks)
            }
            is Stmt.For -> {
                blocks.add(stmt.body)
                getBlocksRecursive(stmt.body, blocks)
            }
            else -> {}
        }
    }
}