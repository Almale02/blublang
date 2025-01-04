package compile.code_analysis.ast_analizer

import compile.code_analysis.AstAnalyzer
import compile.code_analysis.CodeAnalyzer
import compile.parser.Stmt
import compilerError

object AstDecls {
    var functions: Array<Stmt.FunctionDecl> = arrayOf()
    var structs: Array<Stmt.StructDecl> = arrayOf()
    var structHashmap: HashMap<String, Int> = hashMapOf()
}

class GetDecls: AstAnalyzer {
    override fun analyse(codeAnalyzer: CodeAnalyzer) {
        val functions = arrayListOf<Stmt.FunctionDecl>()
        val structs = arrayListOf<Stmt.StructDecl>()

        for (stmt in codeAnalyzer.ast) {
            when (stmt) {
                is Stmt.FunctionDecl -> {
                    functions.add(stmt)
                }
                is Stmt.StructDecl -> {
                    if (AstDecls.structHashmap.contains(stmt.name))
                        compilerError("struct with name ${stmt.name} is already defined")
                    structs.add(stmt)
                    AstDecls.structHashmap[stmt.name] = structs.lastIndex
                }
                else -> {}
            }
        }
        AstDecls.functions = functions.toTypedArray()
        AstDecls.structs = structs.toTypedArray()
    }
}