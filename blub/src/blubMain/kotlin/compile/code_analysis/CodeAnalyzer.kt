package compile.code_analysis

import compile.parser.Stmt

class CodeAnalyzer(val ast: Array<Stmt>) {
    val astAnalyzers: ArrayList<AstAnalyzer> = arrayListOf()

    fun analyse() {
        for (analyzer in astAnalyzers) {
            analyzer.analyse(this)
        }
    }
    infix fun add(analyzer: AstAnalyzer) {
        astAnalyzers.add(analyzer)
    }
}
interface AstAnalyzer {
    fun analyse(codeAnalyzer: CodeAnalyzer)
}
