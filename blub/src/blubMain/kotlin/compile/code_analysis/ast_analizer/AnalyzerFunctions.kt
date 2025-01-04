package compile.code_analysis.ast_analizer

import compile.code_analysis.AstAnalyzer
import compile.code_analysis.CodeAnalyzer
import compile.types.TypeInfo
import compile.types.TypeRegistry

class AnalyzerFunctions: AstAnalyzer {
    override fun analyse(codeAnalyzer: CodeAnalyzer) {
        for (func in AstDecls.functions) {
            val handle = TypeRegistry.addType(TypeInfo.Fn(
                func.returnType?.let { TypeRegistry.resolveTypeHandleFromAstType(it).handle } ?: TypeRegistry.typeInfoToHandle[TypeInfo.Unit]!!,
                func.args.map { TypeRegistry.resolveTypeHandleFromAstType(it.type).handle }.toTypedArray()
            ))
            TypeRegistry.functionNameToHandle[func.name] = handle
        }
    }
}