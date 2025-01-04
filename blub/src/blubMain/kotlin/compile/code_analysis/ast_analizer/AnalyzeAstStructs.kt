package compile.code_analysis.ast_analizer

import compile.code_analysis.AstAnalyzer
import compile.code_analysis.CodeAnalyzer
import compile.parser.Stmt
import compile.types.StructField
import compile.types.TypeInfo
import compile.types.TypeRegistry
import compilerError

class AnalyzeAstStructs: AstAnalyzer {
    override fun analyse(codeAnalyzer: CodeAnalyzer) {
        defineStructs(AstDecls.structs)
    }
}
private fun defineStructs(structs: Array<Stmt.StructDecl>) {
    for (structStmt in structs) {
        val structHandle = TypeRegistry.structNameToHandle[structStmt.name]
        if (structHandle != null) {
           if (TypeRegistry.isTypeDefined(structHandle)) {
               continue
           }
        }
        val typesToDefine = defineStructType(structStmt)

        for (t in typesToDefine) {
            if (!AstDecls.structHashmap.contains(t)) {
                compilerError("expected struct $t to be defined")
            }
        }
        defineStructs(typesToDefine.map { AstDecls.structs[AstDecls.structHashmap[it]!!] }.toTypedArray())
    }
}
private fun defineStructType(structStmt: Stmt.StructDecl): Array<String> {
    val typesToBeDefined = arrayListOf<String>()

    val structType = TypeInfo.Struct(structStmt.name, {
       val fields = arrayListOf<StructField>()
       structStmt.fields.forEach {
           val fieldTypeResolve = TypeRegistry.resolveTypeHandleFromAstType(it.type)
           fields.add(StructField(it.name, fieldTypeResolve.handle))
           fieldTypeResolve.typeIsUndefined?.let { typeName ->
               typesToBeDefined.add(typeName)
           }
       }
       fields.toTypedArray()
    }())
    TypeRegistry.structNameToHandle[structStmt.name]?.let {
        TypeRegistry.defineType(it, structType)
        return typesToBeDefined.toTypedArray()
    }

    TypeRegistry.addType(structType)
    return typesToBeDefined.toTypedArray()
}