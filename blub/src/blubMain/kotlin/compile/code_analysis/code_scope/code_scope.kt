
package compile.code_analysis.code_scope

import arrow.core.left
import compile.parser.Expr
import compile.parser.Stmt
import compile.types.NumberTypes
import compile.types.TypeHandle
import compile.types.TypeInfo
import compile.types.TypeRegistry
import compilerError
import compilerICE

data class CodeExprHandle(val id: ULong)
data class CodeSubExprHandle(val id: ULong)

@Suppress("NAME_SHADOWING")
class CodeScope(val id: Int, val parent: CodeScope?) {
    val stmts = arrayListOf<AnalysisStmt>()
    val varNameToExprHandle = hashMapOf<String, CodeExprHandle>()

    fun addScope(): CodeScope{
        return CodeScopeParser.newScope(this)
    }
    infix fun parseCodeBlock(block: Array<Stmt>) {
        block.forEach(::parseAstStmt)
    }
    infix fun getVarExprHandle(varName: String): Pair<CodeExprHandle, CodeScope>? {
        for (parent in CodeScopeParser.parentsOfScope[this]!!) {
            parent.varNameToExprHandle[varName]?.let {
                return Pair(it, parent)
            }
        }
        varNameToExprHandle[varName]?.let {
            return Pair(it, this)
        }
        return null
    }
    infix fun getVarTypeHandle(varName: String) = getVarExprHandle(varName)?.let {
        Pair(CodeScopeParser getTypeHandle it.first, it.second)
    }
    infix fun parseAstExpr(expr: Expr): CodeExprHandle {
        return when (expr) {
            is Expr.Number -> {
                val exprHandle = CodeScopeParser.newExprHandle(this)
                val analysisExpr = AnalysisExpr.Number(exprHandle)
                val i32TypeHandle = TypeRegistry getTypeHandle TypeInfo.Number(NumberTypes.I32)

                CodeScopeParser.exprTypeMap[exprHandle] = i32TypeHandle
                CodeScopeParser.exprToAnalysisExpr[exprHandle] = analysisExpr

                exprHandle

            }
            is Expr.String -> {
                val exprHandle = CodeScopeParser.newExprHandle(this)
                val analysisExpr = AnalysisExpr.String
                val typeHandle = TypeRegistry getTypeHandle TypeInfo.Str

                CodeScopeParser.exprTypeMap[exprHandle] = typeHandle
                CodeScopeParser.exprToAnalysisExpr[exprHandle] = analysisExpr

                exprHandle
            }
            is Expr.Ident -> {
                val baseName = expr.ident.first()

                var baseSubExprHandle: CodeSubExprHandle? = null
                val exprHandle = CodeScopeParser.newExprHandle(this)

                val varType = getVarTypeHandle(baseName)
                if (varType != null)  {
                    CodeScopeParser.exprToAnalysisExpr[exprHandle] = AnalysisExpr.Ident(
                        getVarExprHandle(baseName)?.first ?: compilerICE("variable $baseName didnt have expr handle"),
                        expr.ident
                    )
                    baseSubExprHandle = CodeScopeParser.toSubExpr(this, getVarExprHandle(baseName)!!.first)

                } else {
                    TypeRegistry.functionNameToHandle[baseName]?.let { funcType ->
                        if (expr.ident.size > 1)
                            compilerError("you cannot access fields on functions")

                        CodeScopeParser.exprTypeMap[exprHandle] = funcType
                        CodeScopeParser.exprToAnalysisExpr[exprHandle] = AnalysisExpr.Ident(exprHandle, arrayOf())
                        return exprHandle
                    } ?: run {
                        TypeRegistry.structNameToHandle[baseName]?.let {
                            val subExprHandle = CodeScopeParser.newSubExprHandle(this)
                            CodeScopeParser.subExprToStaticType[subExprHandle] = it
                            CodeScopeParser.subExprTypeMap[subExprHandle] = TypeRegistry getTypeHandle TypeInfo.StaticType
                            baseSubExprHandle = subExprHandle
                        } ?: run {
                            compilerError("expected variable, function, or struct '$baseName' to be defined")
                        }
                    }
                }
                val typeHandle = parseIdentAccess(exprHandle, this, baseSubExprHandle ?: compilerICE("base sub expr handle was null"), expr.ident)
                CodeScopeParser.exprTypeMap[exprHandle] = typeHandle

                exprHandle
            }
            is Expr.Call -> {
                val baseExprHandle = this parseAstExpr expr.base
                val baseType = CodeScopeParser getTypeHandle baseExprHandle
                val typeInfo = TypeRegistry getTypeInfo baseType
                if (typeInfo is TypeInfo.Fn) {

                    if (typeInfo.args.size != expr.args.size)
                        compilerError("function ${typeInfo.display()} expected ${typeInfo.args.size} but got: ${expr.args.size}")
                    val args = expr.args.withIndex().map { (i, argExpr) ->
                        val argExprHandle = this parseAstExpr argExpr
                        val argExprType = CodeScopeParser getTypeHandle argExprHandle
                        val argTypeInfo = TypeRegistry getTypeInfo argExprType

                        val expectedType = typeInfo.args[i]

                        if (expectedType != argExprType)
                            compilerError(
                                "expected type ${expectedType.display()}, but found type: ${argExprType.display()}" +
                                        " at function call ${typeInfo.display()} at arg $i"
                            )

                        argExprHandle
                    }.toTypedArray()

                    val exprHandle = CodeScopeParser.newExprHandle(this)
                    CodeScopeParser.exprTypeMap[exprHandle] = typeInfo.returnTypeHandle
                    CodeScopeParser.exprToAnalysisExpr[exprHandle] = AnalysisExpr.Call(baseExprHandle, args)

                    return exprHandle
                }
                compilerError("expected function to be called or struct to be created")

            }
            is Expr.Binary -> {
                val leftExprHandle = this parseAstExpr expr.left
                val rightExprHandle = this parseAstExpr expr.right

                val leftTypeHandle = CodeScopeParser getTypeHandle leftExprHandle
                val rightTypeHandle = CodeScopeParser getTypeHandle rightExprHandle

                val leftTypeInfo = TypeRegistry getTypeInfo leftTypeHandle
                val rightTypeInfo = TypeRegistry getTypeInfo rightTypeHandle

                if (leftTypeHandle != rightTypeHandle)
                    compilerError("binary expr expects all sides to be the same type, but left is: ${leftTypeInfo.display()}, and right is: ${rightTypeInfo.display()}")
                val exprHandle = CodeScopeParser.newExprHandle(this)

                CodeScopeParser.exprTypeMap[exprHandle] = leftTypeHandle
                CodeScopeParser.exprToAnalysisExpr[exprHandle] = AnalysisExpr.Binary(leftExprHandle, expr.op, rightExprHandle)

                exprHandle
            }
            is Expr.StructCreate -> {
                val structNameExprHandle = this parseAstExpr expr.struct
                val structNameTypeHandle = CodeScopeParser getTypeHandle structNameExprHandle
                val structNameTypeInfo = TypeRegistry getTypeInfo structNameTypeHandle

                if (structNameTypeInfo !is TypeInfo.StaticType)
                    compilerError("struct creates requires a struct to create which has a type of StaticType")
                val structInfo = (TypeRegistry getTypeInfo (CodeScopeParser getStaticType structNameExprHandle)) as TypeInfo.Struct

                if (structInfo.fields.size != expr.fields.size)
                    compilerError("not all fields are inited")
                val initedFields = hashSetOf<String>()
                val fields = arrayListOf<Pair<String, CodeExprHandle>>()
                for (field in expr.fields) {
                    if (initedFields.contains(field.first)) {
                        compilerError("field ${field.first} has already been inited")
                    }
                    initedFields.add(field.first)
                    val fieldValue = this parseAstExpr field.second
                    val fieldTypeHandle = CodeScopeParser getTypeHandle fieldValue
                    if (fieldTypeHandle !=
                        (structInfo.fields.firstOrNull { it.name == field.first }
                            ?: compilerError("field ${field.first} is not on struct ${structInfo.display()}")).typeHandle) {
                        compilerError("field with name: ${field.first} has type ${fieldTypeHandle.display()}, but expected: ${structInfo.fields.firstOrNull {it.name == field.first}!!.typeHandle.display()}")
                    }
                    fields.add(Pair(field.first, fieldValue))
                }
                val analysisExpr = CodeScopeParser.newExprHandle(this)
                val typeOfStaticType = CodeScopeParser getStaticType structNameExprHandle
                CodeScopeParser.exprTypeMap[analysisExpr] = typeOfStaticType
                CodeScopeParser.exprToAnalysisExpr[analysisExpr] = AnalysisExpr.StructCreate(structNameExprHandle, fields.toTypedArray())

                analysisExpr
            }
        }
    }
    infix fun parseAstStmt(stmt: Stmt) {
        when (stmt) {
            is Stmt.VarDecl -> {
                if (getVarTypeHandle(stmt.name) != null)
                    compilerError("redefinition of variable ${stmt.name}")
                if (stmt.initValue == null)
                    compilerError("init values expected for variables")

                val initExprHandle = this parseAstExpr stmt.initValue
                varNameToExprHandle[stmt.name] = initExprHandle
                stmts.add(AnalysisStmt.VarDecl(stmt, initExprHandle))
            }
            is Stmt.If -> {
                val guardExprHandle = this parseAstExpr stmt.guard
                val newScope = addScope()

                newScope.parseCodeBlock(stmt.body)

                stmts.add(AnalysisStmt.If(stmt, newScope, guardExprHandle))
            }
            is Stmt.For -> {
                val iterExprHandle = this parseAstExpr stmt.iteratorExpr
                val iterType = CodeScopeParser getTypeHandle iterExprHandle
                val iterTypeInfo = TypeRegistry getTypeInfo iterType

                val newScope = addScope()

                val captureVars = arrayListOf<String>()

                when (iterTypeInfo) {
                    is TypeInfo.Array -> {
                        val captureTypeHandle = CodeScopeParser.newExprHandle(this)
                        val captureExpr = stmt.captureExpr
                            when (captureExpr) {
                            is Expr.Ident -> {
                                if (captureExpr.ident.size != 1)
                                    compilerError("variable names cant have dont in them")
                                captureVars.add(captureExpr.ident.first())
                                newScope.varNameToExprHandle[captureExpr.ident.first()] = captureTypeHandle
                                CodeScopeParser.exprTypeMap[captureTypeHandle] = iterTypeInfo.handle
                                CodeScopeParser.exprToAnalysisExpr[captureTypeHandle] = AnalysisExpr.CaptureVar(true, iterTypeInfo.handle, captureExpr.ident.first())
                            }
                            else -> compilerError("for array iterations, the capture must be a variable name")
                        }
                    }
                    else -> compilerError("${iterTypeInfo.display()} doesnt support iteration")
                }
                newScope parseCodeBlock stmt.body
                stmts.add(AnalysisStmt.For(stmt, newScope, captureVars.toTypedArray(), iterExprHandle))
            }
            is Stmt.FunctionDecl -> {
                val newScope = addScope()
                val funcTypeHandle = TypeRegistry funcNameToHandle stmt.name
                val funcInfo = TypeRegistry getTypeInfo funcTypeHandle
                if (funcInfo !is TypeInfo.Fn)
                    compilerError("unreachable code")

                for ((i, arg) in stmt.args.withIndex()) {
                    val argType = funcInfo.args[i]
                    val argExprHandle = CodeScopeParser.newExprHandle(this)

                    newScope.varNameToExprHandle[arg.name] = argExprHandle
                    CodeScopeParser.exprTypeMap[argExprHandle] = argType
                    CodeScopeParser.exprToAnalysisExpr[argExprHandle] = AnalysisExpr.FnArg(arg.name, argType)
                }
                newScope parseCodeBlock stmt.body
                stmts.add(AnalysisStmt.FunctionDecl(stmt, funcInfo, newScope))
            }
            is Stmt.StructDecl -> {
                val structTypeHandle = TypeRegistry structNameToHandle stmt.name
                val structInfo = TypeRegistry getTypeInfo structTypeHandle
                if (structInfo !is TypeInfo.Struct)
                    compilerError("unreachable code")

                val fields = stmt.fields.withIndex().map { (i, field) ->
                    AnalysisStructField(field.name, structInfo.fields[i].typeHandle, field.defaultValue?.let { this parseAstExpr it })
                }.toTypedArray()

                stmts.add(AnalysisStmt.StructDecl(stmt, fields))
            }
            is Stmt.ExprStmt -> {
                stmts.add(AnalysisStmt.ExprStmt(stmt, this parseAstExpr stmt.expr))
            }
        }
    }

    override fun hashCode() = id

    override fun equals(other: Any?) = other?.let { other ->
        if (other!is CodeScope)
            return false
        id == other.id
    } ?: false
}

object CodeScopeParser {
    var nextExprHandle: ULong = 1u
    var nextSubExprHandle: ULong = 1u
    var nextScopeId = 1

    val exprTypeMap = hashMapOf<CodeExprHandle, TypeHandle>()
    val exprToAnalysisExpr = hashMapOf<CodeExprHandle, AnalysisExpr>()
    val exprToStaticType = hashMapOf<CodeExprHandle, TypeHandle>()
    val exprToCodeScope = hashMapOf<CodeExprHandle, CodeScope>()
    val subExprTypeMap = hashMapOf<CodeSubExprHandle, TypeHandle>()
    val subExprToStaticType = hashMapOf<CodeSubExprHandle, TypeHandle>()
    val subExprToCodeScope = hashMapOf<CodeSubExprHandle, CodeScope>()

    val parentsOfScope = hashMapOf<CodeScope, HashSet<CodeScope>>()

    fun parentsHas(scope: CodeScope, exprHandle: CodeExprHandle): Boolean {
        return parentsOfScope[scope]!!.contains(exprToCodeScope[exprHandle])
    }
    fun parentsHas(scope: CodeScope, subExprHandle: CodeSubExprHandle): Boolean {
        return parentsOfScope[scope]!!.contains(subExprToCodeScope[subExprHandle])
    }
    infix fun getTypeHandle(exprHandle: CodeExprHandle): TypeHandle {
        return exprTypeMap[exprHandle]!!
    }
    infix fun getTypeHandle(exprHandle: CodeSubExprHandle): TypeHandle {
        return subExprTypeMap[exprHandle]!!
    }
    infix fun getStaticType(exprHandle: CodeExprHandle) = exprToStaticType[exprHandle]!!
    infix fun getSubExprStaticType(exprHandle: CodeSubExprHandle) = subExprToStaticType[exprHandle]!!
    infix fun getAnalysisExpr(exprHandle: CodeExprHandle): AnalysisExpr{
        return exprToAnalysisExpr[exprHandle]!!
    }
    infix fun getScopeOf(exprHandle: CodeExprHandle) = exprToCodeScope[exprHandle]!!
    infix fun getScopeOf(subExprHandle: CodeSubExprHandle) = subExprToCodeScope[subExprHandle]!!

    fun toSubExpr(scope: CodeScope, exprHandle: CodeExprHandle): CodeSubExprHandle {
        val newHandle = newSubExprHandle(scope)
        subExprTypeMap[newHandle] = this getTypeHandle exprHandle

        exprToStaticType[exprHandle]?.let {
            subExprToStaticType[newHandle] = it
        }

        return newHandle
    }

    fun newRootScope(): CodeScope {
        return CodeScope(nextScopeId++, null)
    }
    infix fun newScope(parent: CodeScope): CodeScope {
        val newScope = CodeScope(nextScopeId++, parent)
        parentsOfScope[newScope] = HashSet()
        parentsOfScope[newScope]!!.add(parent)
        return newScope
    }
    infix fun newExprHandle(scope: CodeScope): CodeExprHandle {
        val newHandle = CodeExprHandle(nextExprHandle++)
        exprToCodeScope[newHandle] = scope
        return newHandle
    }
    infix fun newSubExprHandle(scope: CodeScope): CodeSubExprHandle {
        val newHandle = CodeSubExprHandle(nextSubExprHandle++)
        subExprToCodeScope[newHandle] = scope
        return newHandle
    }
}

private fun parseIdentAccess(identExpr: CodeExprHandle, scope: CodeScope, baseSubExprHandle: CodeSubExprHandle, access: Array<String>, isBaseStatic: Boolean = false): TypeHandle {
    val typeHandle = CodeScopeParser getTypeHandle baseSubExprHandle
    return when (val typeInfo = TypeRegistry getTypeInfo typeHandle) {
        is TypeInfo.Struct -> {
            access.forEach { print("$it ") }
            println()
            if (access.size == 1)
                return typeHandle
            var fieldSubExprHandle: CodeSubExprHandle? = null
            for (field in typeInfo.fields) {
                if (field.name == (access.getOrNull(1) ?: compilerICE("here 1"))) {
                    fieldSubExprHandle = CodeScopeParser.newSubExprHandle(scope)
                    CodeScopeParser.subExprTypeMap[fieldSubExprHandle] = field.typeHandle
                    break;
                }
            }
            parseIdentAccess(identExpr, scope,
                fieldSubExprHandle?: compilerError("could not find field ${access.getOrNull(1) ?: compilerICE("here 2")} on type: ${typeInfo.display()}"),
                access.sliceArray(1 until access.size)
            )
        }
        is TypeInfo.StaticType -> {
            val staticType = CodeScopeParser.subExprToStaticType[baseSubExprHandle] ?: compilerICE("could not find subexpressions static type")
            when (val staticTypeInfo = TypeRegistry getTypeInfo staticType ) {
                is TypeInfo.Struct -> {
                    if (access.size == 1) {
                        CodeScopeParser.exprToStaticType[identExpr] = staticType
                        return typeHandle
                    }
                    val fieldName = access.getOrNull(1) ?: compilerICE("field name index was null in static_type.struct")
                    var fieldSubExprHandle: CodeSubExprHandle? = null
                    for (field in staticTypeInfo.fields) {
                        if (!field.isStatic)
                            compilerError("cannot access non static fields on type")
                        if (field.name == fieldName) {
                            fieldSubExprHandle = CodeScopeParser.newSubExprHandle(scope)
                            CodeScopeParser.subExprTypeMap[fieldSubExprHandle] = typeHandle
                            break
                        }
                    }
                    parseIdentAccess(identExpr, scope,
                        fieldSubExprHandle?: compilerError("could not find field ${fieldName} on type: ${typeInfo.display()}"),
                        access.sliceArray(1 until access.size)
                    )
                }
                is TypeInfo.StaticType -> TODO()
                else -> return typeHandle
            }
        }
        else -> {
            TypeRegistry getTypeHandle typeInfo
        }
    }
}
