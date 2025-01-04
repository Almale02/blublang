
package compile.code_analysis.code_scope

import compile.parser.Expr
import compile.parser.Stmt
import compile.types.NumberTypes
import compile.types.TypeHandle
import compile.types.TypeInfo
import compile.types.TypeRegistry
import compilerError
import compilerICE

data class CodeScopeHandle(val id: ULong)
data class CodeExprHandle(val id: ULong)

class CodeScope(val handle: CodeScopeHandle, val parent: CodeScopeHandle?) {
    val exprTypeMap = hashMapOf<CodeExprHandle, TypeHandle>()
    val exprToAnalysisExpr = hashMapOf<CodeExprHandle, AnalysisExpr>()
    val varNameToExprHandle = hashMapOf<String, CodeExprHandle>()
    val stmts = arrayListOf<AnalysisStmt>()

    infix fun getTypeHandle(exprHandle: CodeExprHandle): TypeHandle {
        return exprTypeMap[exprHandle]!!
    }
    infix fun getAnalysisExpr(exprHandle: CodeExprHandle): AnalysisExpr{
        return exprToAnalysisExpr[exprHandle]!!
    }
    infix fun getExprHandle(varName: String): CodeExprHandle {
        return varNameToExprHandle[varName]!!
    }
    fun addScope(): CodeScopeHandle {
        return CodeScopeParser.newScope(handle)
    }
    infix fun parseCodeBlock(block: Array<Stmt>) {
        block.forEach(::parseAstStmt)
    }
    infix fun getVarType(varName: String): Pair<TypeHandle, CodeScopeHandle>? {
        val exprHandleRes = getVarExprHandle(varName) ?: return null
        val typeHandle = CodeScopeParser.getScope(exprHandleRes.second).getTypeHandle(exprHandleRes.first)
        return Pair(typeHandle, exprHandleRes.second)

    }
    infix fun getVarExprHandle(varName: String): Pair<CodeExprHandle, CodeScopeHandle>? {
        varNameToExprHandle[varName]?.let { return Pair(it, handle) }
        var parent = this.parent
        while (true) {
            if (parent == null)
                compilerICE("unreachable code")

            val parentScope = CodeScopeParser getScope parent
            val varExprHandle = parentScope.varNameToExprHandle[varName]

            if (varExprHandle != null) {
                return Pair(varExprHandle, parent)
            } else {
                val newParent = parentScope.parent
                if (newParent != null)
                    parent = newParent
                else {
                    return null
                }
            }
        }
    }
    infix fun parseAstExpr(expr: Expr): CodeExprHandle {
        return when (expr) {
            is Expr.Number -> {
                val exprHandle = CodeScopeParser.newExprHandle()
                val analysisExpr = AnalysisExpr.Number(exprHandle)
                val i32TypeHandle = TypeRegistry getTypeHandle TypeInfo.Number(NumberTypes.I32)

                exprTypeMap[exprHandle] = i32TypeHandle
                exprToAnalysisExpr[exprHandle] = analysisExpr

                exprHandle

            }
            is Expr.String -> {
                val exprHandle = CodeScopeParser.newExprHandle()
                val analysisExpr = AnalysisExpr.String
                val typeHandle = TypeRegistry getTypeHandle TypeInfo.Str

                exprTypeMap[exprHandle] = typeHandle
                exprToAnalysisExpr[exprHandle] = analysisExpr

                exprHandle
            }
            is Expr.Ident -> {
                val baseVarName = expr.ident.first()

                var baseVarType = getVarType(baseVarName)
                if (baseVarType == null)  {
                    TypeRegistry.functionNameToHandle[baseVarName]?.let { funcType ->
                        if (expr.ident.size > 1)
                            compilerError("you cannot access fields on functions")

                        val exprHandle = CodeScopeParser.newExprHandle()
                        exprTypeMap[exprHandle] = funcType
                        exprToAnalysisExpr[exprHandle] = AnalysisExpr.Ident(exprHandle, arrayOf())

                        return exprHandle
                    } ?: compilerError("expected variable $baseVarName to be defined")
                }
                val typeHandle = parseIdentAccess(baseVarType.first, expr.ident)
                val exprHandle = CodeScopeParser.newExprHandle()
                exprTypeMap[exprHandle] = typeHandle
                exprToAnalysisExpr[exprHandle] = AnalysisExpr.Ident(
                    CodeScopeParser.getScope(baseVarType.second) getExprHandle baseVarName,
                    expr.ident
                )

                exprHandle
            }
            is Expr.Call -> {
                val baseExprHandle = this parseAstExpr expr.base
                val baseType = this getTypeHandle baseExprHandle
                val functionTypeInfo = TypeRegistry getTypeInfo baseType
                if (functionTypeInfo!is TypeInfo.Fn)
                    compilerError("only functions can be called")

                if (functionTypeInfo.args.size != expr.args.size)
                    compilerError("function ${functionTypeInfo.display()} expected ${functionTypeInfo.args.size} but got: ${expr.args.size}")
                val args = expr.args.withIndex().map { (i, argExpr) ->
                    val argExprHandle = this parseAstExpr argExpr
                    val argExprType = this getTypeHandle argExprHandle
                    val argTypeInfo = TypeRegistry getTypeInfo argExprType

                    val expectedType = functionTypeInfo.args[i]

                    if (expectedType != argExprType)
                        compilerError(
                            "expected type ${expectedType.display()}, but found type: ${argExprType.display()}" +
                                    " at function call ${functionTypeInfo.display()} at arg $i"
                        )

                    argExprHandle
                }.toTypedArray()

                val exprHandle = CodeScopeParser.newExprHandle()
                exprTypeMap[exprHandle] = functionTypeInfo.returnTypeHandle
                exprToAnalysisExpr[exprHandle] = AnalysisExpr.Call(baseExprHandle, args)

                exprHandle
            }
            is Expr.Binary -> {
                val leftExprHandle = this parseAstExpr expr.left
                val rightExprHandle = this parseAstExpr expr.right

                val leftTypeHandle = this getTypeHandle leftExprHandle
                val rightTypeHandle = this getTypeHandle rightExprHandle

                val leftTypeInfo = TypeRegistry getTypeInfo leftTypeHandle
                val rightTypeInfo = TypeRegistry getTypeInfo rightTypeHandle

                if (leftTypeHandle != rightTypeHandle)
                    compilerError("binary expr expects all sides to be the same type, but left is: ${leftTypeInfo.display()}, and right is: ${rightTypeInfo.display()}")
                val exprHandle = CodeScopeParser.newExprHandle()

                exprTypeMap[exprHandle] = leftTypeHandle
                exprToAnalysisExpr[exprHandle] = AnalysisExpr.Binary(leftExprHandle, expr.op, rightExprHandle)

                exprHandle
            }
        }
    }
    infix fun parseAstStmt(stmt: Stmt) {
        when (stmt) {
            is Stmt.VarDecl -> {
                if (getVarType(stmt.name) != null)
                    compilerError("redefinition of variable ${stmt.name}")
                if (stmt.initValue == null)
                    compilerError("init values expected for variables")

                val initExprHandle = this parseAstExpr stmt.initValue
                varNameToExprHandle[stmt.name] = initExprHandle
                stmts.add(AnalysisStmt.VarDecl(stmt, initExprHandle))
            }
            is Stmt.If -> {
                val guardExprHandle = this parseAstExpr stmt.guard
                val newScopeHandle = addScope()

                CodeScopeParser.getScope(newScopeHandle).parseCodeBlock(stmt.body)

                stmts.add(AnalysisStmt.If(stmt, newScopeHandle, guardExprHandle))
            }
            is Stmt.For -> {
                val iterExprHandle = this parseAstExpr stmt.iteratorExpr
                val iterType = this getTypeHandle iterExprHandle
                val iterTypeInfo = TypeRegistry getTypeInfo iterType

                val newScopeHandle = addScope()
                val newScope = CodeScopeParser getScope newScopeHandle

                val captureVars = arrayListOf<String>()

                when (iterTypeInfo) {
                    is TypeInfo.Array -> {
                        val captureTypeHandle = CodeScopeParser.newExprHandle()
                        val captureExpr = stmt.captureExpr
                            when (captureExpr) {
                            is Expr.Ident -> {
                                if (captureExpr.ident.size != 1)
                                    compilerError("variable names cant have dont in them")
                                captureVars.add(captureExpr.ident.first())
                                newScope.varNameToExprHandle[captureExpr.ident.first()] = captureTypeHandle
                                newScope.exprTypeMap[captureTypeHandle] = iterTypeInfo.handle
                                newScope.exprToAnalysisExpr[captureTypeHandle] = AnalysisExpr.CaptureVar(true, iterTypeInfo.handle, captureExpr.ident.first())
                            }
                            else -> compilerError("for array iterations, the capture must be a variable name")
                        }
                    }
                    else -> compilerError("${iterTypeInfo.display()} doesnt support iteration")
                }
                newScope parseCodeBlock stmt.body
                stmts.add(AnalysisStmt.For(stmt, newScopeHandle, captureVars.toTypedArray(), iterExprHandle))
            }
            is Stmt.FunctionDecl -> {
                val newScopeHandle = addScope()
                val newScope = CodeScopeParser getScope newScopeHandle
                val funcTypeHandle = TypeRegistry funcNameToHandle stmt.name
                val funcInfo = TypeRegistry getTypeInfo funcTypeHandle
                if (funcInfo !is TypeInfo.Fn)
                    compilerError("unreachable code")

                for ((i, arg) in stmt.args.withIndex()) {
                    val argType = funcInfo.args[i]
                    val argExprHandle = CodeScopeParser.newExprHandle()

                    newScope.varNameToExprHandle[arg.name] = argExprHandle
                    newScope.exprTypeMap[argExprHandle] = argType
                    newScope.exprToAnalysisExpr[argExprHandle] = AnalysisExpr.FnArg(arg.name, argType)
                }
                newScope parseCodeBlock stmt.body
                stmts.add(AnalysisStmt.FunctionDecl(stmt, funcInfo, newScopeHandle))
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
}

object CodeScopeParser {
    val scopes = hashMapOf<CodeScopeHandle, CodeScope>()
    var nextScopeHandle: ULong = 1u
    var nextExprHandle: ULong = 1u

    infix fun getScope(handle: CodeScopeHandle): CodeScope {
        return scopes[handle]!!
    }
    fun newRootScope(): CodeScopeHandle {
        val newHandle = CodeScopeHandle(nextScopeHandle++)

        scopes[newHandle] = CodeScope(newHandle, null)
        return newHandle
    }
    infix fun newScope(parent: CodeScopeHandle): CodeScopeHandle {
        val newId = CodeScopeHandle(nextScopeHandle++)

        scopes[newId] = CodeScope(newId, parent)
        return newId
    }
    fun newExprHandle(): CodeExprHandle {
        return CodeExprHandle(nextExprHandle++)
    }
}

private fun parseIdentAccess(baseType: TypeHandle, access: Array<String>): TypeHandle {
    val typeInfo = TypeRegistry getTypeInfo baseType
    return when (typeInfo) {
        is TypeInfo.Struct -> {
            if (access.isEmpty())
                baseType
            var currType: TypeHandle? = null
            for (field in typeInfo.fields) {
                if (field.name == access[1]) {
                    currType = field.typeHandle
                    break;
                }
            }

            var newAccess = access.copyOfRange(1, access.lastIndex)
            parseIdentAccess(
                currType ?: compilerError("could not find field ${access[1]} on type: ${typeInfo.display()}"),
                newAccess
            )
        }
        else -> {
            TypeRegistry getTypeHandle typeInfo
        }
    }
}