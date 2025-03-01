package compile.parser

import arrow.core.Either
import arrow.core.left
import compile.lexer.Token
import compile.lexer.TokenId
import compilerError
import compilerICE

fun Parser.parseStmt(): Stmt {
    stmtLookup[currToken().id]?.run {
        return this()
    }
    val expr = parseExpr(BindingPower.Default)
    expr.getOrNull()?.let { compilerError(it.msg) }
    advanceExpectManual(TokenId.SemiColon) ?: compilerError("this is not python idiot")

    return Stmt.ExprStmt(expr.leftOrNull()!!)
}
fun Parser.parseBlock(): Array<Stmt> {
    val block = arrayListOf<Stmt>()
    while (currToken().id != TokenId.CloseBlock) {
        block.add(parseStmt())
    }
    return block.toTypedArray()
}
fun Parser.parseVarDeclStmt(): Stmt {
    advance()

    val isMut = advance().id == TokenId.Mut

    if (!isMut)
        backtrack()
    val name = when (val x = advanceExpect(TokenId.Ident)) {
        is Either.Left -> x.value
        is Either.Right -> compilerError(x.value.msg)
    }.let { (it as Token.Ident).ident }
    advanceExpect(TokenId.Assignment)

    if (currToken().id == TokenId.SemiColon)
       return Stmt.VarDecl(name, isMut, null)

    val initValue = when (val expr = parseExpr(BindingPower.Default)) {
        is Either.Left -> expr.value
        is Either.Right -> compilerError(expr.value.msg)
    }
    advanceExpect(TokenId.SemiColon)

    return Stmt.VarDecl(name, isMut, initValue)
}
fun Parser.parseForStmt(): Stmt {
    advance()
    val capture = when (val x = parseExpr(BindingPower.Default)) {
        is Either.Left -> x.value
        is Either.Right -> compilerError(x.value.msg)
    }
    advanceExpect(TokenId.In)
    var iterator = parseExpr(BindingPower.Default)

    iterator.getOrNull()?.let { it ->
        it.parsedSuccessfully?.let {
            iterator = it.left()
        }
    }
    advanceExpect(TokenId.OpenBlock).getOrNull()?.let { openBlockMsg ->
        iterator.getOrNull()?.let {
            compilerError(it.msg)
        }
        compilerError("this ${openBlockMsg.msg}")
    }
    val body = parseBlock()
    advanceExpect(TokenId.CloseBlock)
    return Stmt.For(capture, iterator.leftOrNull() ?: compilerICE("this is fucking up"), body)
}
fun Parser.parseIfStmt(): Stmt {
    advance()
    val guard = when (val x = parseExpr(BindingPower.Default)) {
        is Either.Left -> x.value
        is Either.Right -> compilerError(x.value.msg)
    }
    advanceExpect(TokenId.OpenBlock)
    val body = parseBlock()
    advanceExpect(TokenId.CloseBlock)
    return Stmt.If(guard, body)
}
fun Parser.parseFunctionDeclStmt(): Stmt {
    val isPub = isPrev(TokenId.Pub)
    var returnType: AstType? = null
    val args = arrayListOf<ArgsDecl>()

    advanceExpect(TokenId.Fn)
    val isExtern = currToken().id == TokenId.At
    if (isExtern)
        advance()
    val name = when (val x = advanceExpect(TokenId.Ident)) {
        is Either.Left -> x.value
        is Either.Right -> compilerError(x.value.msg)
    }.let { (it as Token.Ident).ident }
    advanceExpect(TokenId.OpenParen)

    while (true) {
        if (currToken() == Token.CloseParen) {
            advance()
            break
        }
        val argName = advanceExpect(TokenId.Ident).let { (when (it) {
            is Either.Left -> it.value
            is Either.Right -> compilerError(it.value.msg)
        } as Token.Ident).ident }
        advanceExpect(TokenId.Colon)
        val argType = parseType(BindingPower.Default)

        args.add(ArgsDecl(argName, argType))
        if (currToken().id == TokenId.CloseParen) {
            advance()
            break
        }
        advanceExpect(TokenId.Comma)
    }
    if (currToken().id == TokenId.Colon) {
        advance()
        returnType = parseType(BindingPower.Default)
    }
    advanceExpect(TokenId.OpenBlock)
    val body = parseBlock()
    advanceExpect(TokenId.CloseBlock)

    return Stmt.FunctionDecl(isExtern, isPub, name, args.toTypedArray(), body, returnType)
}
fun Parser.parseStructDeclStmt(): Stmt {
    val isPub = isPrev(TokenId.Pub)
    val fields = arrayListOf<AstStructField>()
    advanceExpect(TokenId.Struct)

    val isExtern = currToken().id == TokenId.At
    if (isExtern) {
        advance()
    }
    val structName = advanceExpect(TokenId.Ident).let { (when (it) {
        is Either.Left -> it.value
        is Either.Right -> compilerError(it.value.msg)
    } as Token.Ident).ident }
    advanceExpect(TokenId.OpenBlock)

    while (true) {
        if (currToken().id == TokenId.CloseBlock)
            break
        val isFieldPub = currToken().id == TokenId.Pub
        if (isFieldPub)
           advance()
        val fieldName = advanceExpect(TokenId.Ident).let { (when (it) {
            is Either.Left -> it.value
            is Either.Right -> compilerError(it.value.msg)
        } as Token.Ident).ident }

        advanceExpect(TokenId.Colon)
        val fieldType = parseType(BindingPower.Default)

        val hasDefaultValue = currToken().id == TokenId.Assignment
        val defaultValue = hasDefaultValue.let {
            if (it) {
                advanceExpect(TokenId.Assignment)
                when (val x = parseExpr(BindingPower.Default)) {
                    is Either.Left -> x.value
                    is Either.Right -> compilerError(x.value.msg)
                }
            } else {
                null
            }
        }
        fields.add(AstStructField(isFieldPub, fieldName, fieldType, defaultValue))
        advanceExpect(TokenId.Comma)
    }
    advance()
    return Stmt.StructDecl(isExtern, isPub, structName, fields.toTypedArray())
}