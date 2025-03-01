package compile.parser

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import compile.lexer.Token
import compile.lexer.TokenId
import compilerError
import compilerICE

fun Parser.parseExpr(bp: BindingPower): Either<Expr, ParseExprError> {
    var backtrackTo = head
    val token = currToken()
    if (token.id == TokenId.SemiColon)
        compilerError("semicolons should not be parsed as expressions")

    val nudHandler = nudLookup[token.id] ?: compilerError("expected nud handler for token: $token, head: $head, rem: ${remainder()}")
    var left = nudHandler()
    left.getOrNull()?.let {
        println("triggered nud")
        head = backtrackTo
        return left
    }
    backtrackTo = head

    if (currToken().id == TokenId.SemiColon)
        return left
    while (true) {
        val tokenBp = bindingPowers[currToken().id] ?: break

        if (tokenBp < bp)
            break
        ledLookup[currToken().id]?.let {
            val prevLeft = left
            left = it(left.leftOrNull()!!, bp)

            left.getOrNull()?.let {
                head = backtrackTo
                return ParseExprError(left.getOrNull()!!.msg, prevLeft.leftOrNull()!!).right()
            }
        }
        backtrackTo = head
    }
    return left
}

fun Parser.parseBinaryExpr(left: Expr, bp: BindingPower): Either<Expr.Binary, ParseExprError> {
    val operator = advance()
    val right = parseExpr(bp)
    right.getOrNull()?.let { return it.right() }

    return Expr.Binary(left, operator, right.leftOrNull()!!).left()
}

fun Parser.parseLiteralExpr(): Either<Expr, ParseExprError>{
    val token = advance()
    return when (token) {
        is Token.Number -> Expr.Number(token).left()
        is Token.String -> Expr.String(token.string).left()
        else -> compilerError("token $token is not a primary literal")
    }
}

fun Parser.parseIdent(): Either<Expr.Ident, ParseExprError> {
    val idents = arrayListOf<String>()

    while (true) {
        idents.add(advanceExpect(TokenId.Ident).let { (when (it) {
            is Either.Left -> it.value
            is Either.Right -> compilerError(it.value.msg)
        } as Token.Ident).ident })

        if (currToken().id != TokenId.Dot) {
            return Expr.Ident(idents.toTypedArray()).left()
        }
        advance()
    }
}
// call(32);
fun Parser.parseCall(left: Expr, bp: BindingPower): Either<Expr.Call, ParseExprError> {
    advanceExpect(TokenId.OpenParen)
    val args = arrayListOf<Expr>()

    while (true) {
        if (currToken().id != TokenId.CloseParen) {
            val newExpr = parseExpr(bp)
            newExpr.getOrNull()?.let { return it.right() }
            args.add(newExpr.leftOrNull()!!)

        }
        if (advance().id != TokenId.CloseParen) {
            backtrack()
            advanceExpect(TokenId.Comma)
        } else {
            return Expr.Call(left, args.toTypedArray()).left()
        }
    }
}

// StructName {field1: 32, field2: main()}
fun Parser.parseStructCreate(left: Expr, bp: BindingPower): Either<Expr.StructCreate, ParseExprError> {
    if (left !is Expr.Ident)
        return ParseExprError("struct name is an identifier").right()
    advanceExpect(TokenId.OpenBlock).getOrNull()?.let { return it.right() }
    val args = arrayListOf<Pair<String, Expr>>()
    var isFirst = true;

    while (true) {
        if (currToken().id != TokenId.CloseBlock) {
            val name = when (val x = advanceExpect(TokenId.Ident)) {
                is Either.Left<*> ->  x.leftOrNull()!!
                is Either.Right<*> -> return x
            } as Token.Ident
            advanceExpect(TokenId.Colon).getOrNull()?.let { return it.right() }

            val x = parseExpr(bp)
            x.getOrNull()?.let { return it.right() }
            val value = x.leftOrNull()!!
            args.add(Pair(name.ident, value))
            if (advance().id == TokenId.CloseBlock)
                break
            else {
                backtrack()
            }
            advanceExpect(TokenId.Comma).getOrNull()?.let { return it.right() }
            isFirst = false;
        } else {
            if (isFirst) {
                advance()
                break
            }
        }
    }
    return Expr.StructCreate(left, args.toTypedArray()).left()
}

//