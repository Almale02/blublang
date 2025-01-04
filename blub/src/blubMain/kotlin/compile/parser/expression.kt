package compile.parser

import compile.lexer.Token
import compile.lexer.TokenId
import compilerError

fun Parser.parseExpr(bp: BindingPower): Expr {
    val token = currToken()
    if (token.id == TokenId.SemiColon)
        compilerError("semicolons should not be parsed as expressions")

    val nudHandler = nudLookup[token.id] ?: compilerError("expected nud handler for token: $token")
    var left = nudHandler()

    if (currToken().id == TokenId.SemiColon)
        return left
    while (true) {
        val tokenBp = bindingPowers[currToken().id] ?: break

        if (tokenBp < bp)
            break
        ledLookup[currToken().id]?.let {
            left = it(left, bp)
        }
    }
    return left
}
fun Parser.parseBinaryExpr(left: Expr, bp: BindingPower): Expr {
    val operator = advance()
    val right = parseExpr(bp)

    return Expr.Binary(left, operator, right)
}
fun Parser.parseLiteralExpr(): Expr {
    val token = advance()
    return when(token) {
        is Token.Number -> Expr.Number(token)
        is Token.String -> Expr.String(token.string)
        else -> compilerError("token $token is not a primary literal")
    }
}
fun Parser.parseIdent(): Expr {
    val idents = arrayListOf<String>()

    while (true) {
        idents.add(advanceExpect(TokenId.Ident).let { (it as Token.Ident).ident })

        if (currToken().id != TokenId.Dot) {
            return Expr.Ident(idents.toTypedArray())
        }
        advance()
    }
}
fun Parser.parseCall(left: Expr, bp: BindingPower): Expr {
    advanceExpect(TokenId.OpenParen)
    val args = arrayListOf<Expr>()

    while(true) {
        if (currToken().id != TokenId.CloseParen)
            args.add(parseExpr(bp))
        if (advance().id != TokenId.CloseParen) {
            backtrack()
            advanceExpect(TokenId.Comma)
        } else {
            return Expr.Call(left, args.toTypedArray())
        }
    }
}