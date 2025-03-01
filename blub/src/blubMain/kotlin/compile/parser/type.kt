package compile.parser

import arrow.core.Either
import compile.lexer.Token
import compile.lexer.TokenId
import compilerError

fun Parser.parseType(bp: BindingPower): AstType {
   val token = currToken()
   val nudHandler = typeNudLookup[token.id] ?: compilerError("expected type, but found token: ${token.string_repr}")

    var left = nudHandler()

    if (currToken().id == TokenId.SemiColon)
        return left
    while (true) {
        val tokenBp = typeBindingPowers[currToken().id] ?: break

        if (tokenBp < bp)
            break
        typeLedLookup[currToken().id]?.let {
            left = it(left, bp)
        }
    }
    return left
}
fun Parser.parseSymbolType(): AstType {
    return AstType.Symbol(advanceExpect(TokenId.Ident).let { (when (it) {
        is Either.Left -> it.value
        is Either.Right -> compilerError(it.value.msg)
    } as Token.Ident).ident })
}
fun Parser.parseArrayType(): AstType {
    advanceExpect(TokenId.OpenBracket)
    val itemType = parseType(BindingPower.Default)
    advanceExpect(TokenId.CloseBracket)

    return AstType.Array(itemType)
}
fun Parser.parsePointerType(): AstType {
    advanceExpect(TokenId.Star)
    val isMut = currToken().id == TokenId.Mut

    if (isMut)
        advance()
    return AstType.Pointer(isMut, parseType(BindingPower.Default))
}