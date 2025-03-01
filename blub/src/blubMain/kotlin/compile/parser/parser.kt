package compile.parser

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import compile.lexer.Token
import compile.lexer.TokenId
import compilerError

typealias StmtHandler = Parser.() -> Stmt
typealias NudHandler = Parser.() -> Either<Expr, ParseExprError>
typealias LedHandler = Parser.(Expr, BindingPower) -> Either<Expr, ParseExprError>

typealias TypeNudHandler = Parser.() -> AstType
typealias TypeLedHandler = Parser.(AstType, BindingPower) -> AstType

data class ParseExprError(val msg: String, val parsedSuccessfully: Expr? = null)


class Parser(val tokens: Array<Token>) {
    var head: Int = 0
    val skipTokens: HashSet<TokenId> = HashSet()

    val bindingPowers: HashMap<TokenId, BindingPower> = HashMap()
    val nudLookup: HashMap<TokenId, NudHandler> = HashMap()
    val ledLookup: HashMap<TokenId, LedHandler> = HashMap()

    val typeBindingPowers: HashMap<TokenId, BindingPower> = HashMap()
    val typeNudLookup: HashMap<TokenId, TypeNudHandler> = HashMap()
    val typeLedLookup: HashMap<TokenId, TypeLedHandler> = HashMap()

    val stmtLookup: HashMap<TokenId, StmtHandler> = HashMap()

    fun currToken(): Token = tokens[head]
    fun hasFinished(): Boolean = currToken() is Token.Eof

    fun parse(): Array<Stmt> {
        head = 0
        if (skipTokens.contains(currToken().id))
            advance()
        val body = arrayListOf<Stmt>()

        while (!hasFinished()) {
            body.add(parseStmt())
        }
        return body.toTypedArray()
    }
    fun remainder(): String {
        var string = "\n"
        for (token in tokens.slice(head..tokens.lastIndex)) {
            string += token.string_repr
            string += "\n"
        }
        return string
    }
    fun advance(): Token {
        if (hasFinished())
            compilerError("reached eof before expected")

        val curr = currToken()
        while(true) {
            head += 1
            if (!skipTokens.contains(currToken().id)) {
                break
            }
        }
        return curr
    }
    fun advanceExpect(expected: TokenId): Either<Token, ParseExprError> {
        val token = currToken()
        if (token.id != expected)
            return ParseExprError("expected token $expected, but found: ${token.id}").right()

        return advance().left()
    }
    fun advanceExpectManual(expected: TokenId): Token? {
        if (currToken().id != expected)
            return null

        return advance()
    }
    fun backtrack(): Token? {
        if (head == 0)
            return null

        head -= 1
        return currToken()
    }
    fun lookBack(): Token? {
        if (head == 0)
            return null

        return tokens[head -1]
    }
    fun isPrev(expected: TokenId): Boolean {
        return lookBack()?.let { it.id == expected } ?: false
    }
    private fun nud(tokenId: TokenId, handler: NudHandler) {
        nudLookup[tokenId] = handler
    }
    private fun led(tokenId: TokenId, bindingPower: BindingPower, handler: LedHandler) {
        bindingPowers[tokenId] = bindingPower
        ledLookup[tokenId] = handler
    }
    private fun typeNud(tokenId: TokenId, handler: TypeNudHandler) {
        typeNudLookup[tokenId] = handler
    }
    fun typeLed(tokenId: TokenId, bindingPower: BindingPower, handler: TypeLedHandler) {
        typeBindingPowers[tokenId] = bindingPower
        typeLedLookup[tokenId] = handler
    }
    private fun stmt(tokenId: TokenId, handler: StmtHandler) {
        stmtLookup[tokenId] = handler
    }
    fun createTokenLookups() {
        led(TokenId.And, BindingPower.Logical, Parser::parseBinaryExpr)
        led(TokenId.Or, BindingPower.Logical, Parser::parseBinaryExpr)
        led(TokenId.Not, BindingPower.Logical, Parser::parseBinaryExpr)
        //
        led(TokenId.Less, BindingPower.Relational, Parser::parseBinaryExpr)
        led(TokenId.LessEq, BindingPower.Relational, Parser::parseBinaryExpr)
        led(TokenId.Greater, BindingPower.Relational, Parser::parseBinaryExpr)
        led(TokenId.GreaterEq, BindingPower.Relational, Parser::parseBinaryExpr)
        led(TokenId.Eq, BindingPower.Relational, Parser::parseBinaryExpr)
        led(TokenId.NotEq, BindingPower.Relational, Parser::parseBinaryExpr)
        //
        led(TokenId.Plus, BindingPower.Additive, Parser::parseBinaryExpr)
        led(TokenId.Minus, BindingPower.Additive, Parser::parseBinaryExpr)

        led(TokenId.Star, BindingPower.Multiplicative, Parser::parseBinaryExpr)
        led(TokenId.Div, BindingPower.Multiplicative, Parser::parseBinaryExpr)
        led(TokenId.Percent, BindingPower.Multiplicative, Parser::parseBinaryExpr)
        //
        led(TokenId.OpenParen, BindingPower.Call, Parser::parseCall)
        //
        led(TokenId.OpenBlock, BindingPower.Primary, Parser::parseStructCreate)
        //
        //
        stmt(TokenId.Let, Parser::parseVarDeclStmt);
        stmt(TokenId.If, Parser::parseIfStmt);
        stmt(TokenId.For, Parser::parseForStmt);
        stmt(TokenId.Fn, Parser::parseFunctionDeclStmt);
        stmt(TokenId.Struct, Parser::parseStructDeclStmt);
        //
        //
        nud(TokenId.Ident, Parser::parseIdent)
        nud(TokenId.Number, Parser::parseLiteralExpr)
        nud(TokenId.String, Parser::parseLiteralExpr)
        //
        //
        //
        skipTokens.add(TokenId.Pub)
    }
    fun createTypeTokenLookups() {
        typeNud(TokenId.Ident, Parser::parseSymbolType)
        typeNud(TokenId.OpenBracket, Parser::parseArrayType)
        typeNud(TokenId.Star, Parser::parsePointerType)
    }
}

enum class BindingPower(val power: Int) {
    Default(0),
    Comma(1) ,
    Assignment(2),
    Range(3),
    Logical(4),
    Relational(5),
    Additive(6),
    Multiplicative(7),
    Unary(8),
    Call(9),
    Member(10),
    Primary(11),
}
