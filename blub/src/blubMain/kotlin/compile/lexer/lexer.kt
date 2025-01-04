package compile.lexer

typealias LexerHandler = Lexer.() -> Boolean

class Lexer(private val code: String) {
    var remCode: String = code
    var head: Int = 0
    var tokens: ArrayList<Token> = arrayListOf()

    fun tokenize() {
        this.tokens = arrayListOf()
        this.head = 0
        this.remCode = code

        while (true) {
            val token = step()
            if (token is Token.Eof)
                break
        }
    }
    private fun step(): Token {
        var handled = false
        for (handler in handlers.value) {
            if (handler())  {
                handled = true
                break
            }
        }
        if (!handled) {
            throw Error("there was no handler for token at position: $head, and remaining: \n$remCode")
        }
        return tokens.last()
    }
    fun handleWhitespace() {
        val pattern = Regex("[\\s]*")

        val res = pattern.matchAt(remCode, 0) ?: throw Error("expected whitespace after token")
        setHead(head + res.range.last +1)
    }
    fun setHead(pos: Int) {
        head = pos
        remCode = code.slice(head..code.lastIndex)
    }
    private val handlers =
       lazy { arrayOf(
            handler@{
                if (remCode.isEmpty()) {
                    tokens.add(Token.Eof)
                    return@handler true
                }
                false
            },
            defaultHandler(Regex("let"), Token.Let),
            defaultHandler(Regex("mut"), Token.Mut),
            //
            defaultHandler(Regex("use"), Token.Use),
            defaultHandler(Regex("fn"), Token.Fn),
            defaultHandler(Regex("for"), Token.For),
            defaultHandler(Regex("in"), Token.In),
            defaultHandler(Regex("loop"), Token.Loop),
            defaultHandler(Regex("match"), Token.Match),
            defaultHandler(Regex("if"), Token.If),
            defaultHandler(Regex("struct"), Token.Struct),
            defaultHandler(Regex("pub"), Token.Pub),
            defaultHandler(Regex("@"), Token.At),
            //
            defaultHandler(Regex("or"), Token.Or),
            defaultHandler(Regex("and"), Token.And),
            //
            //
            identifierHandler(),
            stringHandler(),
            numberHandler(),
            //
            defaultHandler(Regex("\\("), Token.OpenParen),
            defaultHandler(Regex("\\)"), Token.CloseParen),
            defaultHandler(Regex("\\["), Token.OpenBracket),
            defaultHandler(Regex("\\]"), Token.CloseBracket),
            defaultHandler(Regex("\\{"), Token.OpenBlock),
            defaultHandler(Regex("\\}"), Token.CloseBlock),
            //
            defaultHandler(Regex("=="), Token.Eq),
            defaultHandler(Regex("="), Token.Assignment),
            defaultHandler(Regex("!"), Token.Not),
            defaultHandler(Regex("!="), Token.NotEq),
            defaultHandler(Regex("<="), Token.LessEq),
            defaultHandler(Regex(">="), Token.GreaterEq),
            defaultHandler(Regex("<"), Token.Less),
            defaultHandler(Regex(">"), Token.Greater),
            //
            defaultHandler(Regex("\\.\\.="), Token.RangeIncl),
            defaultHandler(Regex("\\.\\.="), Token.Range),
            defaultHandler(Regex("\\."), Token.Dot),
            defaultHandler(Regex(";"), Token.SemiColon),
            defaultHandler(Regex(":"), Token.Colon),
            defaultHandler(Regex(","), Token.Comma),
            defaultHandler(Regex("->"), Token.Arrow),
            //
            defaultHandler(Regex("\\+="), Token.PlusEq),
            defaultHandler(Regex("-="), Token.MinusEq),
            //
            defaultHandler(Regex("\\+"), Token.Plus),
            defaultHandler(Regex("-"), Token.Minus),
            defaultHandler(Regex("/"), Token.Div),
            defaultHandler(Regex("\\*"), Token.Star),
            defaultHandler(Regex("%"), Token.Percent)
        )}
}
fun stringHandler(): LexerHandler {
    return handler@{
        val res = Regex("\"(.*)\"").matchAt(remCode, 0) ?: return@handler false
        res.groups[0]?.let {
            setHead(head + res.range.last +1)
            handleWhitespace()
            tokens.add(Token.String(it.value))

            return@handler true

        } ?: throw Error("string capture block is null")
    }
}
fun identifierHandler(): LexerHandler {
    return handler@{
        val res = Regex("[a-zA-Z_][a-zA-Z0-9_]*").matchAt(remCode, 0) ?: return@handler false

        setHead(head + res.range.last +1)
        handleWhitespace()
        tokens.add(Token.Ident(res.value))

        true
    }
}
fun numberHandler(): LexerHandler {
    return handler@{
        val res = Regex("(?:-)?[0-9]+(?:\\.[0-9]+)?").matchAt(remCode, 0) ?: return@handler false

        val is_neg = Regex("-").matchAt(res.value, 0) != null
        val whole_part = Regex("[0-9]+").find(res.value)!!

        Regex("\\.([0-9]+)").find(res.value)?.let {
            tokens.add(Token.Number(is_neg, whole_part.value, it.groups[1]!!.value, null))
        } ?: tokens.add(Token.Number(is_neg, whole_part.value, null, null))

        setHead(head + res.range.last +1)
        handleWhitespace()
        true
    }
}
fun defaultHandler(pattern: Regex, token: Token): LexerHandler {
    return handler@{
        val res =  pattern.matchAt(remCode, 0) ?: return@handler false

        setHead(head + res.range.last +1)
        handleWhitespace()
        tokens.add(token)
        true
    }
}
