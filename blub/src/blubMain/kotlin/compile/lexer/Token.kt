package compile.lexer


enum class TokenId {
    Eof,
    Number,
    String,
    Ident,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBlock,
    CloseBlock,
    Assignment,
    At,

    // Comparison operators
    Eq,
    Not,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,

    // Logical operators
    Or,
    And,

    // Punctuation
    Dot,
    Range,
    RangeIncl,
    SemiColon,
    Colon,
    Comma,
    Arrow,

    // Compound assignment operators
    PlusEq,
    MinusEq,

    // Arithmetic operators
    Plus,
    Minus,
    Div,
    Star,
    Percent,

    // Reserved keywords
    Let,
    Mut,
    Use,
    Fn,
    For,
    In,
    Loop,
    Match,
    If,
    Struct,
    Pub,
}

sealed class Token(val string_repr: kotlin.String, val id: TokenId) {
    object Eof : Token("eof",TokenId.Eof)
    data class Number(
        val is_neg: Boolean,
        val whole_segment: kotlin.String,
        val decimal_segment: kotlin.String?,
        val number_type: kotlin.String?
    ) : Token("number", TokenId.Number)
    data class String(val string: kotlin.String) : Token("string", TokenId.String)
    data class Ident(val ident: kotlin.String) : Token("identifier", TokenId.Ident)

    object OpenParen : Token("(",TokenId.OpenParen)
    object CloseParen : Token(")",TokenId.CloseParen)
    object OpenBracket : Token("[",TokenId.OpenBracket)
    object CloseBracket : Token("]",TokenId.CloseBracket)
    object OpenBlock : Token("{",TokenId.OpenBlock)
    object CloseBlock : Token("}",TokenId.CloseBlock)
    object Assignment : Token("=",TokenId.Assignment)
    object At : Token("@",TokenId.At)

    // Comparison operators
    object Eq : Token("==",TokenId.Eq)
    object Not : Token("!",TokenId.Not)
    object NotEq : Token("!=",TokenId.NotEq)
    object Less : Token("<",TokenId.Less)
    object Greater : Token(">",TokenId.Greater)
    object LessEq : Token("<=",TokenId.LessEq)
    object GreaterEq : Token(">=",TokenId.GreaterEq)

    // Logical operators
    object Or : Token("or",TokenId.Or)
    object And : Token("and",TokenId.And)

    // Punctuation
    object Dot : Token(".",TokenId.Dot)
    object Range : Token("..",TokenId.Range)
    object RangeIncl : Token("..=",TokenId.RangeIncl)
    object SemiColon : Token(";",TokenId.SemiColon)
    object Colon : Token(":",TokenId.Colon)
    object Comma : Token(",", TokenId.Comma)
    object Arrow : Token("->",TokenId.Arrow)

    // Compound assignment operators
    object PlusEq : Token("+=",TokenId.PlusEq)
    object MinusEq : Token("-=",TokenId.MinusEq)

    // Arithmetic operators
    object Plus : Token("+",TokenId.Plus)
    object Minus : Token("-",TokenId.Minus)
    object Div : Token("/",TokenId.Div)
    object Star : Token("*",TokenId.Star)
    object Percent : Token("%",TokenId.Percent)

    // Reserved keywords
    object Let : Token("let",TokenId.Let)
    object Mut : Token("mut",TokenId.Mut)
    object Use : Token("use",TokenId.Use)
    object Fn : Token("fn",TokenId.Fn)
    object For : Token("for",TokenId.For)
    object In : Token("in",TokenId.In)
    object Loop : Token("loop",TokenId.Loop)
    object Match : Token("match",TokenId.Match)
    object If : Token("if",TokenId.If)
    object Struct : Token("struct",TokenId.Struct)
    object Pub : Token("pub",TokenId.Pub)

    override fun equals(other: Any?): Boolean {
        if (other == null) return false
        if (other !is Token) return false

        return this.id == other.id
    }
}
