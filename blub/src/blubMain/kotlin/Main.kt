@file:OptIn(ExperimentalForeignApi::class)

import com.github.ajalt.mordant.rendering.TextColors
import compile.code_analysis.CodeAnalyzer
import compile.code_analysis.ast_analizer.AnalyzeAstStructs
import compile.code_analysis.ast_analizer.AnalyzerFunctions
import compile.code_analysis.ast_analizer.GetAstBlocks
import compile.code_analysis.ast_analizer.GetDecls
import compile.code_analysis.ast_analizer.ValidateBlockLevelStmt
import compile.code_analysis.ast_analizer.ValidateTopLevelStmts
import compile.code_analysis.code_scope.CodeScopeParser
import compile.lexer.Lexer
import compile.parser.Parser
import compile.types.TypeRegistry
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.UIntVar
import kotlinx.cinterop.alloc
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.ptr
import kotlinx.cinterop.value
import llvmc.LLVMGetVersion
import okio.FileSystem
import okio.Path.Companion.toPath
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    memScoped {
        val major = alloc<UIntVar>()
        val minor = alloc<UIntVar>()
        val patch = alloc<UIntVar>()

        LLVMGetVersion(major.ptr, minor.ptr, patch.ptr)

        println("${major.value}.${minor.value}.${patch.value}")
    }

    val argCount = args.size
    when {
        argCount >= 1 -> {
            val path = args[0].toPath()
            val fileSystem = FileSystem.SYSTEM

            val code = fileSystem.read(path) {
                readUtf8()
            }

            val lexer = Lexer(code)
            lexer.tokenize()

            val parser = Parser(lexer.tokens.toTypedArray())
            parser.createTokenLookups()
            parser.createTypeTokenLookups()
            val ast = parser.parse()

            TypeRegistry.addPrimitives()

            val analyzer = CodeAnalyzer(ast)
            analyzer add GetAstBlocks()
            analyzer add ValidateTopLevelStmts()
            analyzer add ValidateBlockLevelStmt()
            analyzer add GetDecls()
            analyzer add AnalyzeAstStructs()
            analyzer add AnalyzerFunctions()

            analyzer.analyse()

            //ast.forEach(::println)

            //println(TypeRegistry.functionNameToHandle["main"]!!.display())
            val rootScope = CodeScopeParser getScope CodeScopeParser.newRootScope()
            rootScope parseCodeBlock ast

        }
        else -> tuiError("invalid arg")
    }
}

fun compilerError(message: String): Nothing {
    println("${TextColors.red("compiler error: ")}${TextColors.red.bg(message)}")
    println(Throwable().stackTraceToString())
    exitProcess(-1)
}
fun compilerICE(message: String): Nothing {
    println("${TextColors.red("internal compiler error: ")}${TextColors.red.bg(message)}")
    println(Throwable().stackTraceToString())
    exitProcess(-1)
}
fun tuiError(message: String): Nothing {
    println("${TextColors.red("error: ")}${TextColors.red.bg(message)}")
    println(Throwable().stackTraceToString())
    exitProcess(-1)
}
