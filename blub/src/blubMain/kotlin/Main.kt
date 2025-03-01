@file:OptIn(ExperimentalForeignApi::class)

import arrow.core.Either
import arrow.core.left
import com.github.ajalt.mordant.rendering.TextColors
import compile.code_analysis.CodeAnalyzer
import compile.code_analysis.ast_analizer.AnalyzeAstStructs
import compile.code_analysis.ast_analizer.AnalyzerFunctions
import compile.code_analysis.ast_analizer.GetAstBlocks
import compile.code_analysis.ast_analizer.GetDecls
import compile.code_analysis.ast_analizer.ValidateBlockLevelStmt
import compile.code_analysis.ast_analizer.ValidateTopLevelStmts
import compile.code_analysis.code_scope.AnalysisExpr
import compile.code_analysis.code_scope.AnalysisStmt
import compile.code_analysis.code_scope.CodeScope
import compile.code_analysis.code_scope.CodeScopeParser
import compile.lexer.Lexer
import compile.parser.Parser
import compile.parser.Stmt
import compile.parser.StmtId
import compile.types.TypeRegistry
import kotlinx.cinterop.*
import llvm.*
import llvm.types.AddressSpace
import llvm.types.FnType
import llvmc.*
import okio.FileSystem
import okio.Path
import okio.Path.Companion.toPath
import platform.posix._configthreadlocale
import kotlin.system.exitProcess

@OptIn(ExperimentalForeignApi::class)
fun main(args: Array<String>) {
    val argCount = args.size
    when {
        argCount >= 2 -> {
            when (args[0]) {
                "build" -> {
                    val path = args[1].toPath()
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
                    val rootScope = CodeScopeParser.newRootScope()
                    rootScope parseCodeBlock ast

                    rootScope.stmts.forEach {
                        if (it is AnalysisStmt.FunctionDecl) {
                            it.scope.varNameToExprHandle["lena"]?.let {
                                //
                            }
                        }
                    }
                }
            }
        }
        argCount == 1 -> {
            when (args[0]) {
                "llvm" -> {
                    val context = Context.create() ?: compilerICE("failed to create context")
                    val i8 = context.i8Type()
                    val i32 = context.i32Type()
                    val i64 = context.i64Type()
                    val void = context.voidType()

                    val module = context.createModule("main")
                    val builder = context.createBuilder()
                    val mainFnType  = i32.fnType(arrayOf(), false)
                    val mainFn = module.addFunction("main", mainFnType, Linkage.External)
                    val mainBlock = context.appendBasicBlock(mainFn, "main_block")
                    val printfFnType = i32.fnType(arrayOf(i8.ptrType()), true)
                    val printfFn = module.addFunction("printf", printfFnType, Linkage.External)

                    builder.setAtEnd(mainBlock)
                    val decFormatString = builder.buildGlobalStringPtr("num is %d\n", "dec_fmt_str").leftOrNull()!!

                    builder.buildCall(printfFn, printfFnType, arrayOf(decFormatString.value, i32.constInt((-32).toULong(), true)), "hellow_orld")
                    builder.buildReturn(i32.constInt(1u, false))
                    module.setTriplet("x86_64-pc-windows-msvc")
                    module.writeBitcodeToMemory().writeToFile("./out.bc".toPath())
                    println(module.toString())
                }
                else -> tuiError("invalid args")
            }
        }
        else -> main(arrayOf("build", "test.blub"))
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
fun compilerICEWarning(message: String) {
    println("${TextColors.red("internal compiler error warning: ")}${TextColors.red.bg(message)}")
    println(Throwable().stackTraceToString())
}
fun tuiError(message: String): Nothing {
    println("${TextColors.red("error: ")}${TextColors.red.bg(message)}")
    println(Throwable().stackTraceToString())
    exitProcess(-1)
}
//lld-link out.obj /subsystem:console kernel32.lib libucrt.lib /entry:main/
//lld-link out.obj /subsystem:console libucrt.lib libcmt.lib kernel32.lib