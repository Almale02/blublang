@file:OptIn(ExperimentalForeignApi::class)
@file:Suppress("OPT_IN_USAGE_FUTURE_ERROR", "MISSING_DEPENDENCY_CLASS_IN_EXPRESSION_TYPE")

package llvm

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import compilerICE
import compilerICEWarning
import kotlinx.cinterop.*
import kotlinx.cinterop.internal.CCall
import llvm.types.AddressSpace
import llvm.types.FnType
import llvm.types.Type
import llvm.values.FnValue
import llvm.values.GlobalValue
import llvm.values.InstructionOpcode
import llvm.values.Value
import llvmc.*
import okio.Path
import platform.posix.memcpy

enum class Linkage {
    // `Appending` linkage may only be applied to global variables of pointer to array type. When two global
    // variables with appending linkage are linked together, the two global arrays are appended together.
    // This is the LLVM, typesafe, equivalent of having the system linker append together "sections" with
    // identical names when .o files are linked. Unfortunately this doesn't correspond to any feature in .o
    // files, so it can only be used for variables like llvm.global_ctors which llvm interprets specially.
    Appending,
    // Globals with `AvailableExternally` linkage are never emitted into the object file corresponding to
    // the LLVM module. From the linker's perspective, an `AvailableExternally` global is equivalent to an
    // external declaration. They exist to allow inlining and other optimizations to take place given
    // knowledge of the definition of the global, which is known to be somewhere outside the module. Globals
    // with `AvailableExternally` linkage are allowed to be discarded at will, and allow inlining and other
    // optimizations. This linkage type is only allowed on definitions, not declarations.
    AvailableExternally,
    // `Common` linkage is most similar to "weak" linkage, but they are used for tentative definitions
    // in C, such as "int X;" at global scope. Symbols with Common linkage are merged in the same way as
    // weak symbols, and they may not be deleted if unreferenced. `Common` symbols may not have an explicit
    // section, must have a zero initializer, and may not be marked 'constant'. Functions and aliases may
    // not have `Common` linkage.
    Common,
    // `DLLExport` causes the compiler to provide a global pointer to a pointer in a DLL, so that it can be
    // referenced with the dllimport attribute. On Microsoft Windows targets, the pointer name is formed by
    // combining __imp_ and the function or variable name. Since this storage class exists for defining a dll
    // interface, the compiler, assembler and linker know it is externally referenced and must refrain from
    // deleting the symbol.
    DLLExport,
    // `DLLImport` causes the compiler to reference a function or variable via a global pointer to a pointer
    // that is set up by the DLL exporting the symbol. On Microsoft Windows targets, the pointer name is
    // formed by combining __imp_ and the function or variable name.
    DLLImport,
    // If none of the other identifiers are used, the global is externally visible, meaning that it
    // participates in linkage and can be used to resolve external symbol references.
    External,
    // The semantics of this linkage follow the ELF object file model: the symbol is weak until linked,
    // if not linked, the symbol becomes null instead of being an undefined reference.
    ExternalWeak,
    // FIXME: Unknown linkage type
    Ghost,
    // Similar to private, but the value shows as a local symbol (STB_LOCAL in the case of ELF) in the object
    // file. This corresponds to the notion of the 'static' keyword in C.
    Internal,
    // FIXME: Unknown linkage type
    LinkerPrivate,
    // FIXME: Unknown linkage type
    LinkerPrivateWeak,
    // Globals with `LinkOnceAny` linkage are merged with other globals of the same name when linkage occurs.
    // This can be used to implement some forms of inline functions, templates, or other code which must be
    // generated in each translation unit that uses it, but where the body may be overridden with a more
    // definitive definition later. Unreferenced `LinkOnceAny` globals are allowed to be discarded. Note that
    // `LinkOnceAny` linkage does not actually allow the optimizer to inline the body of this function into
    // callers because it doesn’t know if this definition of the function is the definitive definition within
    // the program or whether it will be overridden by a stronger definition. To enable inlining and other
    // optimizations, use `LinkOnceODR` linkage.
    LinkOnceAny,
    // FIXME: Unknown linkage type
    LinkOnceODRAutoHide,
    // Some languages allow differing globals to be merged, such as two functions with different semantics.
    // Other languages, such as C++, ensure that only equivalent globals are ever merged (the "one definition
    // rule" — "ODR"). Such languages can use the `LinkOnceODR` and `WeakODR` linkage types to indicate that
    // the global will only be merged with equivalent globals. These linkage types are otherwise the same
    // as their non-odr versions.
    LinkOnceODR,
    // Global values with `Private` linkage are only directly accessible by objects in the current module.
    // In particular, linking code into a module with a private global value may cause the private to be
    // renamed as necessary to avoid collisions. Because the symbol is private to the module, all references
    // can be updated. This doesn’t show up in any symbol table in the object file.
    Private,
    // `WeakAny` linkage has the same merging semantics as linkonce linkage, except that unreferenced globals
    // with weak linkage may not be discarded. This is used for globals that are declared WeakAny in C source code.
    WeakAny,
    // Some languages allow differing globals to be merged, such as two functions with different semantics.
    // Other languages, such as C++, ensure that only equivalent globals are ever merged (the "one definition
    // rule" — "ODR"). Such languages can use the `LinkOnceODR` and `WeakODR` linkage types to indicate that
    // the global will only be merged with equivalent globals. These linkage types are otherwise the same
    // as their non-odr versions.
    WeakODR;

    companion object {
        public fun fromLLVM(llvm: LLVMLinkage) = when (llvm) {
            LLVMLinkage.LLVMExternalLinkage -> External
            LLVMLinkage.LLVMAvailableExternallyLinkage -> AvailableExternally
            LLVMLinkage.LLVMLinkOnceAnyLinkage -> LinkOnceAny
            LLVMLinkage.LLVMLinkOnceODRLinkage -> LinkOnceODR
            LLVMLinkage.LLVMLinkOnceODRAutoHideLinkage -> LinkOnceODRAutoHide
            LLVMLinkage.LLVMWeakAnyLinkage -> WeakAny
            LLVMLinkage.LLVMWeakODRLinkage -> WeakODR
            LLVMLinkage.LLVMAppendingLinkage -> Appending
            LLVMLinkage.LLVMInternalLinkage -> Internal
            LLVMLinkage.LLVMPrivateLinkage -> Private
            LLVMLinkage.LLVMDLLImportLinkage -> DLLImport
            LLVMLinkage.LLVMDLLExportLinkage -> DLLExport
            LLVMLinkage.LLVMExternalWeakLinkage -> ExternalWeak
            LLVMLinkage.LLVMGhostLinkage -> Ghost
            LLVMLinkage.LLVMCommonLinkage -> Common
            LLVMLinkage.LLVMLinkerPrivateLinkage -> LinkerPrivate
            LLVMLinkage.LLVMLinkerPrivateWeakLinkage -> LinkerPrivateWeak
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun toLLVM() = when (this) {
        Appending -> LLVMLinkage.LLVMAppendingLinkage
        AvailableExternally -> LLVMLinkage.LLVMAvailableExternallyLinkage
        Common -> LLVMLinkage.LLVMCommonLinkage
        DLLExport -> LLVMLinkage.LLVMDLLExportLinkage
        DLLImport -> LLVMLinkage.LLVMDLLImportLinkage
        External -> LLVMLinkage.LLVMExternalLinkage
        ExternalWeak -> LLVMLinkage.LLVMExternalWeakLinkage
        Ghost -> LLVMLinkage.LLVMGhostLinkage
        Internal -> LLVMLinkage.LLVMInternalLinkage
        LinkerPrivate -> LLVMLinkage.LLVMLinkerPrivateLinkage
        LinkerPrivateWeak -> LLVMLinkage.LLVMLinkerPrivateWeakLinkage
        LinkOnceAny -> LLVMLinkage.LLVMLinkOnceAnyLinkage
        LinkOnceODRAutoHide -> LLVMLinkage.LLVMLinkOnceODRAutoHideLinkage
        LinkOnceODR -> LLVMLinkage.LLVMLinkOnceODRLinkage
        Private -> LLVMLinkage.LLVMPrivateLinkage
        WeakAny -> LLVMLinkage.LLVMWeakAnyLinkage
        WeakODR -> LLVMLinkage.LLVMWeakODRLinkage
    }
}
@Suppress("MISSING_DEPENDENCY_CLASS_IN_EXPRESSION_TYPE")
public class Module(var moduleRef: LLVMModuleRef) {
    lateinit var dataLayout: LLVMString

    @OptIn(ExperimentalForeignApi::class)
    public fun addFunction(name: String, ty: FnType) = FnValue(Value(LLVMAddFunction(moduleRef, name, ty.ty)!!))
    @OptIn(ExperimentalForeignApi::class)
    public fun addFunction(name: String, ty: FnType, linkage: Linkage): FnValue{
        val fnValue = FnValue(Value(LLVMAddFunction(moduleRef, name, ty.ty)!!))
        fnValue.setLinkage(linkage)
        return fnValue
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getContext() = Context(LLVMGetModuleContext(moduleRef)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getFunction(name: String) = LLVMGetNamedFunction(moduleRef, name)?.let { FnValue(Value(it)) }
    public fun getFirstFunction() = LLVMGetFirstFunction(moduleRef)?.let { FnValue(Value(it)) }
    @OptIn(ExperimentalForeignApi::class)
    public fun getLastFunction() = FnValue(Value(LLVMGetLastFunction(moduleRef)!!))
    public fun getFunctions() = object: Iterator<FnValue> {
        var currFun = getFirstFunction()
        override fun hasNext() = currFun != null

        override fun next(): FnValue {
            val fn = currFun!!
            currFun = currFun!!.getNextFunction()
            return fn
        }
    }
    public fun setTriplet(triplet: String) = LLVMSetTarget(moduleRef, triplet)
    @OptIn(ExperimentalForeignApi::class)
    public fun getTriplet() = LLVMString(LLVMGetTarget(moduleRef)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun createExecEngine(): Either<ExecutionEngine, LLVMString> {
        memScoped {
            val engine = alloc<LLVMExecutionEngineRefVar>()
            val msg = alloc<CPointerVar<ByteVar>>()
            Target.initAll(InitConfig.default())
            val code = LLVMCreateExecutionEngineForModule(engine.ptr, moduleRef, msg.ptr)
            if (code == 1) {
                compilerICEWarning("failed to create exec engine: msg: ${LLVMString(msg.value!!)}")
                return LLVMString(msg.value!!).right()
            }
            return ExecutionEngine(engine.value!!, false).left()
        }
    }
    public fun createInterpreterExecEngine(): Either<ExecutionEngine, LLVMString> {
        memScoped {
            val engine = alloc<LLVMExecutionEngineRefVar>()
            val msg = alloc<CPointerVar<ByteVar>>()
            Target.initAll(InitConfig.default())
            val code = LLVMCreateInterpreterForModule(engine.ptr, moduleRef, msg.ptr)
            if (code == 1) {
                compilerICEWarning("failed to create exec engine: msg: ${LLVMString(msg.value!!)}")
                return LLVMString(msg.value!!).right()
            }
            return ExecutionEngine(engine.value!!, false).left()
        }
    }
    public infix fun createJitExecEngine(optLevel: OptLevel): Either<ExecutionEngine, LLVMString> {
        memScoped {
            val engine = alloc<LLVMExecutionEngineRefVar>()
            val msg = alloc<CPointerVar<ByteVar>>()
            Target.initAll(InitConfig.default())
            val code = LLVMCreateJITCompilerForModule(engine.ptr, moduleRef, optLevel.ordinal.toUInt(), msg.ptr)
            if (code == 1) {
                compilerICEWarning("failed to create exec engine: msg: ${LLVMString(msg.value!!)}")
                return LLVMString(msg.value!!).right()
            }
            return ExecutionEngine(engine.value!!, false).left()
        }
    }
    public fun addGlobal(type: Type, addressSpace: AddressSpace, name: String) = GlobalValue(Value(LLVMAddGlobalInAddressSpace(moduleRef, type.ty, name, addressSpace.addressSpace)!!))
    public fun addGlobal(type: Type, name: String) = GlobalValue(Value(LLVMAddGlobal(moduleRef, type.ty, name)!!))
    public fun writeBitcodeToMemory() = MemoryBuffer(LLVMWriteBitcodeToMemoryBuffer(moduleRef)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun verify(): Either<Unit, LLVMString> {
        memScoped {
            val error = alloc<CPointerVar<ByteVar>>()
            if (LLVMVerifyModule(moduleRef, LLVMVerifierFailureAction.LLVMReturnStatusAction, error.ptr) == 1) {
                return LLVMString(error.value!!).right()
            }
            LLVMDisposeMessage(error.value)
        }
        return Unit.left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun printToString() = LLVMString(LLVMPrintModuleToString(moduleRef)!!)
    override fun toString() = printToString().toString()
    @OptIn(ExperimentalForeignApi::class)
    public fun getFirstGlobal() = LLVMGetFirstGlobal(moduleRef)?.let { GlobalValue(Value(it)) }
    public fun getLastGlobal() = LLVMGetLastGlobal(moduleRef)?.let { GlobalValue(Value(it)) }
    @OptIn(ExperimentalForeignApi::class)
    public fun getGlobal(name: String) = LLVMGetNamedGlobal(moduleRef, name)?.let { GlobalValue(Value(it)) }
    public fun getGlobals() = object : Iterator<GlobalValue> {
        var globalValue = getFirstGlobal()
        override fun hasNext(): Boolean {
            return globalValue != null
        }

        override fun next(): GlobalValue {
            val curr = globalValue!!
            globalValue = globalValue!!.getNextGlobal()
            return curr
        }
    }
    public fun linkInModule(other: Module): Either<Unit, LLVMString> {
        TODO()
    }
    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun parseBitcodeFromBuffer(buffer: MemoryBuffer, context: Context): Either<Module, LLVMString> {
            memScoped {
                val module = alloc<LLVMModuleRefVar>()
                val err = alloc<CPointerVar<ByteVar>>()
                if (LLVMParseBitcodeInContext(context.contextRef, buffer.ref, module.ptr, err.ptr) != 0) {
                    return LLVMString(err.value!!).right()
                }
                return Module(module.value!!).left()
            }
        }
    }
}
























//