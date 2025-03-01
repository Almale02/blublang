package llvm

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import kotlinx.cinterop.*
import llvm.values.FnValue
import llvm.values.GenericValue
import llvm.values.Value
import llvmc.*

class ExecutionEngine @OptIn(ExperimentalForeignApi::class) constructor(val ref: LLVMExecutionEngineRef, val jitMode: Boolean) {
    var targetData: TargetData? = null
    @OptIn(ExperimentalForeignApi::class)
    public fun addGlobalMapping(value: Value, add: ULong) {
        TODO()
    }
    @OptIn(ExperimentalForeignApi::class)
    public infix fun addModule(module: Module) = LLVMAddModule(ref, module.moduleRef)
    @OptIn(ExperimentalForeignApi::class)
    public infix fun removeModule(module: Module): Either<Unit, LLVMString> {
        memScoped {
            val newMod = alloc<LLVMModuleRefVar>()
            val error = alloc<CPointerVar<ByteVar>>()
            if (LLVMRemoveModule(ref, module.moduleRef, newMod.ptr, error.ptr) == 1)  {
                return LLVMString(error.value!!).right()
            }
            module.moduleRef = newMod.value!!
        }
        return Unit.left()
    }
    public infix fun getFn(name: String) {
        TODO()
    }
    @OptIn(ExperimentalForeignApi::class)
    public infix fun getFnAddress(name: String): Either<ULong, JitFnLookupError> {
        val address = LLVMGetFunctionAddress(ref, name)
        if (address == 0uL)
            return JitFnLookupError.FnNotFound.right()
        return address.left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public infix fun getFnValue(name: String): Either<FnValue, JitFnLookupError> {
        if (!jitMode)
            return JitFnLookupError.JitNotEnabled.right()
        memScoped {
            val fnValue = alloc<LLVMValueRefVar>()
            if (LLVMFindFunction(ref, name, fnValue.ptr) == 0) {
                return FnValue(Value((fnValue.value!!))).left()
            }
        }
        return JitFnLookupError.FnNotFound.right()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun runFn(fn: FnValue, args: Array<GenericValue>) = GenericValue(LLVMRunFunction(ref, fn.valueRef, args.size.toUInt(), args.map { it.ref }.toCValues())!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun runFnAsMain(fn: FnValue, args: Array<String>, env: Array<String>): Int {
        memScoped {
            val rawArgs = args.map { it.cstr.getPointer(this) }.toCValues()
            val rawEnv = args.map { it.cstr.getPointer(this) }.toCValues()
            return LLVMRunFunctionAsMain(ref,fn.valueRef, args.size.toUInt(), rawArgs.ptr, rawEnv.ptr)
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun runStaticConstructors() = LLVMRunStaticConstructors(ref)
    @OptIn(ExperimentalForeignApi::class)
    public fun runStaticDestructors() = LLVMRunStaticDestructors(ref)
    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun linkInMcJit() = LLVMLinkInMCJIT()
        @OptIn(ExperimentalForeignApi::class)
        public fun linkInInterpreter() = LLVMLinkInInterpreter()
    }

}
enum class JitFnLookupError {
    JitNotEnabled,
    FnNotFound
}


































//