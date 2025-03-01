@file:OptIn(ExperimentalForeignApi::class)

package llvm.values

import arrow.core.left
import arrow.core.right
import compilerICE
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import llvm.types.VoidType
import llvmc.*

class CallSiteValue(val value: Value) {
    public fun setTailCall(bool: Boolean) = LLVMSetTailCall(value.valueRef, bool.toByte().toInt())
    @OptIn(ExperimentalForeignApi::class)
    public fun isTailCall() = LLVMIsTailCall(value.valueRef) == 1
    public fun getTailCallKind() = TailCallKind.fromLLVM(LLVMGetTailCallKind(value.valueRef))
    public fun setTailCallKind(kind: TailCallKind) = LLVMSetTailCallKind(value.valueRef, kind.toLLVM())
    public fun getCalledFnValue() = when (value.getType() is VoidType) {
        true -> value.asInstruction()!!.right()
        false -> value.left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun callConv() = LLVMGetInstructionCallConv(value.valueRef)
}
enum class TailCallKind {
    None,
    Tail,
    MustTail,
    NoTail;

    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun fromLLVM(kind: LLVMTailCallKind) = TailCallKind.entries.find { it.ordinal.toUInt() == kind } ?: compilerICE("reached unreachable code")
    }
    public fun toLLVM() = this.ordinal.toUInt()
}