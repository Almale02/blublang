package llvm.types

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import llvm.values.IntValue
import llvm.values.Value
import llvmc.LLVMConstInt
import llvmc.LLVMGetIntTypeWidth

@Suppress("MISSING_DEPENDENCY_CLASS_IN_EXPRESSION_TYPE")
@OptIn(ExperimentalForeignApi::class)
class IntType(val type: Type): Type(type.ty) {
    public fun constInt(value: ULong, isSigned: Boolean) = IntValue(Value(LLVMConstInt(ty, value, isSigned.toByte().toInt())!!))
    public fun getIntWidth(): UInt = LLVMGetIntTypeWidth(ty)
}