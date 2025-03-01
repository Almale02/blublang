package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import llvm.types.FloatType
import llvmc.*

class GenericValue @OptIn(ExperimentalForeignApi::class) constructor(val ref: LLVMGenericValueRef) {
    @OptIn(ExperimentalForeignApi::class)
    public fun intWidth() = LLVMGenericValueIntWidth(ref)
    @OptIn(ExperimentalForeignApi::class)
    public fun asInt(isSigned: Boolean) = LLVMGenericValueToInt(ref, isSigned.toByte().toInt())
    @OptIn(ExperimentalForeignApi::class)
    public fun asFloat(floatType: FloatType) = LLVMGenericValueToFloat(floatType.ty, ref)
    @OptIn(ExperimentalForeignApi::class)
    public fun intoPtr() = LLVMGenericValueToPointer(ref)
}