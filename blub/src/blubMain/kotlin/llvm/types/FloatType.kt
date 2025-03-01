package llvm.types

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.values.FloatValue
import llvm.values.Value
import llvmc.LLVMConstReal

@OptIn(ExperimentalForeignApi::class)
class FloatType(val type: Type): Type(type.ty) {
    public fun constFloat(value: Double) = FloatValue(Value(LLVMConstReal(ty, value)!!))
}