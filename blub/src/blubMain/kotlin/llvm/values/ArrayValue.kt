@file:OptIn(ExperimentalForeignApi::class)

package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toCValues
import llvm.LLVMString
import llvm.types.ArrayType
import llvm.types.Type
import llvmc.LLVMConstArray
import llvmc.LLVMGetAsString
import llvmc.LLVMIsConstantString

class ArrayValue(val value: Value): Value(value.valueRef), AggregateValue {
    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun newConstArray(ty: Type, values: Array<Value>) =
            ArrayValue(Value(LLVMConstArray(ty.ty, values.map { it.valueRef }.toCValues(), values.size.toUInt())!!))
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun isConstString() = LLVMIsConstantString(valueRef) == 1
    @OptIn(ExperimentalForeignApi::class)
    public fun getStringConst() = LLVMGetAsString(valueRef, arrayOf<ULong>(0u).toULongArray().toCValues())?.let { LLVMString(it) }
    override fun getType(): ArrayType {
        return ArrayType(super.getType())
    }

}