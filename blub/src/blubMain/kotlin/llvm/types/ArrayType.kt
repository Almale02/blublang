@file:OptIn(ExperimentalForeignApi::class)

package llvm.types

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.values.ArrayValue
import llvm.values.Value
import llvmc.LLVMGetArrayLength

class ArrayType(val type: Type): Type(type.ty) {
    public infix fun constArray(values: Array<ArrayValue>) = ArrayValue.newConstArray(type, values.map { it }.toTypedArray())
    @OptIn(ExperimentalForeignApi::class)
    public fun len() = LLVMGetArrayLength(ty)
    public fun isEmpty() = len() == 0u


}