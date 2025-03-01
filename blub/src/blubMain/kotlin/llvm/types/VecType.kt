@file:OptIn(ExperimentalForeignApi::class)
@file:Suppress("NAME_SHADOWING")

package llvm.types

import kotlinx.cinterop.*
import llvm.values.Value
import llvm.values.VectorValue
import llvmc.*

class VecType(val type: Type): Type(type.ty) {
    public fun getSize() = LLVMGetVectorSize(ty)
    public fun<T: Value> constVector(values: Array<T>): VectorValue {
        val values = values.map {it.valueRef}.toCValues()

        memScoped {

            return VectorValue(Value(LLVMConstVector(values, values.size.toUInt())!!))
        }
    }
}