
package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.types.Type
import llvm.types.VecType
import llvmc.*

@OptIn(ExperimentalForeignApi::class)
class VectorValue(val value: Value): Value(value.valueRef) {
    public fun isConstVector() = LLVMIsAConstantVector(valueRef) != null
    public fun isConstDataVector() = LLVMIsAConstantDataVector(valueRef) != null
    public fun constExtractElement(index: IntValue) = Value(LLVMConstExtractElement(valueRef, index.valueRef)!!)
    public fun<T: Value> constInsertElement(index: IntValue, value: T) = Value(LLVMConstInsertElement(valueRef, index.valueRef, value.valueRef)!!)
    public fun constShuffleVector(right: VectorValue, mask: VectorValue) = VectorValue(Value(LLVMConstShuffleVector(valueRef, right.valueRef, mask.valueRef)!!))
    override fun getType(): VecType {
        return VecType(value.getType())
    }

}