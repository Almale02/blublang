package llvm.values

import compilerICE
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.types.StructType
import llvm.types.Type
import llvmc.LLVMGetOperand
import llvmc.LLVMSetOperand

@OptIn(ExperimentalForeignApi::class)
class StructValue(val value: Value): Value(value.valueRef), AggregateValue {
    public infix fun getFieldAtIdx(idx: UInt): Value? {
        if (idx >= getType().countFields())
            return null
        return Value(LLVMGetOperand(valueRef, idx)!!)
    }
    public fun fieldsValueIter() = StructFieldsValueIter(this)
    public fun setFieldAtIdx(idx: UInt, setValue: Value): Boolean {
        (getType().getFieldTypeAtIndex(idx))?.let {
            if (it != setValue.getType())
                return false
        } ?: return false
        LLVMSetOperand(valueRef, idx, setValue.valueRef)
        return true
    }

    override fun getType(): StructType {
        return StructType(value.getType())
    }
}
class StructFieldsValueIter(val structValue: StructValue): Iterator<Value> {
    var idx: UInt = 0u
    override fun hasNext() = idx < structValue.getType().countFields()

    override fun next() = structValue.getFieldAtIdx(idx++)!!
}