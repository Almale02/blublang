package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.types.IntType
import llvm.types.PtrType
import llvm.types.Type
import llvmc.*

@OptIn(ExperimentalForeignApi::class)
class IntValue(val value: Value): Value(value.valueRef) {
    public fun constNot() = IntValue(Value(LLVMConstNot(value.valueRef)!!))
    public fun constNeg() = IntValue(Value(LLVMConstNeg(value.valueRef)!!))
    public fun constNswNeg() = IntValue(Value(LLVMConstNSWNeg(value.valueRef)!!))
    public fun constNuwNeg() = IntValue(Value(LLVMConstNUWNeg(value.valueRef)!!))
    public fun constAdd( rhs: IntValue) = IntValue(Value(LLVMConstAdd(value.valueRef, rhs.value.valueRef)!!))
    public fun constNswAdd( rhs: IntValue) = IntValue(Value(LLVMConstNSWAdd(value.valueRef, rhs.value.valueRef)!!))
    public fun constNuwAdd( rhs: IntValue) = IntValue(Value(LLVMConstNUWAdd(value.valueRef, rhs.value.valueRef)!!))
    public fun constSub( rhs: IntValue) = IntValue(Value(LLVMConstSub(value.valueRef, rhs.value.valueRef)!!))
    public fun constNswSub( rhs: IntValue) = IntValue(Value(LLVMConstNSWSub(value.valueRef, rhs.value.valueRef)!!))
    public fun constNuwSub( rhs: IntValue) = IntValue(Value(LLVMConstNUWSub(value.valueRef, rhs.value.valueRef)!!))
    public fun constMul( rhs: IntValue) = IntValue(Value(LLVMConstMul(value.valueRef, rhs.value.valueRef)!!))
    public fun constNswMul( rhs: IntValue) = IntValue(Value(LLVMConstNSWMul(value.valueRef, rhs.value.valueRef)!!))
    public fun constNuwMul( rhs: IntValue) = IntValue(Value(LLVMConstNUWMul(value.valueRef, rhs.value.valueRef)!!))
    override fun getType(): IntType {
        return IntType(super.getType())
    }

}
