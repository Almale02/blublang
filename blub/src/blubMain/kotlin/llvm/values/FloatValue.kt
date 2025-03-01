package llvm.values

import kotlinx.cinterop.*
import llvm.types.FloatType
import llvm.types.Type
import llvmc.LLVMConstRealGetDouble

@OptIn(ExperimentalForeignApi::class)
class FloatValue(val value: Value): Value(value.valueRef) {
    public fun getConstant(): Pair<Double, Boolean>? {
        if (!isConst())
            return null
        memScoped {
            val lossy = alloc<IntVar>()

            val constant = LLVMConstRealGetDouble(valueRef, lossy.ptr)
            return Pair(constant, lossy.value == 1)
        }
    }

    override fun getType(): FloatType {
        return FloatType(super.getType())
    }
}