@file:OptIn(ExperimentalForeignApi::class)

package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.Linkage
import llvm.types.FnType
import llvm.types.Type
import llvmc.*

class FnValue(val value: Value): Value(value.valueRef) {
    public fun getLinkage() = Linkage.fromLLVM(LLVMGetLinkage(valueRef))
    public fun setLinkage(linkage: Linkage) = LLVMSetLinkage(valueRef, linkage.toLLVM())
    public fun getNextFunction() = LLVMGetNextFunction(valueRef)?.let { FnValue(Value(it)) }
    public fun getPrevFunction() = LLVMGetPreviousFunction(valueRef)?.let { FnValue(Value(it)) }
    public fun getFirstParam() = LLVMGetFirstParam(valueRef)?.let { Value(it) }
    public fun getLastParam() = LLVMGetLastParam(valueRef)?.let { Value(it) }
    public fun getParam(idx: UInt): Value? {
        if (idx + 1u > countParams())
            return null
        return Value(LLVMGetParam(valueRef, idx)!!)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun countParams() = LLVMCountParams(valueRef)
    public fun paramsIter() = ParamsIter(this)
    override fun getType(): FnType {
        return FnType(super.getType())
    }

}
class ParamsIter(val fnValue: FnValue): Iterator<Value> {
    var idx: UInt = 0u
    override fun hasNext(): Boolean {
        return idx < fnValue.countParams()
    }
    override fun next() = fnValue.getParam(idx)!!

}