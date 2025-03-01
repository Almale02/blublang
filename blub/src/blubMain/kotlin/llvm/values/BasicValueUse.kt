@file:OptIn(ExperimentalForeignApi::class, ExperimentalForeignApi::class)

package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.BasicBlock
import llvmc.*

class BasicValueUse(val ref: LLVMUseRef) {
    public fun getNextUse(): BasicValueUse? {
        return LLVMGetNextUse(ref)?.let { BasicValueUse(it) }
    }
    public fun getUser(): Value {
        return Value(LLVMGetUser(ref)!!)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getUsedValue(): GetOperandRes {
        val usedVal = LLVMGetUsedValue(ref)!!
        return LLVMIsABasicBlock(usedVal)?.let {
            GetOperandRes.BasicBlockOp(BasicBlock(LLVMValueAsBasicBlock(usedVal)!!))
        } ?: GetOperandRes.ValueOp(Value(usedVal)) }
}