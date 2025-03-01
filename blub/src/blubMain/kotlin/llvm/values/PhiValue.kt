@file:OptIn(ExperimentalForeignApi::class)

package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toCValues
import llvm.BasicBlock
import llvmc.LLVMAddIncoming
import llvmc.LLVMCountIncoming
import llvmc.LLVMGetIncomingBlock
import llvmc.LLVMGetIncomingValue

class PhiValue(val value: Value): Value(value.valueRef) {
    @OptIn(ExperimentalForeignApi::class)
    public fun addIncoming(incoming: Array<Pair<Value, BasicBlock>>) = LLVMAddIncoming(
        valueRef,
        incoming.map { it.first.valueRef }.toCValues(),
        incoming.map { it.second.ref }.toCValues(),
        incoming.size.toUInt()
    )
    @OptIn(ExperimentalForeignApi::class)
    public fun countIncoming() = LLVMCountIncoming(valueRef)
    @OptIn(ExperimentalForeignApi::class)
    public fun getIncoming(idx: UInt): Pair<Value, BasicBlock>? {
        if (idx >= countIncoming())
            return null
        return Pair(
            Value(LLVMGetIncomingValue(valueRef, idx)!!),
            BasicBlock(LLVMGetIncomingBlock(valueRef, idx)!!),
        )
    }
    public fun getIncomings() = object : Iterator<Pair<Value, BasicBlock>> {
        var idx = 0u
        override fun hasNext(): Boolean {
            return countIncoming() > idx
        }

        override fun next(): Pair<Value, BasicBlock> = getIncoming(idx++)!!
    }
}