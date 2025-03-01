@file:OptIn(ExperimentalForeignApi::class)

package llvm

import arrow.core.None
import arrow.core.Option
import arrow.core.Some
import compile.types.TypeInfo
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.values.*
import llvmc.*
import platform.windows.POINTERINACTIVEVar

class BasicBlock(val ref: LLVMBasicBlockRef) {
    public fun getParent() = LLVMGetBasicBlockParent(ref)?.let { FnValue(Value(it)) }
    public fun getPrevBasicBlock(): BasicBlock? {
        if (getParent() == null) {
            return null
        }
        return LLVMGetPreviousBasicBlock(ref)?.let { BasicBlock((it)) }
    }
    public fun getNextBasicBlock(): BasicBlock? {
        if (getParent() == null) {
            return null
        }
        return LLVMGetNextBasicBlock(ref)?.let { BasicBlock((it)) }
    }
    public infix fun moveBefore(other: BasicBlock): Unit? {
        if (getParent() == null || other.getParent() == null)
            return null
        LLVMMoveBasicBlockBefore(ref, other.ref)
        return Unit
    }
    @OptIn(ExperimentalForeignApi::class)
    public infix fun moveAfter(other: BasicBlock): Unit? {
        if (getParent() == null || other.getParent() == null)
            return null
        LLVMMoveBasicBlockAfter(ref, other.ref)
        return Unit
    }
    public fun getFirstInstruction() = LLVMGetFirstInstruction(ref)?.let { InstructionValue(Value(it)) }
    public fun getLastInstruction() = LLVMGetLastInstruction(ref)?.let { InstructionValue(Value(it)) }
    public fun getInstructionWithName(name: String) = getFirstInstruction()?.getInstructionWithName(name)
    @OptIn(ExperimentalForeignApi::class)
    public fun getTerminator() = LLVMGetBasicBlockTerminator(ref)?.let { InstructionValue(Value(it)) }
    public fun getInstructions() = object : Iterator<InstructionValue> {
        var curr = getFirstInstruction()
        override fun hasNext() =
            curr?.let { it.getNextInstruction() != null } ?: false

        override fun next(): InstructionValue {
            val temp = curr!!
            curr = temp.getNextInstruction()
            return temp
        }
    }
    public fun removeFromFn(): Option<Unit> {
        if (getParent() == null) {
            return None
        }
        LLVMRemoveBasicBlockFromParent(ref)
        return Some(Unit)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getAddr(): PtrValue? {
        if (getNextBasicBlock() == null)
            return null
        val value = PtrValue(Value(LLVMBlockAddress((getParent() ?: return null).valueRef, ref)!!))
        if (value.isNull())
            return null
        return value
    }
}