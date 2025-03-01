@file:OptIn(ExperimentalForeignApi::class, ExperimentalForeignApi::class, ExperimentalForeignApi::class,
    ExperimentalForeignApi::class
)

package llvm.values

import compilerICE
import kotlinx.cinterop.ByteVarOf
import kotlinx.cinterop.CPointer
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.LLVMString
import llvm.types.Type
import llvmc.*

open class Value(val valueRef: LLVMValueRef) {
    public fun asInstruction(): InstructionValue? {
        if (!isInstruction())
            return null
        return InstructionValue(this)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun isInstruction() = LLVMIsAInstruction(valueRef) != null
    public fun isNull() = LLVMIsNull(valueRef) == 1
    @OptIn(ExperimentalForeignApi::class)
    public fun isConst() = LLVMIsConstant(valueRef) == 1
    @OptIn(ExperimentalForeignApi::class)
    public open fun getName(): LLVMString? = LLVMString(LLVMGetValueName(valueRef)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun isUndef() = LLVMIsUndef(valueRef) == 1
    @OptIn(ExperimentalForeignApi::class)
    public infix fun setName(name: String) = LLVMSetValueName(valueRef, name)
    @OptIn(ExperimentalForeignApi::class)
    public open fun getType() = Type(LLVMTypeOf(valueRef) ?: compilerICE("getType return null"))
    @OptIn(ExperimentalForeignApi::class)
    public fun printToString() = LLVMString(LLVMPrintValueToString(valueRef)!!)
    public fun printToStderr() = LLVMDumpValue(valueRef)
    public fun getSection() = LLVMGetSection(valueRef)?.let { LLVMString(it) }
    public fun setSection(section: String?) = LLVMSetSection(valueRef, section)
    public fun getFirstUse() = LLVMGetFirstUse(valueRef)?.let { BasicValueUse(it) }
    public fun replaceAllUsesWith(other: Value) {
        if (valueRef != other.valueRef)
            LLVMReplaceAllUsesWith(valueRef, other.valueRef)
    }
}
sealed interface AggregateValue