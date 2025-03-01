@file:OptIn(ExperimentalForeignApi::class)

package llvm.values

import compilerICE
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.BasicBlock
import llvm.LLVMString
import llvm.types.VoidType
import llvmc.*

enum class IntPredicate {
    // Equal
    EQ,
    // Not Equal
    NE,
    // Unsigned Greater Than
    UGT,
    // Unsigned Greater Than or Equal
    UGE,
    // Unsigned Less Than
    ULT,
    // Unsigned Less Than or Equal
    ULE,
    // Signed Greater Than
    SGT,
    // Signed Greater Than or Equal
    SGE,
    // Signed Less Than
    SLT,
    // Signed Less Than or Equal
    SLE;

    companion object {
        public fun fromUInt(num: UInt) = IntPredicate.entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
    }
    public fun toUInt() = this.ordinal.toUInt()
}
enum class FloatPredicate {
    // Returns true if `left` == `right` and neither are NaN
    OEQ,
    // Returns true if `left` >= `right` and neither are NaN
    OGE,
    // Returns true if `left` > `right` and neither are NaN
    OGT,
    // Returns true if `left` <= `right` and neither are NaN
    OLE,
    // Returns true if `left` < `right` and neither are NaN
    OLT,
    // Returns true if `left` != `right` and neither are NaN
    ONE,
    // Returns true if neither value is NaN
    ORD,
    // Always returns false
    PredicateFalse,
    // Always returns true
    PredicateTrue,
    // Returns true if `left` == `right` or either is NaN
    UEQ,
    // Returns true if `left` >= `right` or either is NaN
    UGE,
    // Returns true if `left` > `right` or either is NaN
    UGT,
    // Returns true if `left` <= `right` or either is NaN
    ULE,
    // Returns true if `left` < `right` or either is NaN
    ULT,
    // Returns true if `left` != `right` or either is NaN
    UNE,
    // Returns true if either value is NaN
    UNO;


    companion object {
        //public fun fromUInt(num: UInt) = FloatPredicate.entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
        @OptIn(ExperimentalForeignApi::class)
        public fun fromLLVM(predicate: LLVMRealPredicate) = when (predicate) {
            LLVMRealPredicate.LLVMRealPredicateFalse -> PredicateFalse
            LLVMRealPredicate.LLVMRealOEQ -> OEQ
            LLVMRealPredicate.LLVMRealOGT -> OGT
            LLVMRealPredicate.LLVMRealOGE -> OGE
            LLVMRealPredicate.LLVMRealOLT -> OLT
            LLVMRealPredicate.LLVMRealOLE -> OLE
            LLVMRealPredicate.LLVMRealONE -> ONE
            LLVMRealPredicate.LLVMRealORD -> ORD
            LLVMRealPredicate.LLVMRealUNO -> UNO
            LLVMRealPredicate.LLVMRealUEQ -> UEQ
            LLVMRealPredicate.LLVMRealUGT -> UGT
            LLVMRealPredicate.LLVMRealUGE -> UGE
            LLVMRealPredicate.LLVMRealULT -> ULT
            LLVMRealPredicate.LLVMRealULE -> ULE
            LLVMRealPredicate.LLVMRealUNE -> UNE
            LLVMRealPredicate.LLVMRealPredicateTrue -> PredicateTrue
        }
        public fun fromUInt(num: UInt) = FloatPredicate.entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
    }
    public fun toLLVM() = when (this) {
        OEQ -> LLVMRealPredicate.LLVMRealOEQ
        OGE -> LLVMRealPredicate.LLVMRealOGE
        OGT -> LLVMRealPredicate.LLVMRealOGT
        OLE -> LLVMRealPredicate.LLVMRealOLE
        OLT -> LLVMRealPredicate.LLVMRealOLT
        ONE -> LLVMRealPredicate.LLVMRealONE
        ORD -> LLVMRealPredicate.LLVMRealORD
        PredicateFalse -> LLVMRealPredicate.LLVMRealPredicateFalse
        PredicateTrue -> LLVMRealPredicate.LLVMRealPredicateTrue
        UEQ -> LLVMRealPredicate.LLVMRealUEQ
        UGE -> LLVMRealPredicate.LLVMRealUGE
        UGT -> LLVMRealPredicate.LLVMRealUGT
        ULE -> LLVMRealPredicate.LLVMRealULE
        ULT -> LLVMRealPredicate.LLVMRealULT
        UNE -> LLVMRealPredicate.LLVMRealUNE
        UNO -> LLVMRealPredicate.LLVMRealUNO
    }

}
enum class InstructionOpcode {
    Add,
    AddrSpaceCast,
    Alloca,
    And,
    AShr,
    AtomicCmpXchg,
    AtomicRMW,
    BitCast,
    Br,
    Call,
    CallBr,
    CatchPad,
    CatchRet,
    CatchSwitch,
    CleanupPad,
    CleanupRet,
    ExtractElement,
    ExtractValue,
    FNeg,
    FAdd,
    FCmp,
    FDiv,
    Fence,
    FMul,
    FPExt,
    FPToSI,
    FPToUI,
    FPTrunc,
    Freeze,
    FRem,
    FSub,
    GetElementPtr,
    ICmp,
    IndirectBr,
    InsertElement,
    InsertValue,
    IntToPtr,
    Invoke,
    LandingPad,
    Load,
    LShr,
    Mul,
    Or,
    Phi,
    PtrToInt,
    Resume,
    Return,
    SDiv,
    Select,
    SExt,
    Shl,
    ShuffleVector,
    SIToFP,
    SRem,
    Store,
    Sub,
    Switch,
    Trunc,
    UDiv,
    UIToFP,
    Unreachable,
    URem,
    UserOp1,
    UserOp2,
    VAArg,
    Xor,
    ZExt;

    companion object {
        public fun fromUInt(num: UInt) = entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
    }
    public fun toUInt() = this.ordinal.toUInt()
}

class InstructionValue(val value: Value): Value(value.valueRef) {
    @OptIn(ExperimentalForeignApi::class)
    public fun isALoadInst(): Boolean {
        return LLVMIsALoadInst(value.valueRef) != null
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun isAStoreInst(): Boolean {
        return LLVMIsAStoreInst(value.valueRef) != null
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun isAAllocaInst(): Boolean {
        return LLVMIsAAllocaInst(value.valueRef) != null
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun isAGetElementPtrInst(): Boolean {
        return LLVMIsAGetElementPtrInst(value.valueRef) != null
    }
    public fun isAAtomicRMWInst(): Boolean {
        return LLVMIsAAtomicRMWInst(value.valueRef) != null
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun isAAtomicCmpXchgInst(): Boolean {
        return LLVMIsAAtomicCmpXchgInst(value.valueRef) != null
    }
    public fun explicitClone(): InstructionValue {
        return InstructionValue(Value(LLVMInstructionClone(value.valueRef)!!))
    }
    public override fun getName(): LLVMString? {
        if (getType() is VoidType) {
            return null
        }
        return value.getName()
    }
    public fun getInstructionWithName(name: String): InstructionValue? {
        getName().let {
            if (it.toString() == name) {
                return this
            }
        }
        return getNextInstruction()?.getInstructionWithName(name)
    }
    public fun getPrevInstruction() = LLVMGetPreviousInstruction(valueRef)?.let { InstructionValue(Value(it)) }
    @OptIn(ExperimentalForeignApi::class)
    public fun getNextInstruction() = LLVMGetNextInstruction(valueRef)?.let { InstructionValue(Value(it)) }
    public fun getOpcode() = InstructionOpcode.fromUInt(LLVMGetInstructionOpcode(valueRef))
    public fun eraseFromBasicBlock() = LLVMInstructionEraseFromParent(valueRef)
    public fun removeFromBasicBlock() = LLVMInstructionRemoveFromParent(valueRef)
    @OptIn(ExperimentalForeignApi::class)
    public fun getParent() = LLVMGetInstructionParent(valueRef)?.let { BasicBlock(it) }
    @OptIn(ExperimentalForeignApi::class)
    public fun isTerminator() = LLVMIsATerminatorInst(valueRef) != null
    @OptIn(ExperimentalForeignApi::class)
    public fun isConditional(): Boolean {
        if (isTerminator())
            return LLVMIsConditional(valueRef) == 1
        return false
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun canUseFastMathFlags() = LLVMCanValueUseFastMathFlags(valueRef) == 1
    public fun getFastMathFlags(): UInt? {
        if (canUseFastMathFlags())
            return LLVMGetFastMathFlags(valueRef)
        return null
    }
    public fun setFastMathFlags(flags: UInt) {
        if (canUseFastMathFlags())
            LLVMSetFastMathFlags(valueRef, flags)
    }
    public fun getNumOperands() = LLVMGetNumOperands(valueRef)
    @OptIn(ExperimentalForeignApi::class)
    public fun getOperand(idx: UInt) = LLVMGetOperand(valueRef, idx)?.let { operand ->
        if (LLVMIsABasicBlock(operand) != null) {
            GetOperandRes.BasicBlockOp(BasicBlock(LLVMValueAsBasicBlock(operand)!!))
        }
        GetOperandRes.ValueOp(Value(operand))
    }
    public fun getOperands(): Iterator<GetOperandRes> {
        return object : Iterator<GetOperandRes> {
            var idx: UInt = 0u
            override fun hasNext() = idx < getNumOperands().toUInt()

            override fun next(): GetOperandRes = getOperand(idx++)!!
        }
    }

    public fun setOperand(idx: UInt, opVal: Value): Boolean {
        if (idx.toInt() >= getNumOperands())
            return false
        LLVMSetOperand(valueRef, idx, opVal.valueRef)
        return true
    }
    public fun getOperandUse(idx: UInt): BasicValueUse? {
        if (idx.toInt() >= getNumOperands())
            return null
        return LLVMGetOperandUse(valueRef, idx)?.let { BasicValueUse(it) }
    }
    public fun getIcmpPredicate(): IntPredicate? {
        if (getOpcode() == InstructionOpcode.ICmp)
            return IntPredicate.fromUInt(LLVMGetICmpPredicate(valueRef))
        return null
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getFcmpPredicate(): FloatPredicate? {
        if (getOpcode() == InstructionOpcode.ICmp)
            return FloatPredicate.fromUInt(LLVMGetICmpPredicate(valueRef))
        return null
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun hasMetadata() = LLVMHasMetadata(valueRef) == 1

}
sealed class GetOperandRes {
    data class ValueOp(val value: Value): GetOperandRes()
    data class BasicBlockOp(val block: BasicBlock): GetOperandRes()
}
