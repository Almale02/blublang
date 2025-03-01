@file:OptIn(ExperimentalForeignApi::class, ExperimentalForeignApi::class)

package llvm.builder

import arrow.core.*
import compile.lexer.Token
import compilerICE
import compilerICEWarning
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import kotlinx.cinterop.toCValues
import llvm.BasicBlock
import llvm.types.*
import llvm.values.*
import llvmc.*
import platform.windows.POINTERINACTIVEVar
import kotlin.math.ln

enum class BuilderError {
    UnsetPos,
    AlignmentError,
    ExtractOutOfRange,
    BitwidthError,
    PointeeTypeMismatch,
    ValueTypeMismatch,
    PointeeAndValueTypeMismatch,
    OrderingError,
    GEPPointee,
    GEPIndex
}

class Builder @OptIn(ExperimentalForeignApi::class) constructor(val builderRef: LLVMBuilderRef) {
    var isPosSet = false

    @OptIn(ExperimentalForeignApi::class)
    public fun buildReturn(value: Value): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildRet(builderRef, value.valueRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildReturn(): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildRetVoid(builderRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildAggregateReturn(values: Array<Value>): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildAggregateRet(builderRef, values.map { it.valueRef }.toCValues(), values.size.toUInt())!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildCall(fn: FnValue, fnType: FnType, args: Array<Value>, name: String): Either<CallSiteValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()

        /*val name = when (LLVMGetTypeKind(fn.getType().getReturnType().ty)) {
            LLVMTypeKind.LLVMVoidTypeKind -> ""
            else -> name
        }*/
        return CallSiteValue(Value(LLVMBuildCall2(builderRef, fnType.ty, fn.valueRef, args.map { it.valueRef }.toCValues(), args.size.toUInt(), name) ?: compilerICE("LLVMBuildCall2 return null"))).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildInvoke(fn: FnValue, args: Array<Value>, thenBlock: BasicBlock, catchBlock: BasicBlock, name: String): Either<CallSiteValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        val name = when (fn.getType().getReturnType()) {
            is VoidType -> ""
            else -> name
        }
        return CallSiteValue(Value(LLVMBuildInvoke2(builderRef, fn.getType().getElementType().ty, fn.valueRef, args.map { it.valueRef }.toCValues(), args.size.toUInt(), thenBlock.ref, catchBlock.ref, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildLandingPad(errorType: Type, personalityFn: FnValue, clauses: Array<Value>, isCleanup: Boolean, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        val value = LLVMBuildLandingPad(builderRef, errorType.ty, personalityFn.valueRef, clauses.size.toUInt(), name)!!

        for (clause in clauses)
            LLVMAddClause(value, clause.valueRef)
        LLVMSetCleanup(value, isCleanup.toByte().toInt())
        return Value(value).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildResume(value: Value): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildResume(builderRef, value.valueRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildGep( ptr: PtrValue, orderedIndexes: Array<IntValue>, inBounds: Boolean, name: String): Either<PtrValue, BuilderError>{
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (inBounds) {
            return PtrValue(Value(LLVMBuildInBoundsGEP2(builderRef, ptr.getType().getElementType().ty, ptr.valueRef, orderedIndexes.map { it.valueRef }.toCValues(), orderedIndexes.size.toUInt(), name)!!)).left()
        }
        return PtrValue(Value(LLVMBuildGEP2(builderRef, ptr.getType().getElementType().ty, ptr.valueRef, orderedIndexes.map { it.valueRef }.toCValues(), orderedIndexes.size.toUInt(), name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildStructGep(ptr: PtrValue, idx: UInt, name: String): Either<PtrValue, BuilderError>{
        val ptrElementType = ptr.getType().getElementType()
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (ptrElementType !is StructType)
            return BuilderError.GEPPointee.right()
        if (idx >= ptrElementType.countFields())
            return BuilderError.GEPIndex.right()
        return PtrValue(Value(LLVMBuildStructGEP2(builderRef, ptrElementType.ty, ptr.valueRef, idx, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildPtrDiff(lhsPtr: PtrValue, rhsPtr: PtrValue, name: String): Either<IntValue, BuilderError>{
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (lhsPtr.getType().getElementType() != rhsPtr.getType().getElementType())
            return BuilderError.PointeeTypeMismatch.right()
        val ptrElemType = lhsPtr.getType().getElementType()
        return IntValue(Value(LLVMBuildPtrDiff2(builderRef, lhsPtr.getType().getElementType().ty, lhsPtr.valueRef, rhsPtr.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildPhi(type: Type, name: String): Either<PhiValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return PhiValue(Value(LLVMBuildPhi(builderRef, type.ty, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildStore(ptr: PtrValue, value: Value): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (ptr.getType().getElementType() != value.getType())
            return BuilderError.PointeeAndValueTypeMismatch.right()
        return InstructionValue(Value(LLVMBuildStore(builderRef, value.valueRef, ptr.valueRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildLoad(ptr: PtrValue, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return Value(LLVMBuildLoad2(builderRef, ptr.getType().getElementType().ty, ptr.valueRef, name)!!).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildAlloca(type: Type, name: String): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return PtrValue(Value(LLVMBuildAlloca(builderRef, type.ty, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildArrayAlloca(type: Type, size: IntValue, name: String): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return PtrValue(Value(LLVMBuildArrayAlloca(builderRef, type.ty, size.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildMemcpy(dest: PtrValue, destAlignBytes: UInt, src: PtrValue, srcAlignBytes: UInt, size: IntValue): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (!destAlignBytes.isAlignmentOk()) {
            compilerICEWarning("dest align bytes to buildMemcpy is not power of 2")
            return BuilderError.AlignmentError.right()
        }
        if (!srcAlignBytes.isAlignmentOk()) {
            compilerICEWarning("src align bytes to buildMemcpy is not power of 2")
            return BuilderError.AlignmentError.right()
        }
        return PtrValue(Value(LLVMBuildMemCpy(builderRef, dest.valueRef, destAlignBytes, src.valueRef, srcAlignBytes, size.valueRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildMemmove(dest: PtrValue, destAlignBytes: UInt, src: PtrValue, srcAlignBytes: UInt, size: IntValue): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (!destAlignBytes.isAlignmentOk()) {
            compilerICEWarning("dest align bytes to buildMemmove is not power of 2")
            return BuilderError.AlignmentError.right()
        }
        if (!srcAlignBytes.isAlignmentOk()) {
            compilerICEWarning("src align bytes to buildMemmove is not power of 2")
            return BuilderError.AlignmentError.right()
        }
        return PtrValue(Value(LLVMBuildMemMove(builderRef, dest.valueRef, destAlignBytes, src.valueRef, srcAlignBytes, size.valueRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildMemset(dest: PtrValue, destAlignBytes: UInt, value: IntValue, size: IntValue): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (!destAlignBytes.isAlignmentOk()) {
            compilerICEWarning("dest align bytes to buildMemset is not power of 2")
            return BuilderError.AlignmentError.right()
        }
        return PtrValue(Value(LLVMBuildMemSet(builderRef, dest.valueRef, value.valueRef, size.valueRef, destAlignBytes)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildMalloc(ty: Type, name: String): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (!ty.isSized()) {
            compilerICEWarning("cannot build malloc call for an unsized type")
            return BuilderError.AlignmentError.right()
        }
        return PtrValue(Value(LLVMBuildMalloc(builderRef, ty.ty, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildArrayMalloc(ty: Type, size: IntValue, name: String): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        if (!ty.isSized()) {
            compilerICEWarning("cannot build array malloc call for an unsized type")
            return BuilderError.AlignmentError.right()
        }
        return PtrValue(Value(LLVMBuildArrayMalloc(builderRef, ty.ty, size.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public infix fun buildFree(ptr: PtrValue): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildFree(builderRef, ptr.valueRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun insertInstruction(instructionValue: InstructionValue, name: String) {
        LLVMInsertIntoBuilderWithName(builderRef, instructionValue.valueRef, name)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun insertInstruction(instructionValue: InstructionValue) {
        LLVMInsertIntoBuilder(builderRef, instructionValue.valueRef)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getInsertBlock() = LLVMGetInsertBlock(builderRef)?.let { BasicBlock(it) }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntAdd(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildAdd(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFloatAdd(lhs: FloatValue, rhs: FloatValue, name: String): Either<FloatValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return FloatValue(Value(LLVMBuildFAdd(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntSub(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildSub(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFloatSub(lhs: FloatValue, rhs: FloatValue, name: String): Either<FloatValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return FloatValue(Value(LLVMBuildFSub(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()

    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntMul(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildMul(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFloatMul(lhs: FloatValue, rhs: FloatValue, name: String): Either<FloatValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return FloatValue(Value(LLVMBuildFMul(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()

    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntUnsignedDiv(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildUDiv(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntSignedDiv(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildSDiv(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFloatDiv(lhs: FloatValue, rhs: FloatValue, name: String): Either<FloatValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return FloatValue(Value(LLVMBuildFDiv(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildXor(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildXor(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildAnd(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildAnd(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildOr(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildOr(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildOr(value: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildNot(builderRef, value.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildLeftShift(lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildShl(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildRightShift(lhs: IntValue, rhs: IntValue, singExtended: Boolean, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return if (singExtended) {
            IntValue(Value(LLVMBuildAShr(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
        } else {
            IntValue(Value(LLVMBuildLShr(builderRef, lhs.valueRef, rhs.valueRef, name)!!)).left()
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildBinop(op: InstructionOpcode, lhs: Value, rhs: Value, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return Value(LLVMBuildBinOp(builderRef, op.toUInt(), lhs.valueRef, rhs.valueRef, name)!!).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildCast(op: InstructionOpcode, fromVal: Value, toType: Type, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return Value(LLVMBuildCast(builderRef, op.toUInt(), fromVal.valueRef, toType.ty, name)!!).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildPtrCast(from: PtrValue, to: PtrType, name: String): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return PtrValue(Value(LLVMBuildPointerCast(builderRef, from.valueRef, to.ty, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntCompare(op: IntPredicate, lhs: IntValue, rhs: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildICmp(builderRef, op.toUInt(), lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFloatCompare(op: FloatPredicate, lhs: FloatValue, rhs: FloatValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildFCmp(builderRef, op.toLLVM(), lhs.valueRef, rhs.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildUnconditionalBranch(destBlock: BasicBlock): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildBr(builderRef, destBlock.ref)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildConditionalBranch(comp: IntValue, thenBlock: BasicBlock, elseBlock: BasicBlock): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildCondBr(builderRef, comp.valueRef, thenBlock.ref, elseBlock.ref)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIndirectBranch(address: Value, dests: Array<BasicBlock>): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        val value = LLVMBuildIndirectBr(builderRef, address.valueRef, dests.size.toUInt())!!
        dests.forEach {
            LLVMAddDestination(value, it.ref)
        }
        return InstructionValue(Value(value)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIntNeg(value: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildNeg(builderRef, value.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFloatNeg(value: FloatValue, name: String): Either<FloatValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return FloatValue(Value(LLVMBuildFNeg(builderRef, value.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildNot(value: IntValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildNot(builderRef, value.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun setAt(block: BasicBlock, instructionValue: InstructionValue) {
        isPosSet = true
        LLVMPositionBuilder(builderRef, block.ref, instructionValue.valueRef)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun setBefore(instructionValue: InstructionValue) {
        isPosSet = true
        LLVMPositionBuilderBefore(builderRef, instructionValue.valueRef)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun setAtEnd(block: BasicBlock) {
        isPosSet = true
        LLVMPositionBuilderAtEnd(builderRef, block.ref)
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun <T> buildExtractValue(value: T, idx: UInt, name: String): Either<Value, BuilderError> where T: Value, T: AggregateValue {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        val size = when (value) {
            is ArrayValue -> value.getType().len()
            is StructValue -> value.getType().countFields()
            else -> {compilerICE("unreachable")}
        }
        if (idx >= size)
            return BuilderError.ExtractOutOfRange.right()
        return Value(LLVMBuildExtractValue(builderRef, value.valueRef, idx, name)!!).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildExtractElement(vectorValue: VectorValue, index: IntValue, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return Value(LLVMBuildExtractElement(builderRef, vectorValue.valueRef, index.valueRef, name)!!).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildInsertElement(vector: VectorValue, value: Value, index: IntValue, name: String): Either<VectorValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return VectorValue(Value(LLVMBuildInsertElement(builderRef, vector.valueRef, value.valueRef, index.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildUnreachable(): Either<InstructionValue, BuilderError>  {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildUnreachable(builderRef)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildFence(atomicOrdering: AtomicOrdering, isSingleThreaded: Boolean, name: String): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return InstructionValue(Value(LLVMBuildFence(builderRef, atomicOrdering.ordinal.toUInt(), isSingleThreaded.toByte().toInt(), name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIsNull(ptr: PtrValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildIsNull(builderRef, ptr.valueRef, name)!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildIsNotNull(ptr: PtrValue, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildIsNotNull(builderRef, ptr.valueRef, name)!!)).left()
    }
    public fun buildIntToPtr(int: IntValue, ptrType: PtrType, name: String): Either<PtrValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return PtrValue(Value(LLVMBuildIntToPtr(builderRef, int.valueRef, ptrType.ty, name)!!)) .left()
    }
    public fun buildPtrToInt(ptr: PtrValue, intType: IntType, name: String): Either<IntValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return IntValue(Value(LLVMBuildPtrToInt(builderRef, ptr.valueRef, intType.ty, name)!!)).left()
    }
    public fun clearInstructionPos() = LLVMClearInsertionPosition(builderRef)
    public fun buildSwitch(value: IntValue, elseBlock: BasicBlock, cases: Array<Pair<IntValue, BasicBlock>>): Either<InstructionValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        val switchVal = LLVMBuildSwitch(builderRef, value.valueRef, elseBlock.ref, cases.size.toUInt())

        for (case in cases) {
            LLVMAddCase(switchVal, case.first.valueRef, case.second.ref)
        }
        return InstructionValue(Value(switchVal!!)).left()
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun buildSelect(condition: IntValue, then: Value, otherwise: Value, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return Value(LLVMBuildSelect(builderRef, condition.valueRef, then.valueRef, otherwise.valueRef, name)!!).left()
    }
    public fun buildGlobalString(value: String, name: String) : Either<GlobalValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return GlobalValue(Value(LLVMBuildGlobalString(builderRef, value, name)!!)).left()
    }
    public fun buildGlobalStringPtr(value: String, name: String) : Either<GlobalValue, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return GlobalValue(Value(LLVMBuildGlobalStringPtr(builderRef, value, name)!!)).left()
    }
    public fun buildVarArgs(list: PtrValue, type: Type, name: String): Either<Value, BuilderError> {
        if (!isPosSet)
            return BuilderError.UnsetPos.right()
        return Value(LLVMBuildVAArg(builderRef, list.valueRef, type.ty, name)!!).left()
    }
}
fun UInt.isAlignmentOk(): Boolean {
    return this > 0u &&
            this.countOneBits() == 1 &&
            (ln(this.toDouble()) / ln(2.0)) < 64.0
}
enum class AtomicOrdering {
    NotAtomic,
    Unordered,
    Monotonic,
    Acquire,
    Release,
    AcquireRelease,
    SequentiallyConsistent;

    companion object {
        public fun fromUInt(order: UInt) = AtomicOrdering.entries.firstOrNull {it.ordinal.toUInt() == order}!!
    }
}


























//