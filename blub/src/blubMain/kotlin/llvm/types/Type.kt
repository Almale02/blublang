@file:OptIn(ExperimentalForeignApi::class)

package llvm.types

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import kotlinx.cinterop.toCValues
import llvm.Context
import llvm.LLVMString
import llvm.values.IntValue
import llvm.values.Value
import llvmc.*

open class Type(val ty: LLVMTypeRef) {
    public open fun constZero(): Value {
        return Value(when (LLVMGetTypeKind(ty)) {
            LLVMTypeKind.LLVMMetadataTypeKind -> LLVMConstPointerNull(ty)!!
            else -> LLVMConstNull(ty)!!
        })
    }
    public fun ptrType(addressSpace: AddressSpace) = PtrType(Type(llvmc.LLVMPointerType(ty, addressSpace.addressSpace)!!))
    public fun ptrType() = PtrType(Type(llvmc.LLVMPointerType(ty, AddressSpace.default().addressSpace)!!))
    public fun vecType(size: UInt) = VecType(Type(LLVMVectorType(ty, size)!!))
    @OptIn(ExperimentalForeignApi::class)
    public fun fnType(params: Array<Type>, isVarArg: Boolean) =
        FnType(Type(LLVMFunctionType(ty, params.map { it.ty }.toCValues(), params.size.toUInt(), isVarArg.toByte().toInt())!!))
    public fun arrayType(size: UInt) = ArrayType(Type(LLVMArrayType(ty, size)!!))
    @OptIn(ExperimentalForeignApi::class)
    public open fun getUndef() = Value(LLVMGetUndef(ty)!!)
    public fun getPoison() = Value(LLVMGetPoison(ty)!!)
    public fun getAlignment() = IntValue(Value(LLVMAlignOf(ty)!!))
    public fun isSized() = LLVMTypeIsSized(ty) == 1
    public fun sizeOf(): IntValue? {
        if (!isSized())
            return null
        return IntValue(Value(LLVMSizeOf(ty)!!))
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getContext() = Context(LLVMGetTypeContext(ty)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun printToString() = LLVMString(LLVMPrintTypeToString(ty)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getElementType(): Type  {println(""); return Type(LLVMGetElementType(ty)!!)}
}