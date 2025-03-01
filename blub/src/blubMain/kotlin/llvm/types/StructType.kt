package llvm.types

import kotlinx.cinterop.*
import llvm.values.StructValue
import llvm.values.Value
import llvmc.*

@OptIn(ExperimentalForeignApi::class)
class StructType(val type: Type): Type(type.ty) {
    public infix fun getFieldTypeAtIndex(idx: UInt): Type? {
        if (isOpaque())
            return null
        if (idx >= countFields())
            return null
        return Type(LLVMStructGetTypeAtIndex(ty, idx)!!)
    }
    public fun getFieldTypes(): Array<Type> {
        memScoped {
            val ref = allocArray<LLVMTypeRefVar>(countFields().toInt())
            LLVMGetStructElementTypes(ty, ref.getPointer(this))
            val typeArray = Array(countFields().toInt())  { idx ->
                Type(ref.getPointer(this)[idx]!!)
            }
            return typeArray
        }
    }
    public infix fun constNamedStruct(values: Array<Value>) =
        StructValue(Value(LLVMConstNamedStruct(ty, values.map { it.valueRef }.toCValues(), values.size.toUInt())!!))
    public fun setBody(fieldTypes: Array<Type>, packed: Boolean): Boolean {
        val isOpaque = isOpaque()
        memScoped {
            LLVMStructSetBody(ty, fieldTypes.map { it.ty }.toCValues().getPointer(this), fieldTypes.size.toUInt(), packed.toByte().toInt())
        }
        return isOpaque
    }
    override fun constZero() = StructValue(type.constZero())
    override fun getUndef(): StructValue {
        return StructValue(super.getUndef())
    }
    public fun isOpaque() = LLVMIsOpaqueStruct(ty) == 1
    public fun isPacked() = LLVMIsPackedStruct(ty)
    public fun countFields() = LLVMCountStructElementTypes(ty)

}