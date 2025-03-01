@file:OptIn(ExperimentalForeignApi::class)

package llvm
import kotlinx.cinterop.*
import llvm.builder.Builder
import llvm.types.*
import llvm.values.ArrayValue
import llvm.values.FnValue
import llvm.values.StructValue
import llvm.values.Value
import llvmc.*

class Context(val contextRef: LLVMContextRef) {
    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun create(): Context? {
            return LLVMContextCreate()?.let { return Context(it) }
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun createBuilder() = Builder(LLVMCreateBuilderInContext(contextRef)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun createModule(name: String) = Module(LLVMModuleCreateWithNameInContext(name, contextRef)!!)

    @OptIn(ExperimentalForeignApi::class)
    public fun voidType() = VoidType(Type(LLVMVoidTypeInContext(contextRef)!!))
    @OptIn(ExperimentalForeignApi::class)
    public fun boolType() = IntType(Type(LLVMInt1TypeInContext(contextRef)!!))
    public fun i8Type() = IntType(Type(LLVMInt8TypeInContext(contextRef)!!))
    @OptIn(ExperimentalForeignApi::class)
    public fun i16lType() = IntType(Type(LLVMInt16TypeInContext(contextRef)!!))
    public fun i32Type() = IntType(Type(LLVMInt32TypeInContext(contextRef)!!))
    public fun i64Type() = IntType(Type(LLVMInt64TypeInContext(contextRef)!!))
    public fun i128Type() = IntType(Type(LLVMInt128TypeInContext(contextRef)!!))
    @OptIn(ExperimentalForeignApi::class)
    public fun customWidthIntType(bits: UInt) = IntType(Type(LLVMIntTypeInContext(contextRef, bits)!!))
    public fun isize(module: Module) = IntType(Type(LLVMIntPtrTypeInContext(contextRef, LLVMGetModuleDataLayout(module.moduleRef)!!)!!))
    public fun f16Type() = FloatType(Type(LLVMHalfTypeInContext(contextRef)!!))
    public fun f32Type() = FloatType(Type(LLVMFloatTypeInContext(contextRef)!!))
    @OptIn(ExperimentalForeignApi::class)
    public fun f64Type() = FloatType(Type(LLVMDoubleTypeInContext(contextRef)!!))
    public fun ptrType(addressSpace: AddressSpace) = PtrType(Type(LLVMPointerTypeInContext(contextRef, addressSpace.addressSpace)!!))
    public fun ptrType() = PtrType(Type(LLVMPointerTypeInContext(contextRef, AddressSpace.default().addressSpace)!!))
    public fun structType(fields: Array<Type>, packed: Boolean, name: String): StructType {
        val struct = LLVMStructCreateNamed(contextRef, name)!!
        memScoped {
            LLVMStructSetBody(struct, allocArray(fields.size) {fields[it].ty}, fields.size.toUInt(), packed.toByte().toInt())
        }
        return StructType(Type(struct))
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun constStructType(values: Array<Value>, packed: Boolean) = StructValue(Value(LLVMConstStructInContext(
        contextRef,
        values.map { it.valueRef }.toCValues(),
        values.size.toUInt(),
        packed.toByte().toInt()
    )!!))
    public infix fun structType(name: String): StructType {
        val struct = LLVMStructCreateNamed(contextRef, name)!!
        return StructType(Type(struct))
    }
    public fun anonymousStructType(fields: Array<Type>, packed: Boolean) = memScoped {  StructType(Type(
        LLVMStructTypeInContext(
            contextRef,
            fields.map { it.ty }.toCValues().getPointer(this),
            fields.size.toUInt(),
            packed.toByte().toInt()
        )!!))}
    @OptIn(ExperimentalForeignApi::class)
    public fun getStructType(name: String) = LLVMGetTypeByName2(contextRef, name)?.let { StructType(Type(it)) }
    @OptIn(ExperimentalForeignApi::class)
    public fun appendBasicBlock(fn: FnValue, name: String) = BasicBlock(LLVMAppendBasicBlockInContext(contextRef, fn.valueRef, name)!!)
    public fun insertBlockAfter(prev: BasicBlock, name: String) = prev.getNextBasicBlock()
        ?.let { prependBasicBlock(prev, name) }
        ?: appendBasicBlock(prev.getParent()!!, name)
    @OptIn(ExperimentalForeignApi::class)
    public fun prependBasicBlock(block: BasicBlock, name: String) = BasicBlock(LLVMInsertBasicBlockInContext(contextRef, block.ref, name)!!)
    public fun constString(string: String, nullTerminated: Boolean) = ArrayValue(Value(LLVMConstStringInContext(contextRef, string, string.length.toUInt(), nullTerminated.toByte().toInt())!!))
}
