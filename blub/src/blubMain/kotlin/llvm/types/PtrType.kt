@file:OptIn(ExperimentalForeignApi::class)

package llvm.types

import compile.types.TypeInfo
import kotlinx.cinterop.ExperimentalForeignApi
import llvmc.LLVMInt32Type
import llvmc.LLVMPointerTypeIsOpaque
import llvmc.LLVMTypeRef

class PtrType(val type: Type): Type(type.ty) {
    @OptIn(ExperimentalForeignApi::class)
    public fun isOpaque() = LLVMPointerTypeIsOpaque(ty) != 0

}
class AddressSpace private constructor(val addressSpace: UInt){
    companion object {
        public fun default() = AddressSpace.new(0u)!!
        public fun new(num: UInt): AddressSpace? {
           return if (num < (1u shl 24)) {
               AddressSpace(num)
           } else {
               null
           }
        }
    }
}