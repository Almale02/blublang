package llvm.types

import compilerICE
import kotlinx.cinterop.*
import llvmc.*

@OptIn(ExperimentalForeignApi::class)
class FnType(val type: Type): Type(type.ty) {
    public fun isVarArg() = LLVMIsFunctionVarArg(ty) != 0
    public fun getParamTypes(): Array<Type> {
        memScoped {
            val count = countParamTypes()
            val params: CValues<LLVMTypeRefVar> = arrayOfNulls<LLVMTypeRef>(count.toInt()).toCValues()
            val paramsPtr = params.getPointer(this)

            LLVMGetParamTypes(ty, params)
            return Array(count.toInt()) {
               Type(paramsPtr[it]!!)
            }
        }
    }
    public fun countParamTypes() = LLVMCountParamTypes(ty)
    public fun getReturnType() = Type(LLVMGetReturnType(type.ty) ?: compilerICE("LLVMGetReturnType returned null"))
}