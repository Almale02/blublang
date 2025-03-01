package llvm.types

import kotlinx.cinterop.ExperimentalForeignApi
import llvm.values.Value

@OptIn(ExperimentalForeignApi::class)
class VoidType(val type: Type): Type(type.ty) {

}