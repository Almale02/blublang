@file:OptIn(ExperimentalForeignApi::class)

package llvm

import kotlinx.cinterop.ByteVarOf
import kotlinx.cinterop.CPointer
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString

class LLVMString(val ptr: CPointer<ByteVarOf<Byte>> ) {
    override fun toString(): String {
        return ptr.toKString()
    }
}