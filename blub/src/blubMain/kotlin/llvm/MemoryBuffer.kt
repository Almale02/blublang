@file:OptIn(ExperimentalForeignApi::class)

package llvm

import arrow.core.Either
import arrow.core.left
import arrow.core.right
import kotlinx.cinterop.*
import llvmc.*
import okio.FileSystem
import okio.Path
import platform.posix.memcpy

class MemoryBuffer(val ref: LLVMMemoryBufferRef) {
    @OptIn(ExperimentalForeignApi::class)
    public fun createFromFile(path: Path): Either<MemoryBuffer, LLVMString> {
        memScoped {
            val buff = alloc<LLVMMemoryBufferRefVar>()
            val outMsg = alloc<CPointerVar<ByteVar>>()
            val path = path.toString()
            val retCode = LLVMCreateMemoryBufferWithContentsOfFile(path, buff.ptr, outMsg.ptr)

            if (retCode == 1)
                return LLVMString(outMsg.value!!).right()
            return MemoryBuffer(buff.value!!).left()
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun getSize() = LLVMGetBufferSize(ref)
    @OptIn(ExperimentalForeignApi::class)
    public fun getPtr() = LLVMGetBufferStart(ref)!!
    @OptIn(ExperimentalForeignApi::class)
    public fun getBytes(): ByteArray {
        val ptr = getPtr()
        val byteArray = ByteArray(getSize().toInt())
        memcpy(byteArray.refTo(0), ptr, getSize())
        return byteArray

    }
    public infix fun writeToFile(path: Path): Either<Unit, FileWriteError> {
        with(FileSystem.SYSTEM) {
            write(path, !exists(path)) {
                write(getBytes())
            }
        }
        return Unit.left()
    }
}
enum class FileWriteError {
    PathDoesntExists,
    PathIsNotFile,
}