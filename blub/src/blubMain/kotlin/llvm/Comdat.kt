@file:OptIn(ExperimentalForeignApi::class)

package llvm

import compilerICE
import kotlinx.cinterop.ExperimentalForeignApi
import llvmc.LLVMComdatRef
import llvmc.LLVMComdatSelectionKind
import llvmc.LLVMGetComdatSelectionKind
import llvmc.LLVMSetComdatSelectionKind

enum class ComdatSelectionKind {
    Any,
    ExactMatch,
    Largest,
    NoDuplicates,
    SameSize;
    companion object {
        public fun fromLLVM(kind: LLVMComdatSelectionKind) = when (kind) {
            LLVMComdatSelectionKind.LLVMAnyComdatSelectionKind -> Any
            LLVMComdatSelectionKind.LLVMExactMatchComdatSelectionKind -> ExactMatch
            LLVMComdatSelectionKind.LLVMLargestComdatSelectionKind -> Largest
            LLVMComdatSelectionKind.LLVMNoDeduplicateComdatSelectionKind -> NoDuplicates
            LLVMComdatSelectionKind.LLVMSameSizeComdatSelectionKind -> SameSize
        }
    }
    public fun toLLVM() = when (this) {
        Any -> LLVMComdatSelectionKind.LLVMAnyComdatSelectionKind
        ExactMatch -> LLVMComdatSelectionKind.LLVMExactMatchComdatSelectionKind
        Largest -> LLVMComdatSelectionKind.LLVMLargestComdatSelectionKind
        NoDuplicates -> LLVMComdatSelectionKind.LLVMNoDeduplicateComdatSelectionKind
        SameSize -> LLVMComdatSelectionKind.LLVMSameSizeComdatSelectionKind
    }
}
class Comdat(val ref: LLVMComdatRef) {
    @OptIn(ExperimentalForeignApi::class)
    public fun getSectionKind() = ComdatSelectionKind.fromLLVM(LLVMGetComdatSelectionKind(ref))
    public fun setSectionKind(kind: ComdatSelectionKind) = LLVMSetComdatSelectionKind(ref, kind.toLLVM())
}