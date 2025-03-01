@file:OptIn(ExperimentalForeignApi::class)

package llvm
import arrow.core.Either
import arrow.core.left
import arrow.core.right
import kotlinx.cinterop.*
import llvm.types.AddressSpace
import llvm.types.StructType
import llvm.types.Type
import llvm.values.GlobalValue
import llvmc.*
import okio.Path

class TargetData(val ref: LLVMTargetDataRef) {
    @OptIn(ExperimentalForeignApi::class)
    public fun getDataLayout() = LLVMString(LLVMCopyStringRepOfTargetData(ref)!!)
    public fun getBitSize(ty: Type) = LLVMSizeOfTypeInBits(ref, ty.ty)
    @OptIn(ExperimentalForeignApi::class)
    public fun create(string: String) = TargetData(LLVMCreateTargetData(string)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getByteOrdering() = when (LLVMByteOrder(ref)) {
        LLVMByteOrdering.LLVMBigEndian -> ByteOrdering.BigEndian
        LLVMByteOrdering.LLVMLittleEndian -> ByteOrdering.LittleEndian
    }
    public fun getPtrByteSize(addressSpace: AddressSpace) = LLVMPointerSizeForAS(ref, addressSpace.addressSpace)
    @OptIn(ExperimentalForeignApi::class)
    public fun getPtrByteSize() = LLVMPointerSize(ref)

    @OptIn(ExperimentalForeignApi::class)
    public fun getStoreSize(type: Type) =
        LLVMStoreSizeOfType(ref, type.ty)

    @OptIn(ExperimentalForeignApi::class)
    public fun getAbiSize(type: Type) = LLVMABISizeOfType(ref, type.ty)

    public fun getAbiAlignment(type: Type) =
        LLVMABIAlignmentOfType(ref, type.ty)

    @OptIn(ExperimentalForeignApi::class)
    public fun getCallFrameAlignment(type: Type) =
        LLVMCallFrameAlignmentOfType(ref, type.ty)

    @OptIn(ExperimentalForeignApi::class)
    public fun getPreferredAlignment(type: Type) =
        LLVMPreferredAlignmentOfType(ref, type.ty)
    public fun getPreferredAlignmentOfGlobal(value: GlobalValue) = LLVMPreferredAlignmentOfGlobal(ref, value.valueRef)
    public fun elementAtOffset(structType: StructType, offset: ULong) = LLVMElementAtOffset(ref, structType.ty, offset)
    @OptIn(ExperimentalForeignApi::class)
    public fun offsetOfElement(structType: StructType, element: UInt) = if (element > structType.countFields() -1u) {null} else {
        LLVMOffsetOfElement(ref, structType.ty, element)
    }
}
public class Target(val ref: LLVMTargetRef) {
    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun initAll(config: InitConfig) {
            with(config) {
                if (base)
                    LLVMInitializeAllTargets()
                if (info)
                    LLVMInitializeAllTargetInfos()
                if (asmParser)
                    LLVMInitializeAllAsmParsers()
                if (asmPrinter)
                    LLVMInitializeAllAsmPrinters()
                if (disassembler)
                    LLVMInitializeAllDisassemblers()
                if (machineCode)
                    LLVMInitializeAllTargetMCs()
            }
        }
        @OptIn(ExperimentalForeignApi::class)
        public fun getFirst() = LLVMGetFirstTarget()?.let { Target(it) }
        public fun fromName(name: String) = LLVMGetTargetFromName(name)?.let { Target(it) }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun createTargetMachine(targetTriple: String, cpu: String, features: String, optLevel: OptLevel, relocMode: RelocMode, codeModel: CodeModel) =
        TargetMachine(LLVMCreateTargetMachine(ref, targetTriple, cpu, features, optLevel.toLLVM(), relocMode.toLLVM(), codeModel.toLLVM())!!)
}
data class InitConfig(
    val asmParser: Boolean,
    val asmPrinter: Boolean,
    val base: Boolean,
    val disassembler: Boolean,
    val info: Boolean,
    val machineCode: Boolean,
) {
    companion object {
        public fun default() = InitConfig(
            asmParser = true,
            asmPrinter = true,
            base = true,
            disassembler = true,
            info = true,
            machineCode = true
        )
    }
}
enum class OptLevel {
    None,
    Less,
    Default,
    Aggressive;

    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun fromLLVM(level: LLVMCodeGenOptLevel) = when (level) {
            LLVMCodeGenOptLevel.LLVMCodeGenLevelNone -> None
            LLVMCodeGenOptLevel.LLVMCodeGenLevelLess -> Less
            LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault -> Default
            LLVMCodeGenOptLevel.LLVMCodeGenLevelAggressive -> Aggressive
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun toLLVM() = when (this) {
        None -> LLVMCodeGenOptLevel.LLVMCodeGenLevelNone
        Less -> LLVMCodeGenOptLevel.LLVMCodeGenLevelLess
        Default -> LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault
        Aggressive -> LLVMCodeGenOptLevel.LLVMCodeGenLevelAggressive
    }
}
enum class CodeModel {
    Default,
    JITDefault,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large;

    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun fromLLVM(llvm: LLVMCodeModel) = when (llvm) {
            LLVMCodeModel.LLVMCodeModelDefault -> Default
            LLVMCodeModel.LLVMCodeModelJITDefault -> JITDefault
            LLVMCodeModel.LLVMCodeModelTiny -> Tiny
            LLVMCodeModel.LLVMCodeModelSmall -> Small
            LLVMCodeModel.LLVMCodeModelKernel -> Kernel
            LLVMCodeModel.LLVMCodeModelMedium -> Medium
            LLVMCodeModel.LLVMCodeModelLarge -> Large
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun toLLVM() = when (this) {
        Default -> LLVMCodeModel.LLVMCodeModelDefault
        JITDefault -> LLVMCodeModel.LLVMCodeModelJITDefault
        Tiny -> LLVMCodeModel.LLVMCodeModelTiny
        Small -> LLVMCodeModel.LLVMCodeModelSmall
        Kernel -> LLVMCodeModel.LLVMCodeModelKernel
        Medium -> LLVMCodeModel.LLVMCodeModelMedium
        Large -> LLVMCodeModel.LLVMCodeModelLarge
    }
}
enum class RelocMode {
    Default,
    Static,
    PIC,
    RWPI,
    ROPI,
    ROPI_RWPI,
    DynamicNoPic;

    companion object {
        public fun fromLLVM(mode: LLVMRelocMode) = when (mode) {
            LLVMRelocMode.LLVMRelocDefault -> Default
            LLVMRelocMode.LLVMRelocStatic -> Static
            LLVMRelocMode.LLVMRelocPIC -> PIC
            LLVMRelocMode.LLVMRelocDynamicNoPic -> DynamicNoPic
            LLVMRelocMode.LLVMRelocROPI -> ROPI
            LLVMRelocMode.LLVMRelocRWPI -> RWPI
            LLVMRelocMode.LLVMRelocROPI_RWPI -> ROPI_RWPI
        }
    }
    public fun toLLVM() = when (this) {
        Default -> LLVMRelocMode.LLVMRelocDefault
        Static -> LLVMRelocMode.LLVMRelocStatic
        PIC -> LLVMRelocMode.LLVMRelocPIC
        RWPI -> LLVMRelocMode.LLVMRelocRWPI
        ROPI -> LLVMRelocMode.LLVMRelocROPI
        ROPI_RWPI -> LLVMRelocMode.LLVMRelocROPI_RWPI
        DynamicNoPic -> LLVMRelocMode.LLVMRelocDynamicNoPic
    }
}
enum class ByteOrdering {
    BigEndian,
    LittleEndian
}
enum class FileType {
    Asm,
    Obj;

    @OptIn(ExperimentalForeignApi::class)
    public fun toLLVM() = when (this) {
        Asm -> LLVMCodeGenFileType.LLVMAssemblyFile
        Obj -> LLVMCodeGenFileType.LLVMObjectFile
    }
}
public class TargetMachine(val ref: LLVMTargetMachineRef) {
    public fun getTarget() = Target(LLVMGetTargetMachineTarget(ref)!!)
    public fun getTriple() = LLVMString(LLVMGetTargetMachineTriple(ref)!!)
    public fun getDefaultTriple() = LLVMString(LLVMGetDefaultTargetTriple()!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun normalizeTriple(targetTriple: LLVMString) = LLVMString(LLVMNormalizeTargetTriple(targetTriple.toString())!!)
    public fun getHostCpuName() = LLVMString(LLVMGetHostCPUName()!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getHostCpuFeatures() = LLVMString(LLVMGetHostCPUFeatures()!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getCpu() = LLVMString(LLVMGetTargetMachineCPU(ref)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getFeatureString() = LLVMString(LLVMGetTargetMachineFeatureString(ref)!!)
    @OptIn(ExperimentalForeignApi::class)
    public fun getTargetData() = TargetData(LLVMCreateTargetDataLayout(ref)!!)
    @OptIn(ExperimentalForeignApi::class)
    public infix fun setAsmVerbosity(verbosity: Boolean) {
        LLVMSetTargetMachineAsmVerbosity(ref, verbosity.toByte().toInt())
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun writeToMemoryBuffer(module: Module, fileType: FileType): Either<MemoryBuffer, LLVMString>  {
        memScoped {
            val buff = alloc<LLVMMemoryBufferRefVar>()
            val outMsg = alloc<CPointerVar<ByteVar>>()
            if (LLVMTargetMachineEmitToMemoryBuffer(ref, module.moduleRef, fileType.toLLVM(), outMsg.ptr, buff.ptr) == 1)
                return LLVMString(outMsg.value!!).right()
            return MemoryBuffer(buff.value!!).left()
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun writeToFile(module: Module, fileType: FileType, path: Path): Either<Unit, LLVMString> {
        memScoped {
            val outMsg = alloc<CPointerVar<ByteVar>>()
            if (LLVMTargetMachineEmitToFile(ref, module.moduleRef, path.toString(), fileType.toLLVM(), outMsg.ptr) == 1)
                return LLVMString(outMsg.value!!).right()
            return Unit.left()
        }
    }
}