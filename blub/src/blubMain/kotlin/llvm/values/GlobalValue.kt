package llvm.values

import compilerICE
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import llvm.Comdat
import llvm.Linkage
import llvm.types.Type
import llvmc.*

@OptIn(ExperimentalForeignApi::class)
class GlobalValue(val value: Value): Value(value.valueRef) {
    public fun getPrevGlobal() = LLVMGetPreviousGlobal(valueRef)?.let { GlobalValue(Value(it)) }
    public fun getNextGlobal() = LLVMGetNextGlobal(valueRef)?.let { GlobalValue(Value(it)) }
    public fun getDllStorageClass() = DLLStorageClass.fromUInt(LLVMGetDLLStorageClass(valueRef))
    public fun setDllStorageClass(dllStorageClass: DLLStorageClass) = LLVMSetDLLStorageClass(valueRef, dllStorageClass.ordinal.toUInt())
    public fun getInitializer() = LLVMGetInitializer(valueRef)?.let { Value(it) }
    public fun setInitializer(value: Value) = LLVMSetInitializer(valueRef, value.valueRef)
    public fun isThreadLocal() = LLVMIsThreadLocal(valueRef) == 1
    public fun setThreadLocal(isThreadLocal: Boolean) = LLVMSetThreadLocal(valueRef, isThreadLocal.toByte().toInt())
    public fun getThreadLocalMode() = ThreadLocalMode.fromUInt(LLVMGetThreadLocalMode(valueRef))
    public fun setThreadLocalMode(mode: ThreadLocalMode) = LLVMSetThreadLocalMode(valueRef, mode.ordinal.toUInt())
    public fun isDecl() = LLVMIsDeclaration(valueRef) == 1
    public fun hasUnnamedAddr() = LLVMHasUnnamedAddr(valueRef) == 1
    public fun setUnnamedAddr(hasUnnamedAddr: Boolean) = LLVMSetUnnamedAddr(valueRef, hasUnnamedAddr.toByte().toInt())
    public fun isConstant() = LLVMIsConstant(valueRef) == 1
    public fun setConstant(isConstant: Boolean) = LLVMSetGlobalConstant(valueRef, isConstant.toByte().toInt())
    public fun isExternallyInited() = LLVMIsExternallyInitialized(valueRef) == 1
    public fun setExternallyInited(boolean: Boolean) = LLVMSetExternallyInitialized(valueRef, boolean.toByte().toInt())
    public fun setVisibility(visibility: GlobalVisibility) = LLVMSetVisibility(valueRef, visibility.toLLVM())
    public fun getVisibility() = GlobalVisibility.fromLLVM(LLVMGetVisibility(valueRef))
    public fun delete() = LLVMDeleteGlobal(valueRef)
    public fun getComdat() = Comdat(LLVMGetComdat(valueRef)!!)
    public fun setComdat(comdat: Comdat) = LLVMSetComdat(valueRef, comdat.ref)
    public fun getLinkage() = Linkage.fromLLVM(LLVMGetLinkage(valueRef))
    public fun setLinkage(linkage: Linkage) = LLVMSetLinkage(valueRef, linkage.toLLVM())
    public fun getValueType() = Type(LLVMGlobalGetValueType(valueRef)!!)
}
enum class DLLStorageClass {
    Default,
    Import,
    Export;

    companion object {
        public fun fromUInt(num: UInt) = DLLStorageClass.entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
    }
}
enum class ThreadLocalMode {
    GeneralDynamicTLSModel,
    LocalDynamicTLSModel,
    InitialExecTLSModel,
    LocalExecTLSModel,
    NotThreadLocal;

    companion object {
        public fun fromUInt(num: UInt) = ThreadLocalMode.entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
    }
}
enum class UnnamedAddr {
    No,
    Local,
    Global;
    companion object {
        @OptIn(ExperimentalForeignApi::class)
        public fun fromLLVM(addr: LLVMUnnamedAddr) = when (addr) {
            LLVMUnnamedAddr.LLVMNoUnnamedAddr -> No
            LLVMUnnamedAddr.LLVMLocalUnnamedAddr -> Local
            LLVMUnnamedAddr.LLVMGlobalUnnamedAddr -> Global
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun toLLVM() = when (this) {
        No -> LLVMUnnamedAddr.LLVMNoUnnamedAddr
        Local -> LLVMUnnamedAddr.LLVMLocalUnnamedAddr
        Global -> LLVMUnnamedAddr.LLVMGlobalUnnamedAddr
    }
}
enum class GlobalVisibility {
    Default,
    Hidden,
    Protected;
    companion object {
        public fun fromUInt(num: UInt) = GlobalVisibility.entries.firstOrNull {it.ordinal.toUInt() == num} ?: compilerICE("reached unreachable code")
        @OptIn(ExperimentalForeignApi::class)
        public fun fromLLVM(x: LLVMVisibility) = when (x) {
            LLVMVisibility.LLVMDefaultVisibility -> GlobalVisibility.Default
            LLVMVisibility.LLVMHiddenVisibility -> GlobalVisibility.Hidden
            LLVMVisibility.LLVMProtectedVisibility -> GlobalVisibility.Protected
        }
    }
    @OptIn(ExperimentalForeignApi::class)
    public fun toLLVM() = when (this) {
        Default -> LLVMVisibility.LLVMDefaultVisibility
        Hidden -> LLVMVisibility.LLVMHiddenVisibility
        Protected -> LLVMVisibility.LLVMProtectedVisibility
    }
}