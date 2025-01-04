package compile.types

import com.appmattus.crypto.Algorithm
import com.appmattus.crypto.Digest
import compile.parser.AstType
import compilerError
import compilerICE
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toByte
import utils.LongHashCode
import utils.LongHashMap
import utils.toByteArray
import kotlin.experimental.ExperimentalNativeApi

sealed class TypeInfo(val id: Int): LongHashCode {
    data class Struct(val name: String, val fields: kotlin.Array<StructField>) : TypeInfo(0)
    data class Fn(val returnTypeHandle: TypeHandle, val args: kotlin.Array<TypeHandle>) : TypeInfo(1)
    data class Number(val number: NumberTypes) : TypeInfo(2)
    data class Array(val handle: TypeHandle) : TypeInfo(3)
    data class Pointer(val isMut: Boolean, val pointee: TypeHandle) : TypeInfo(4)
    object Str : TypeInfo(5)
    object Unit : TypeInfo(6)
    object Bool : TypeInfo(7)

    @OptIn(ExperimentalForeignApi::class, ExperimentalNativeApi::class)
    override fun longHashCode(hasher: Digest<*>) {
        hasher.update(id.toByteArray())
        when (this) {
            is Array -> {
                hasher.update(this.handle.toByteArray())
            }

            is Struct -> {
                // it is enough to hash the name because the names are unique across structs
                hasher.update(name.encodeToByteArray())
            }

            is Fn -> {
                hasher.update(returnTypeHandle.toByteArray())
                args.forEach { hasher.update(it.toByteArray()) }
            }

            is Number -> {
                hasher.update(number.id.toByteArray())
            }

            is Pointer -> {
                hasher.update(isMut.toByte())
                hasher.update(pointee.toByteArray())
            }

            is Bool -> {}
            is Str -> {}
            is Unit -> {}
        }
    }

    fun display(): String {
        return when (this) {
            is Struct -> this.name
            is Fn -> {
                var string = ""
                string += "fn ("
                this.args.forEach {
                    string += TypeRegistry.typeHandleToInfo[it]!!.display()
                    string += ", "
                }
                string += "): "
                string += TypeRegistry.typeHandleToInfo[this.returnTypeHandle]!!.display()
                string
            }
            is Array -> {
                var string = ""
                string += "["
                string += TypeRegistry.typeHandleToInfo[this.handle]!!.display()
                string += "]"

                string
            }
            is Pointer -> {
                var string = ""
                string += "*"
                if (this.isMut) {
                    string += "mut"
                    string += " "
                }
                string += TypeRegistry.typeHandleToInfo[this.pointee]!!.display()

                string
            }
            is Number -> {
                this.number.display()
            }
            Unit -> "Unit"
            Bool -> "bool"
            Str -> "str"
        }
    }
}
data class StructField(val name: String, val typeHandle: TypeHandle)
data class TypeHandle(val handle: ULong): LongHashCode {
    fun toByteArray(): ByteArray {
        return handle.toByteArray()
    }

    override fun longHashCode(hasher: Digest<*>) {
        hasher.update(toByteArray())
    }

    fun display(): String {
        return TypeRegistry.typeHandleToInfo[this]!!.display()
    }

}
enum class NumberTypes(val id: Int) {
    I8(1),
    I16(2),
    I32(3),
    I64(4),
    I128(5),
    U8(6),
    U16(7),
    U32(8),
    U64(9),
    U128(10),
    Usize(11),
    F32(12),
    F64(13);

    fun display(): String {
        return when (this) {
            I8 -> "i8"
            I16 -> "i16"
            I32 -> "i32"
            I64 -> "i64"
            I128 -> "i128"
            U8 -> "u8"
            U16 -> "u16"
            U32 -> "u32"
            U64 -> "u64"
            U128 -> "u128"
            Usize -> "usize"
            F32 -> "f32"
            F64 -> "f64"
        }
    }

}
object TypeRegistry {
    var currentTypeId: ULong = 1u
    val typeHandleToInfo = LongHashMap<TypeHandle, TypeInfo>()
    val typeInfoToHandle = LongHashMap<TypeInfo, TypeHandle>()
    val structNameToHandle = HashMap<String, TypeHandle>()
    val functionNameToHandle = HashMap<String, TypeHandle>()

    fun containsType(info: TypeInfo): Boolean {
        return when (info) {
            is TypeInfo.Struct -> structNameToHandle.containsKey(info.name)
            else -> typeInfoToHandle.containsKey(info)
        }
    }
    infix fun getTypeInfo(handle: TypeHandle): TypeInfo {
        return typeHandleToInfo[handle]!!
    }
    infix fun getTypeHandle(info: TypeInfo): TypeHandle {
        return typeInfoToHandle[info]!!
    }
    infix fun structNameToHandle(name: String): TypeHandle {
        return structNameToHandle[name]!!
    }
    infix fun funcNameToHandle(name: String): TypeHandle {
        return functionNameToHandle[name]!!
    }
    fun addType(info: TypeInfo): TypeHandle {
        if (containsType(info))
            compilerICE("tried to add type which already exist: $info")
        val typeId = currentTypeId
        val typeHandle = TypeHandle(typeId)
        currentTypeId += 1u
        typeHandleToInfo[typeHandle] = info
        typeInfoToHandle[info] = typeHandle

        if (info is TypeInfo.Struct) {
            structNameToHandle[info.name] = typeHandle
        }
        return typeHandle
    }
    fun defineType(handle: TypeHandle, info: TypeInfo) {
        typeHandleToInfo[handle] = info
        typeInfoToHandle[info] = handle
    }
    fun addPrimitives() {
        addType(TypeInfo.Unit)
        addType(TypeInfo.Str)
        addType(TypeInfo.Bool)
        addType(TypeInfo.Number(NumberTypes.I8))
        addType(TypeInfo.Number(NumberTypes.I16))
        addType(TypeInfo.Number(NumberTypes.I32))
        addType(TypeInfo.Number(NumberTypes.I64))
        addType(TypeInfo.Number(NumberTypes.I128))
        addType(TypeInfo.Number(NumberTypes.U8))
        addType(TypeInfo.Number(NumberTypes.U16))
        addType(TypeInfo.Number(NumberTypes.U32))
        addType(TypeInfo.Number(NumberTypes.U64))
        addType(TypeInfo.Number(NumberTypes.U128))
        addType(TypeInfo.Number(NumberTypes.Usize))
        addType(TypeInfo.Number(NumberTypes.F32))
        addType(TypeInfo.Number(NumberTypes.F64))
    }
    fun isTypeDefined(handle: TypeHandle): Boolean {
        return typeHandleToInfo.containsKey(handle)
    }
    fun resolveTypeHandleFromAstType(astType: AstType): TypeHandleResolveRes {
        return when(astType) {
            is AstType.Symbol -> {
                return when (astType.symbol) {
                    "i8" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.I8)]!!)
                    "i16" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.I16)]!!)
                    "i32" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.I32)]!!)
                    "i64" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.I64)]!!)
                    "i128" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.I128)]!!)
                    "u8" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.U8)]!!)
                    "u16" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.U16)]!!)
                    "u32" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.U32)]!!)
                    "u64" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.U64)]!!)
                    "u128" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.U128)]!!)
                    "usize" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.Usize)]!!)
                    "f32" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.F32)]!!)
                    "f64" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Number(NumberTypes.F64)]!!)
                    "str" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Str]!!)
                    "bool" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Bool]!!)
                    "Unit" -> TypeHandleResolveRes.newDefinedType(typeInfoToHandle[TypeInfo.Unit]!!)
                    else -> {
                        return structNameToHandle[astType.symbol]?.let {
                            TypeHandleResolveRes.newDefinedType(it)
                        } ?: TypeHandleResolveRes.newUndefinedStruct(this, astType.symbol)
                    }
                }
            }
            is AstType.Pointer -> {
                val pointeeResolve = resolveTypeHandleFromAstType(astType.pointeeType)
                val otherUndefinedTypes = arrayListOf<Pair<TypeHandle, String>>()
                val pointeeTypeInfo = TypeInfo.Pointer(astType.isMut, pointeeResolve.handle)

                pointeeResolve.typeIsUndefined?.let {
                    otherUndefinedTypes.add(Pair(pointeeResolve.handle, it))
                }
                pointeeResolve.otherUndefinedTypes.forEach {
                    otherUndefinedTypes.add(it)
                }
                val resolveHandle = if (!containsType(pointeeTypeInfo)) {
                    addType(pointeeTypeInfo)
                } else {
                    typeInfoToHandle[pointeeTypeInfo]!!
                }
                TypeHandleResolveRes(resolveHandle, null, otherUndefinedTypes.toTypedArray())
            }
            is AstType.Array -> {
                val arrayDataResolve = resolveTypeHandleFromAstType(astType.underlying)
                val otherUndefinedTypes = arrayListOf<Pair<TypeHandle, String>>()

                arrayDataResolve.typeIsUndefined?.let {
                    otherUndefinedTypes.add(Pair(arrayDataResolve.handle, it))
                }
                arrayDataResolve.otherUndefinedTypes.forEach {
                    otherUndefinedTypes.add(it)
                }
                val resolveHandle = if (!containsType(TypeInfo.Array(arrayDataResolve.handle))) {
                    addType(TypeInfo.Array(arrayDataResolve.handle))
                } else {
                    typeInfoToHandle[TypeInfo.Array(arrayDataResolve.handle)]!!
                }
                TypeHandleResolveRes(resolveHandle, null, otherUndefinedTypes.toTypedArray())
            }
        }
    }

}
data class TypeHandleResolveRes(val handle: TypeHandle, val typeIsUndefined: String?, val otherUndefinedTypes: Array<Pair<TypeHandle, String>>) {
    companion object {
        fun newUndefinedStruct(typeReg: TypeRegistry, name: String): TypeHandleResolveRes {
            val newId = typeReg.currentTypeId
            typeReg.currentTypeId += 1u
            typeReg.structNameToHandle[name] = TypeHandle(newId)

            return TypeHandleResolveRes(TypeHandle(newId), name, arrayOf())
        }
        fun newDefinedType(handle: TypeHandle): TypeHandleResolveRes {
            return TypeHandleResolveRes(handle, null, arrayOf())
        }
    }
}
