@file:OptIn(ExperimentalForeignApi::class)

package llvm.values

import kotlinx.cinterop.ExperimentalForeignApi

class PtrValue(val value: Value): Value(value.valueRef) {

}