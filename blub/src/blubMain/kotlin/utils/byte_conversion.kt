package utils

fun Int.toByteArray(): ByteArray{
    val buffer = ByteArray(4)
    for (i in 0..3) buffer[i] = (this shr (i*8)).toByte()
    return buffer
}
fun ULong.toByteArray(): ByteArray {
    return ByteArray(8) { index ->
        ((this shr ((7 - index) * 8)) and 0xFFu).toByte()}
}
fun Long.toByteArray(): ByteArray {
    return toLong().toByteArray()
}
