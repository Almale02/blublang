package utils

import com.appmattus.crypto.Algorithm
import com.appmattus.crypto.Digest

interface LongHashCode {
    fun longHashCode(hasher: Digest<*>)
    fun hash(): Long {
        val hasher = Algorithm.XXHash64().createDigest()
        longHashCode(hasher)

        return hasher.digest().fold(0L) { acc, byte -> acc shl 8 or (byte.toLong() and 0xFF) }
    }
}

class LongHashMap<K : LongHashCode, V> : MutableMap<K, V> {
    private val internalMap = HashMap<Long, MutableList<Pair<K, V>>>()

    override val size: Int
        get() = internalMap.values.sumOf { it.size }

    override val entries: MutableSet<MutableMap.MutableEntry<K, V>>
        get() = object : MutableSet<MutableMap.MutableEntry<K, V>> {
            override val size: Int
                get() = this@LongHashMap.size

            override fun contains(element: MutableMap.MutableEntry<K, V>): Boolean {
                return internalMap.values.any { bucket -> bucket.any { (k, v) -> k == element.key && v == element.value } }
            }

            override fun containsAll(elements: Collection<MutableMap.MutableEntry<K, V>>): Boolean {
                return elements.all { contains(it) }
            }

            override fun isEmpty(): Boolean = this@LongHashMap.isEmpty()

            override fun iterator(): MutableIterator<MutableMap.MutableEntry<K, V>> {
                val allEntries = internalMap.values.flatten().map { (k, v) ->
                    object : MutableMap.MutableEntry<K, V> {
                        override val key: K = k
                        override val value: V = v
                        override fun setValue(newValue: V): V {
                            val hash = key.hash()
                            val bucket = internalMap[hash] ?: return v
                            val index = bucket.indexOfFirst { it.first == key }
                            if (index != -1) {
                                val old = bucket[index].second
                                bucket[index] = Pair(key, newValue)
                                return old
                            }
                            throw NoSuchElementException("Key not found")
                        }
                    }
                }

                return object : MutableIterator<MutableMap.MutableEntry<K, V>> {
                    private val iterator = allEntries.iterator()
                    private var lastReturned: MutableMap.MutableEntry<K, V>? = null

                    override fun hasNext(): Boolean = iterator.hasNext()

                    override fun next(): MutableMap.MutableEntry<K, V> {
                        val next = iterator.next()
                        lastReturned = next
                        return next
                    }

                    override fun remove() {
                        if (lastReturned == null) {
                            throw IllegalStateException()
                        }
                        this@LongHashMap.remove(lastReturned!!.key)
                        lastReturned = null
                    }
                }
            }

            override fun add(element: MutableMap.MutableEntry<K, V>): Boolean {
                return this@LongHashMap.put(element.key, element.value) == null
            }

            override fun addAll(elements: Collection<MutableMap.MutableEntry<K, V>>): Boolean {
                var modified = false
                for (entry in elements) {
                    if (add(entry)) {
                        modified = true
                    }
                }
                return modified
            }

            override fun clear() = this@LongHashMap.clear()

            override fun remove(element: MutableMap.MutableEntry<K, V>): Boolean {
                return this@LongHashMap.remove(element.key) != null
            }

            override fun removeAll(elements: Collection<MutableMap.MutableEntry<K, V>>): Boolean {
                var modified = false
                for (entry in elements) {
                    if (remove(entry)) {
                        modified = true
                    }
                }
                return modified
            }

            override fun retainAll(elements: Collection<MutableMap.MutableEntry<K, V>>): Boolean {
                val retainSet = elements.map { it.key }.toSet()
                val iterator = this.iterator()
                var modified = false
                while (iterator.hasNext()) {
                    if (!retainSet.contains(iterator.next().key)) {
                        iterator.remove()
                        modified = true
                    }
                }
                return modified
            }
        }

    override val keys: MutableSet<K>
        get() = internalMap.values.flatten().map { it.first }.toMutableSet()

    override val values: MutableCollection<V>
        get() = internalMap.values.flatten().map { it.second }.toMutableSet()

    override fun containsKey(key: K): Boolean {
        return internalMap[key.hash()]?.any { it.first == key } ?: false
    }

    override fun clear() {
        internalMap.clear()
    }

    override fun isEmpty(): Boolean {
        return internalMap.isEmpty()
    }

    override fun remove(key: K): V? {
        val hash = key.hash()
        val bucket = internalMap[hash] ?: return null
        val iterator = bucket.iterator()
        while (iterator.hasNext()) {
            val (k, v) = iterator.next()
            if (k == key) {
                iterator.remove()
                if (bucket.isEmpty()) internalMap.remove(hash)
                return v
            }
        }
        return null
    }

    override fun putAll(from: Map<out K, V>) {
        from.forEach { (k, v) -> put(k, v) }
    }

    override fun put(key: K, value: V): V? {
        val hash = key.hash()
        val bucket = internalMap.getOrPut(hash) { mutableListOf() }
        val existing = bucket.find { it.first == key }
        if (existing != null) {
            val index = bucket.indexOf(existing)
            return bucket.set(index, key to value).second
        } else {
            bucket.add(key to value)
            return null
        }
    }

    override fun get(key: K): V? {
        return findInBucket(key.hash(), key)?.second
    }

    override fun containsValue(value: V): Boolean {
        return internalMap.values.any { bucket -> bucket.any { (_, v) -> v == value } }
    }
    private fun findInBucket(hash: Long, key: K): Pair<K, V>? {
        val bucket = internalMap[hash] ?: return null
        return bucket.find { it.first == key }
    }
}
