package com.wavesplatform.db

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.WithDB
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable
import scala.language.reflectiveCalls

class SubStorageSpec extends FreeSpec with Matchers with PropertyChecks with WithDB {
  "Adding value to SubStorage should work" in {
    val storage = new SubStorage(db, "test-a")
    val key = "key-1".getBytes(StandardCharsets.UTF_8)
    val v1 = "value 1".getBytes(StandardCharsets.UTF_8)
    storage.put(key, v1)
    val v2 = storage.get(key).getOrElse(Array.emptyByteArray)
    v2 should contain theSameElementsAs v1
  }

  "Adding and removing of value should work" in {
    val storage = new SubStorage(db, "test-b")
    val key = "key".getBytes(StandardCharsets.UTF_8)
    val v1 = "value".getBytes(StandardCharsets.UTF_8)
    storage.put(key, v1)
    val v2 = storage.get(key)
    v2.isDefined should be(true)
    storage.delete(key)
    val v3 = storage.get(key)
    v3.isDefined should be(false)
  }

  "Iterating over all keys should work" in {
    val storage = new SubStorage(db, "test") {
      def key(str: String, i: Int): Array[Byte] = makeKey(str.getBytes(Charset), i)

      def keys(): Set[Array[Byte]] = {
        val result = mutable.HashSet.empty[Array[Byte]]
        val it = allKeys
        while (it.hasNext) {
          result.add(it.next())
        }
        result.toSet
      }
    }

    storage.put(storage.key("aaa", 0), Ints.toByteArray(1))
    storage.put(storage.key("aaa", 1), Ints.toByteArray(2))
    storage.put(storage.key("bbb", 0), Ints.toByteArray(1))
    storage.put(storage.key("bbb", 1), Ints.toByteArray(2))

    val keys = storage.keys()
    keys should contain(key("test:aaa", 0))
    keys should contain(key("test:aaa", 1))
    keys should contain(key("test:bbb", 0))
    keys should contain(key("test:bbb", 1))
  }

  "Iterating over some keys should work" in {
    val storage = new SubStorage(db, "test") {
      def key(str: String, i: Int): Array[Byte] = makeKey(str.getBytes(Charset), i)

      def keys(p: Array[Byte]): Set[Array[Byte]] = {
        val result = mutable.HashSet.empty[Array[Byte]]
        val it = allKeys
        while (it.hasNext) {
          val k = it.next()
          if (k.startsWith(p)) result.add(k)
        }
        result.toSet
      }
    }

    storage.put(storage.key("aaa", 0), Ints.toByteArray(1))
    storage.put(storage.key("aaa", 1), Ints.toByteArray(2))
    storage.put(storage.key("bbb", 0), Ints.toByteArray(1))
    storage.put(storage.key("bbb", 1), Ints.toByteArray(2))

    val keys = storage.keys("test:bbb".getBytes(StandardCharsets.UTF_8))
    keys should not contain key("test:aaa", 0)
    keys should not contain key("test:aaa", 1)
    keys should contain(key("test:bbb", 0))
    keys should contain(key("test:bbb", 1))
  }

  "Removing everything should work" in {

    val storage = new SubStorage(db, "test")
    storage.put(key("test:test", 0), Ints.toByteArray(1))
    storage.put(key("test:test", 1), Ints.toByteArray(2))
    storage.put(key("test:trash", 0), Ints.toByteArray(1))
    storage.put(key("test:trash", 1), Ints.toByteArray(2))

    storage.get(key("test:test", 0)).isDefined should be(true)
    storage.get(key("test:test", 1)).isDefined should be(true)
    storage.get(key("test:trash", 0)).isDefined should be(true)
    storage.get(key("test:trash", 1)).isDefined should be(true)

    storage.removeEverything(None)

    storage.get(key("test:test", 0)).isDefined should be(false)
    storage.get(key("test:test", 1)).isDefined should be(false)
    storage.get(key("test:trash", 0)).isDefined should be(false)
    storage.get(key("test:trash", 1)).isDefined should be(false)
  }

  "Removing some keys should work" in {
    val storage = new SubStorage(db, "test") {
      def key(str: String, i: Int): Array[Byte] = makeKey(str.getBytes(Charset), i)

      def keys(prefix: Array[Byte]): Set[Array[Byte]] = {
        val result = mutable.HashSet.empty[Array[Byte]]
        val it = allKeys
        while (it.hasNext) {
          val k = it.next()
          if (k.startsWith(prefix)) result.add(k)
        }
        result.toSet
      }
    }

    storage.put(storage.key("good", 0), Ints.toByteArray(1))
    storage.put(storage.key("good", 1), Ints.toByteArray(2))
    storage.put(storage.key("trash", 0), Ints.toByteArray(1))
    storage.put(storage.key("trash", 1), Ints.toByteArray(2))

    val b = storage.createBatch()
    storage.keys("test:trash".getBytes(StandardCharsets.UTF_8)).foreach(k => storage.delete(k, b))
    storage.commit(b)

    storage.get(key("test:good", 0)).isDefined should be(true)
    storage.get(key("test:good", 1)).isDefined should be(true)
    storage.get(key("test:trash", 0)).isDefined should be(false)
    storage.get(key("test:trash", 1)).isDefined should be(false)
  }

  def key(str: String, i: Int): Array[Byte] =
    Bytes.concat(str.getBytes(StandardCharsets.UTF_8), ":".getBytes(StandardCharsets.UTF_8), Ints.toByteArray(i))

}
