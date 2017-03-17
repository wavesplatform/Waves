package com.wavesplatform.state2

import java.util

import org.h2.mvstore.{MVMap, MVStore}
import org.scalatest.{FreeSpec, Matchers}

class FreeSpecPG extends FreeSpec with Matchers {

  val s = "C:\\Users\\ilyas\\Desktop\\file.data"

  "sdf" - {
    "init" in {
      val mVStore = new MVStore.Builder().fileName(s).open()
      val map: MVMap[Int, util.LinkedList[Array[Byte]]] = mVStore.openMap("some0")
      val mapint: MVMap[Int, String] = mVStore.openMap("some1")
      val maparr: MVMap[Int, Array[String]] = mVStore.openMap("some2")

      val mapcompositeKey: MVMap[(Array[Byte], Int), (Long, Long)] = mVStore.openMap("some3")

      mapcompositeKey.put((Array(0: Byte, 1: Byte), 2), (3, 4))
      println(mapcompositeKey.get((Array(0: Byte, 1: Byte), 2)))

      println(map.get(7))
      Option(map.get(7)) match {
        case Some(m) =>
          m.forEach(println)
          println(m.size())
          m.add(Array(1: Byte))
          map.put(7, m)

        case None =>
          val ints = new util.LinkedList[Array[Byte]]()
          ints.add(Array(0: Byte))
          map.put(7, ints)

      }


      println(mapint.get(8))

      Option(mapint.get(8)) match {
        case Some(m) =>
          println(m)
          mapint.put(8, m + "sdf")

        case None =>
          mapint.put(8, "init")

      }


      println(maparr.get(9))

      Option(maparr.get(9)) match {
        case Some(m) =>
          m.foreach(println)
          maparr.put(9, m :+ "sdf")

        case None =>
          maparr.put(9, Array("init"))

      }

      mVStore.commit()
      mVStore.close()
    }
  }
}
