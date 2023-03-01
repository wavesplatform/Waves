package com.wavesplatform.riderunner.storage

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.{InMemWithoutHeightPersistentCache, PersistentCache}
import com.wavesplatform.state.Height
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}

import java.util.concurrent.atomic.AtomicInteger

class ExactWithHeightStorageTestSuite extends BaseTestSuite with HasDb with HasTestAccounts {
  private implicit val ctx = new ReadWrite(null)

  "ExactWithHeightStorage" - {
    "loading from a blockchain" - {
      "if it hasn't the key, stores it as Absent" in {
        val persistent = new InMemWithoutHeightPersistentCache[String, Int]
        val storage    = new BaseStorage(persistent)

        storage.getOpt("1") shouldBe None
        persistent.get("1") shouldBe RemoteData.Absence
      }

      "if it has the key, stores it as Cached" in {
        val persistent = new InMemWithoutHeightPersistentCache[String, Int]
        val storage = new BaseStorage(persistent) {
          override def getFromBlockchain(key: String): Option[Int] =
            if (key == "1") Some(1) else super.getFromBlockchain(key)
        }

        storage.get("1") shouldBe 1
        persistent.get("1") shouldBe RemoteData.Cached(1)
      }
    }

    "reloads from the disk cache if the in-memory cache overfilled" in {
      val calls      = new AtomicInteger(0)
      val persistent = new InMemWithoutHeightPersistentCache[String, Int]
      val storage = new BaseStorage(persistent) {
        override def getFromBlockchain(key: String): Option[Int] = {
//          if (key == "1") calls.incrementAndGet() // passes if I uncomment
          key.toIntOption
        }
      }

      def set(key: String, value: Int): Unit = persistent.set(0, key, RemoteData.Cached(value))

      storage.get("1") shouldBe 1
      storage.get("2") shouldBe 2

      set("1", 11)
      withClue("displace old entries:") {
        // Yes, we have to do it at least three times (twice with cleanup)
        Iterator.continually(3 to 4).take(3).flatten.foreach { x =>
          storage.get(x.toString) shouldBe x
        }
      }

      withClue("1 reloaded:") { storage.get("1") shouldBe 11 }
      calls.get() shouldBe 1
    }
  }

  private class BaseStorage(override val persistentCache: PersistentCache[String, Int])
      extends ExactWithHeightStorage[String, Int, Int] {
    override lazy val settings: ExactWithHeightStorage.Settings = ExactWithHeightStorage.Settings(2)
    override def getFromBlockchain(key: String): Option[Int]    = None
    def get(key: String): Int                                   = getOpt(key).value
    def getOpt(key: String): Option[Int]                        = get(Height(0), key, Int.MaxValue)
  }
}
