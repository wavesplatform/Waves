package scorex.transaction.state.database

import com.wavesplatform.UtxPool
import com.wavesplatform.settings.{FeesSettings, FunctionalitySettings, UtxSettings}
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.PublicKeyAccount
import scorex.transaction.{FeeCalculator, GenesisTransaction}
import scorex.utils.Time

import scala.concurrent.duration._

class UtxPoolSpecification extends FreeSpec with Matchers with MockFactory {

  "utx database" - {
    "do nothing if tx db becomes full" in {

      val smallSizedUTXSettings = UtxSettings(1, 1.second)

      val db: UtxPool = new UtxPool(stub[ChannelGroup], stub[Time],
        stub[StateReader], new FeeCalculator(FeesSettings(Map.empty)), FunctionalitySettings.TESTNET, smallSizedUTXSettings)


      db.putIfNew(newTx(1)) shouldBe 'right

      db.all() should have size 1

      db.putIfNew(newTx(2)) shouldBe 'left
    }

    "does not add the same transaction twice" in {
      val db: UtxPool = new UtxPool(stub[ChannelGroup], stub[Time],
        stub[StateReader], new FeeCalculator(FeesSettings(Map.empty)), FunctionalitySettings.TESTNET, UtxSettings(10, 1.minute))

      db.putIfNew(newTx(1)) shouldBe 'right
      db.putIfNew(newTx(1)) shouldBe 'left
    }

    "validator returns false" in {
      val db: UtxPool = new UtxPool(stub[ChannelGroup], stub[Time],
        stub[StateReader], new FeeCalculator(FeesSettings(Map.empty)), FunctionalitySettings.TESTNET, UtxSettings(10, 1.minute))

      db.all() shouldBe empty

      db.putIfNew(newTx(1)) shouldBe 'left
      db.all() shouldBe empty
    }
  }

  private def newTx(id: Long) = GenesisTransaction.create(PublicKeyAccount(Array.fill(32)(0: Byte)), id, 4598723454L).right.get

}
