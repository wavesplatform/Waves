package scorex.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.BlockchainFeatureStatus
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FeaturesSettings
import com.wavesplatform.state2.{BlockDiff, HistoryTest, NgState}
import org.scalatest.{FunSuite, Matchers}
import scorex.settings.TestFunctionalitySettings


class NgHistoryReaderTest extends FunSuite with Matchers with HistoryTest {
  private val ApprovalPeriod = 10000

  private val FeaturesSettingsWithoutSupportedFeatures: FeaturesSettings =
    FeaturesSettings(autoShutdownOnUnsupportedFeature = false, List.empty)

  test("ng reader feature activation and rollback") {
    var ngState = Option.empty[NgState]

    val history = HistoryWriterImpl(None, new ReentrantReadWriteLock(), TestFunctionalitySettings.Enabled,
      FeaturesSettingsWithoutSupportedFeatures).get

    val ngHistoryReader = new NgHistoryReader(() => ngState, history)

    appendBlock(history, genesisBlock)

    ngHistoryReader.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined

    (1 until ApprovalPeriod - 1).foreach { _ =>
      appendTestBlock3(history, Set(1))
    }

    history.height() shouldBe ApprovalPeriod - 1
    ngHistoryReader.height() shouldBe history.height()
    ngHistoryReader.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined

    ngState = Some(NgState(getNextTestBlock(history), BlockDiff.empty, 0L))

    ngHistoryReader.height() shouldBe ApprovalPeriod
    history.height() shouldBe ApprovalPeriod - 1
    ngHistoryReader.featureStatus(1) shouldBe BlockchainFeatureStatus.Accepted

    ngState = None

    ngHistoryReader.height() shouldBe ApprovalPeriod - 1
    ngHistoryReader.height() shouldBe history.height()
    ngHistoryReader.featureStatus(1) shouldBe BlockchainFeatureStatus.Undefined
  }
}

