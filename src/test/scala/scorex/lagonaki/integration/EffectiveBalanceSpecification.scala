package scorex.lagonaki.integration

import com.wavesplatform.settings.Constants
import org.scalatest.{FunSuite, Matchers}
import scorex.account.Account

class EffectiveBalanceSpecification extends FunSuite with Matchers with scorex.waves.TestingCommons  {
  private def accounts = application.wallet.privateKeyAccounts()

  val richBalance = accounts.find(_.address == "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K").get
  val first = application.wallet.generateNewAccount().get
  val second = application.wallet.generateNewAccount().get
  val third = application.wallet.generateNewAccount().get

  private implicit val state = application.storedState

  override def beforeAll(): Unit = {
    super.beforeAll()
    assetTransfer(richBalance, first, 200 * Constants.UnitsInWave)
    assetTransfer(richBalance, second, 100 * Constants.UnitsInWave)
    assetTransfer(richBalance, third, 100 * Constants.UnitsInWave)
  }

  private def assertBalances(acc: Account, balance: Long, effectiveBalance: Long): Unit = {
    state.balance(acc) shouldBe balance
    state.effectiveBalance(acc) shouldBe effectiveBalance
  }

  private var createdLeaseTxId: String = _

  test("leasing is works correctly") {
    assertBalances(first, 200 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

    createdLeaseTxId = lease(first, second, 100 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave).get

    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
  }

  test("Сan not make leasing without having enough money") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    lease(second, first, 101 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave, assertSuccess = false)

    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
  }

  test("Can not make transfer without having enough of your own money") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    assetTransfer(second, first, 101 * Constants.UnitsInWave, assertSuccess = false)

    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
  }


  test("Can not make transfer without having enough of effective balance") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    assetTransfer(first, second, 91 * Constants.UnitsInWave, assertSuccess = false)

    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
  }

  test("Can not make leasing without having enough waves for fee") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    lease(first, second, 88 * Constants.UnitsInWave, assertSuccess = false, fee = 3 * Constants.UnitsInWave)

    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
  }


  test("leasing cancel is works correctly") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    cancelLease(first, createdLeaseTxId, fee = 10 * Constants.UnitsInWave)

    assertBalances(first, 180 * Constants.UnitsInWave, 180 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
  }
}
