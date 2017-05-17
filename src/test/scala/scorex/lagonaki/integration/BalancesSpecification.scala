package scorex.lagonaki.integration

import com.wavesplatform.settings.Constants
import org.scalatest.{FunSuite, Matchers}
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.transaction.AssetAcc

class BalancesSpecification extends FunSuite with Matchers with scorex.waves.TestingCommons  {
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

  private def assertAssetBalances(acc: AssetAcc, balance: Long): Unit = {
    state.assetBalance(acc) shouldBe balance
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

  test("Can not make transfer without having enough of own money") {
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

  test("Can not make cancel leasing from another sender") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    cancelLease(third, createdLeaseTxId, fee = 10 * Constants.UnitsInWave, assertSuccess = false)

    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
  }

  test("Can not make leasing cancel from another account") {
    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

    cancelLease(first, createdLeaseTxId, fee = 10 * Constants.UnitsInWave)

    cancelLease(first, createdLeaseTxId, fee = 10 * Constants.UnitsInWave, assertSuccess = false)

    assertBalances(first, 180 * Constants.UnitsInWave, 180 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
  }

  var issuedAssetId: String = _

  test("Assets issue should not lead to a change in the balance and the effective balance only on the fee") {
    assertBalances(first, 180 * Constants.UnitsInWave, 180 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

    issuedAssetId = issue(first, "name", "description", 100000, 2, reissuable = true, fee = 10 * Constants.UnitsInWave).get

    assertBalances(first, 170 * Constants.UnitsInWave, 170 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 100000)
  }

  test("Assets reissue should not lead to a change in the balance and the effective balance only on the fee") {
    assertBalances(first, 170 * Constants.UnitsInWave, 170 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

    reissue(first, issuedAssetId, 100000, reissuable = true, fee = 10 * Constants.UnitsInWave).get

    assertBalances(first, 160 * Constants.UnitsInWave, 160 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 200000)
  }

  test("Assets transfer should not lead to a change in the balance and the effective balance, only on the fee") {
    assertBalances(first, 160 * Constants.UnitsInWave, 160 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

    assetTransfer(first, second, 100000, fee = 10 * Constants.UnitsInWave, Some(issuedAssetId))

    assertBalances(first, 150 * Constants.UnitsInWave, 150 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 100000)
    assertAssetBalances(AssetAcc(second, Some(Base58.decode(issuedAssetId).get)), 100000)
  }

  test("Waves transfer should lead to a change in the balance and the effective balance and on the fee") {
    assertBalances(first, 150 * Constants.UnitsInWave, 150 * Constants.UnitsInWave)
    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

    assetTransfer(first, second, 10 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave)

    assertBalances(first, 130 * Constants.UnitsInWave, 130 * Constants.UnitsInWave)
    assertBalances(second, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
  }

  test("Waves payment should lead to a change in the balance and the effective balance and the fee") {
    assertBalances(first, 130 * Constants.UnitsInWave, 130 * Constants.UnitsInWave)
    assertBalances(second, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)

    payment(first, second, 10 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave)

    assertBalances(first, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
  }

  test("Burn should not lead to a change in the balance and the effective balance, only on the fee") {
    assertBalances(first, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 100000)
    assertAssetBalances(AssetAcc(second, Some(Base58.decode(issuedAssetId).get)), 100000)

    burn(first, issuedAssetId, 100000, fee = 10 * Constants.UnitsInWave)

    assertBalances(first, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 0)
    assertAssetBalances(AssetAcc(second, Some(Base58.decode(issuedAssetId).get)), 100000)
  }

  test("Can not make transfer without having enough of your own available money") {
    assertBalances(first, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
    assertBalances(third, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

    val l1 = lease(first, second, 90 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave).get
    val l2 = lease(second, third, 90 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave).get

    assertBalances(first, 99 * Constants.UnitsInWave, 9 * Constants.UnitsInWave)  // first has 9 own waves
    assertBalances(second, 119 * Constants.UnitsInWave, 119 * Constants.UnitsInWave) // second has 29 own waves
    assertBalances(third, 100 * Constants.UnitsInWave, 190 * Constants.UnitsInWave)

    assetTransfer(second, third, 30 * Constants.UnitsInWave, 1 * Constants.UnitsInWave, assertSuccess = false)

    cancelLease(first, l1, 1 * Constants.UnitsInWave)
    cancelLease(second, l2, 1 * Constants.UnitsInWave)

    assertBalances(first, 98 * Constants.UnitsInWave, 98 * Constants.UnitsInWave)
    assertBalances(second, 118 * Constants.UnitsInWave, 118 * Constants.UnitsInWave)
    assertBalances(third, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
  }
}
