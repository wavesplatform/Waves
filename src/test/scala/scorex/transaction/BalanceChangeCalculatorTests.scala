package scorex.transaction

import org.scalatest.{FreeSpec, Matchers}
import play.api.libs.json._
import scorex.account.{Account, AddressScheme}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.OrderJson._
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}

class BalanceChangeCalculatorTests extends FreeSpec with Matchers {
  private val Waves = 100000000L
  private val Fee = 100000L
  private val PartialFee = 40000L
  private val SoldAssets = 200L
  private val PriceForAsset = 2

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'I'
  }

  "ExchangeTransaction should produce correct balance changes" in {
    val buyOrder = Json.parse("""{"id":"5NBwSz4Q9mu2BGJPy4GXc6VuR1ujD8bJwRRw4AWFp1dG","senderPublicKey":"23tUEAH9Ms6ass8V68NrqJXoFvvjxXWCWn8wdSeqvFjS","matcherPublicKey":"Dym3rNfBR8J2KRrYESsQ9pC12pJ15W6HMWufgMv3oupq","assetPair":{"amountAsset":"8gQUzxe41hEXo9K1Ma7y2NU8a13t3xMjAnowZqztN6SH","priceAsset":null},"orderType":"buy","price":20000000000000000,"amount":200,"timestamp":1491386976004,"expiration":1493978975004,"matcherFee":100000,"signature":"4s58zd5UAikUmpVwwrXyCpnmhQTAQehyq1eLpq8nSV9aRjkbhMVdgH2BneopDJXuAc1XZqXSQib3w2XE4JiTJTAk"}""").as[Order]

    val sellOrder = Json.parse("""{"id":"3GyddDg9Mp3BhNHjhPeKDRjkDpmbDwRpUnBF3UvjhWhE","senderPublicKey":"G9ez3SeK76ckKrWiTF8FgoPvFmDovuBSE5waQhs5cH41","matcherPublicKey":"Dym3rNfBR8J2KRrYESsQ9pC12pJ15W6HMWufgMv3oupq","assetPair":{"amountAsset":"8gQUzxe41hEXo9K1Ma7y2NU8a13t3xMjAnowZqztN6SH","priceAsset":null},"orderType":"sell","price":20000000000000000,"amount":500,"timestamp":1491386975800,"expiration":1493978974800,"matcherFee":100000,"signature":"2Jtq2f2sSShwkTVJzwaK6o2zAqsyRQdvaf4uYQvEphqgWUFxtKwSYBBuSV2RbxDQHPvKNnDtuoKT193WLCo5uPQ"}""").as[Order]

    val tx = ExchangeTransaction.create(buyOrder, sellOrder, 200000000L * 100000000L, 200, 100000L, 40000L, 100000,
      1491386976025L,
      Base58.decode("2Fqk6gFTyTQsTmM4fB5t3Mh71PCamT32hFHu8ifkt6SC7WN3zkijbr1MB7qaRXq1pbNjEJa9sYCZXwi7RjkteVN").get)
      .right.get

    val changes = BalanceChangeCalculator.balanceChanges(null)(tx).right.get

    val matcherAddress = Account.fromString("3HevUqdcHuiLvpeVLo4sGVqxSsZczJuCYHo").right.get
    val aliceAddress = Account.fromString("3HPsvMYzyowd3Lyxj9PRAoZifWePHaGLkxU").right.get
    val bobAddress = Account.fromString("3HWgKQ7SWT1HHxevxDFRGRN6wFKxvGeAjhm").right.get

    val matcherWavesAccount = AssetAcc(matcherAddress, None)
    val aliceWavesAccount = AssetAcc(aliceAddress, None)
    val bobWavesAccount = AssetAcc(bobAddress, None)

    val aliceAssetAccount = AssetAcc(aliceAddress, Some(Base58.decode("8gQUzxe41hEXo9K1Ma7y2NU8a13t3xMjAnowZqztN6SH").get))
    val bobAssetAccount = AssetAcc(bobAddress, Some(Base58.decode("8gQUzxe41hEXo9K1Ma7y2NU8a13t3xMjAnowZqztN6SH").get))

    changes.size should be(7)
    changes should contain(BalanceChange(matcherWavesAccount, Fee - Fee + PartialFee)) // Matcher pays Fee for Exchange transaction and receives Fee and PartialFee
    changes should contain(BalanceChange(aliceAssetAccount, -SoldAssets)) // Alice sells assets
    changes should contain(BalanceChange(bobAssetAccount, SoldAssets)) // Bob receives assets
    changes should contain(BalanceChange(aliceWavesAccount, PriceForAsset * SoldAssets * Waves)) // Alice receives Waves for assets
    changes should contain(BalanceChange(bobWavesAccount, -PriceForAsset * SoldAssets * Waves)) // Bob spends Waves to buy assets
    changes should contain(BalanceChange(bobWavesAccount, -Fee)) // Bob pays full Fee for Order
    changes should contain(BalanceChange(aliceWavesAccount, -PartialFee)) // Alice pays partial fee for Order

  }
}
