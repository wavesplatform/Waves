package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.{TransactionGen, crypto}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

import scala.util.Success

class IssueTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {
  property("Issue serialization roundtrip") {
    forAll(issueGen) { issue: IssueTransaction =>
      val recovered = issue.builder.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("IssueV1 decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "A9I8SiXiOfL4M+2UuZaYcy/LEWk8DfNWNB0qGuxC1cRB5oGjqUQg2T0VhhCFH6g82BUU7hQqvuRxR7XlfTpYWosDORgnJbABGUmTh1Y57XKnwfXyCOhJ6azLy4FjFJ3+CRQABhAYZiBGFQL1EQ8jcD0TMA4Cfkp0MhkPXk8MNg0tM2sPYmkvTyNuJjtzFx8dSgxeBnQEPWcNbDgcIHorAx1lYj52CCcKKBZ5fSAJQT09egUdTyhffDxGUmlAIjYvXF0OYyxBdAZ8PkNQdih+UAJNECZ9RltXMENnYDwtfXcKAx5BLwQQdAw5D1JIcRQdPz11WwY8EkVWWABUN3MmVD9ISF58dRAMaWprfjkeAGxvW30NDQdpUThNWGckQB4MNXACKkx1VgN4Rm00e0AMVnsrNnUGdVxrNX4mOzIkUS4DJANzEmomAV9eUSNUCjhKcHg/exRbcWhZGGNWLHcQNyMLPTcXImRZAStlUx57DHBjDTgOQxouD0t7eiwtJwxgJWstFgVSLHAXVWVBfV5ydGVteSA4DWtXEVwQMXEMUkgVTBYvODhwey9mX0hCSzIcJjV4W2kreT4faDsFfltNHRxoCDJhHE0nUxwsL08ZBHcfQ2Y3VlgCdwZuVG1OSRd3C115fGh8VSEUL3QrbBYYBSB2WlBKfSgxbSdLXBYaWARWH394YAQ1fyVoKBNgQQgcTwxVZG06cVJjBnBEdyh9YHspPVM3NE8XLhxeORkjU1l7MSgrd0taIkpCbDVWcwhuPHdjXzQ8Ci59RFpAfFAOXXBZQxZ0GnpwU1V6BW5gdhF1G1AnFSE6WG9JFxl/WCIPBThQcDRtUyIwDDsTNyx1TGZoUnlhMkJiVAQxfHwWVyVLNBhifScAeg0PYnhTfXFDHgslUWJTFWQQJ0QVXEsjITtUYV0yUC9qXEUFExgbaVsKN1hFVEFXEHpPOVpqKxIvaXc8chM1JiNlH3p0Dx1FVTg4FmYrNg9CWiMQMGlSf1RraBQhPyN3fBsbd18ENkwnTlpEGSA7KWRdN3YOMn0aWipAQgIyc0ckWj8oFxYYGh0hf3Q6XyMDJEJBU014B2lBK3RCVl0wTVooaWAZIklNQQ5JSldRWEUhAxxaLSheGDtaXDVFP08bNzMyAh9pYy08JgAADog3+LCwBAEAAAAACGQt/wAADXhJOZOm"
    )
    val json = Json.parse(Base64.decode("ewogICJzZW5kZXJQdWJsaWNLZXkiIDogIjRxc2JuV2pnMzM4ZlNZMnV1UnIxcFZ6eUJnNkJ3VjhqMzdBZEVrNVpaaVJaIiwKICAicXVhbnRpdHkiIDogMTU5NzgyMTczODYxNjAsCiAgInNpZ25hdHVyZSIgOiAiNUNucWJKYlZvaEN0aDRYRVo0NnM3eHpzOXk0RW15VTJmaFdNMlBQN1Z4eDVTYUhjUVdyU01LMmlSbzFRUTF5akdLZjVVUk43V0FOU0N1cnZwQjZxUlZNdCIsCiAgImZlZSIgOiAxNDA3ODMxMDMsCiAgImRlc2NyaXB0aW9uIiA6ICJcdTAwMTFcdTAwMEYjcD1cdTAwMTMwXHUwMDBFXHUwMDAyfkp0Mlx1MDAxOVx1MDAwRl5PXGY2XHItM2tcdTAwMEZiaS9PI24mO3NcdTAwMTdcdTAwMUZcdTAwMURKXGZeXHUwMDA2dFx1MDAwND1nXHJsOFx1MDAxQyB6K1x1MDAwM1x1MDAxRGViPnZcYidcbihcdTAwMTZ5fSBcdEE9PXpcdTAwMDVcdTAwMURPKF98PEZSaUBcIjYvXFxdXHUwMDBFYyxBdFx1MDAwNnw+Q1B2KH5QXHUwMDAyTVx1MDAxMCZ9RltXMENnYDwtfXdcblx1MDAwM1x1MDAxRUEvXHUwMDA0XHUwMDEwdFxmOVx1MDAwRlJIcVx1MDAxNFx1MDAxRD89dVtcdTAwMDY8XHUwMDEyRVZYXHUwMDAwVDdzJlQ/SEhefHVcdTAwMTBcZmlqa345XHUwMDFFXHUwMDAwbG9bfVxyXHJcdTAwMDdpUThNWGckQFx1MDAxRVxmNXBcdTAwMDIqTHVWXHUwMDAzeEZtNHtAXGZWeys2dVx1MDAwNnVcXGs1fiY7MiRRLlx1MDAwMyRcdTAwMDNzXHUwMDEyaiZcdTAwMDFfXlEjVFxuOEpweD97XHUwMDE0W3FoWVx1MDAxOGNWLHdcdTAwMTA3I1x1MDAwQj03XHUwMDE3XCJkWVx1MDAwMStlU1x1MDAxRXtcZnBjXHI4XHUwMDBFQ1x1MDAxQS5cdTAwMEZLe3osLSdcZmAlay1cdTAwMTZcdTAwMDVSLHBcdTAwMTdVZUF9XnJ0ZW15IDhccmtXXHUwMDExXFxcdTAwMTAxcVxmUkhcdTAwMTVMXHUwMDE2Lzg4cHsvZl9IQksyXHUwMDFDJjV4W2kreT5cdTAwMUZoO1x1MDAwNX5bTVx1MDAxRFx1MDAxQ2hcYjJhXHUwMDFDTSdTXHUwMDFDLC9PXHUwMDE5XHUwMDA0d1x1MDAxRkNmN1ZYXHUwMDAyd1x1MDAwNm5UbU5JXHUwMDE3d1x1MDAwQl15fGh8VSFcdTAwMTQvdCtsXHUwMDE2XHUwMDE4XHUwMDA1IHZaUEp9KDFtJ0tcXFx1MDAxNlx1MDAxQVhcdTAwMDRWXHUwMDFGf3hgXHUwMDA0NX8laChcdTAwMTNgQVxiXHUwMDFDT1xmVWRtOnFSY1x1MDAwNnBEdyh9YHspPVM3NE9cdTAwMTcuXHUwMDFDXjlcdTAwMTkjU1l7MSgrd0taXCJKQmw1VnNcYm48d2NfNDxcbi59RFpAfFBcdTAwMEVdcFlDXHUwMDE2dFx1MDAxQXpwU1V6XHUwMDA1bmB2XHUwMDExdVx1MDAxQlAnXHUwMDE1ITpYb0lcdTAwMTdcdTAwMTl/WFwiXHUwMDBGXHUwMDA1OFBwNG1TXCIwXGY7XHUwMDEzNyx1TGZoUnlhMkJiVFx1MDAwNDF8fFx1MDAxNlclSzRcdTAwMThifSdcdTAwMDB6XHJcdTAwMEZieFN9cUNcdTAwMUVcdTAwMEIlUWJTXHUwMDE1ZFx1MDAxMCdEXHUwMDE1XFxLIyE7VGFdMlAvalxcRVx1MDAwNVx1MDAxM1x1MDAxOFx1MDAxQmlbXG43WEVUQVdcdTAwMTB6TzlaaitcdTAwMTIvaXc8clx1MDAxMzUmI2VcdTAwMUZ6dFx1MDAwRlx1MDAxREVVODhcdTAwMTZmKzZcdTAwMEZCWiNcdTAwMTAwaVJ/VGtoXHUwMDE0IT8jd3xcdTAwMUJcdTAwMUJ3X1x1MDAwNDZMJ05aRFx1MDAxOSA7KWRdN3ZcdTAwMEUyfVx1MDAxQVoqQEJcdTAwMDIyc0ckWj8oXHUwMDE3XHUwMDE2XHUwMDE4XHUwMDFBXHUwMDFEIX90Ol8jXHUwMDAzJEJBU014XHUwMDA3aUErdEJWXTBNWihpYFx1MDAxOVwiSU1BXHUwMDBFSUpXUVhFIVx1MDAwM1x1MDAxQ1otKF5cdTAwMTg7WlxcNUU/T1x1MDAxQjczMlx1MDAwMlx1MDAxRmljLTwmIiwKICAidHlwZSIgOiAzLAogICJ2ZXJzaW9uIiA6IDEsCiAgInJlaXNzdWFibGUiIDogdHJ1ZSwKICAic2VuZGVyIiA6ICIzTXpYV3I5czJ6VFVWNWNnakpmRXBrcjV4cDNrazJjeVhSaiIsCiAgImZlZUFzc2V0SWQiIDogbnVsbCwKICAicHJvb2ZzIiA6IFsgIjVDbnFiSmJWb2hDdGg0WEVaNDZzN3h6czl5NEVteVUyZmhXTTJQUDdWeHg1U2FIY1FXclNNSzJpUm8xUVExeWpHS2Y1VVJON1dBTlNDdXJ2cEI2cVJWTXQiIF0sCiAgImFzc2V0SWQiIDogIkg3aHFIcVhlTlR6dmlLelJ1cE5IdWR5TmpkUmIzcERrc0wzNmdGYktjdmljIiwKICAiZGVjaW1hbHMiIDogNCwKICAibmFtZSIgOiAiXHUwMDEwXHUwMDE4ZiBGXHUwMDE1IiwKICAiaWQiIDogIkg3aHFIcVhlTlR6dmlLelJ1cE5IdWR5TmpkUmIzcERrc0wzNmdGYktjdmljIiwKICAidGltZXN0YW1wIiA6IDE0ODEwMjc1NzQ2NzI2Cn0="))

    val tx = IssueTransaction.serializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("Issue serialization from TypedTransaction") {
    forAll(issueGen) { issue: IssueTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 3,
                       "id": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "feeAssetId": null,
                       "timestamp": 1526287561757,
                       "version": 1,
                       "signature": "28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ",
                       "proofs": ["28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ"],
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "name": "Gigacoin",
                       "quantity": 10000000000,
                       "reissuable": true,
                       "decimals": 8,
                       "description": "Gigacoin"
                       }
    """)

    val tx = IssueTransaction
      .create(
        TxVersion.V1,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "Gigacoin".getBytes("UTF-8"),
        "Gigacoin".getBytes("UTF-8"),
        10000000000L,
        8,
        true,
        script = None,
        100000000,
        1526287561757L,
        Proofs(ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get)
      )
      .right
      .get

    tx.json() shouldEqual js
  }

}
