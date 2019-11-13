package com.wavesplatform.transaction

import cats.kernel.Monoid
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{Global, utils}
import com.wavesplatform.state.HistoryTest
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.{TransactionGen, WithDB, crypto}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

import scala.util.Success

class IssueTransactionV2Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen with WithDB with HistoryTest {

  property("IssueV2 serialization roundtrip") {
    forAll(issueV2TransactionGen()) { tx: IssueTransaction =>
      val recovered = IssueTransaction.parseBytes(tx.bytes()).get

      tx.sender.stringRepr shouldEqual recovered.sender.stringRepr
      tx.timestamp shouldEqual recovered.timestamp
      tx.decimals shouldEqual recovered.decimals
      tx.description shouldEqual recovered.description
      tx.script shouldEqual recovered.script
      tx.reissuable shouldEqual recovered.reissuable
      tx.fee shouldEqual recovered.fee
      tx.name shouldEqual recovered.name
      tx.chainByte shouldEqual recovered.chainByte
      tx.bytes() shouldEqual recovered.bytes()
    }
  }

  property("IssueV2 decode pre-encoded bytes") {
    val bytes = Base64.decode(
      "AAMCVA/MCl3Fl2LdfZZq8QFFqSYMKmIKmmKx/bS0l/ihx+ZvAAVCRwtcQQMwDjIdPzIqO0FiemJoKXVPG39QPn8nCz8dNm8WUUZtJVZwam9vG0JUVnIXCDIOci9aUFhoQk40TiQ0bHEZR0ZhWEJMOx4JWWUpAWVQMGkeBDxOZ3AzMXA9N20CUB9PCxR0BQdIQ0wUGFBALhRfFWl9KlN2fGcJSUUaGnlcCS5FOg5IV2UsWjYOUQAUCBFMdDgEJCZUD2coBUV1Wz9xKAN3agt3RhQLIzwSIxxHQVFFclkzRlFsSyoSHV1gPhZoRBJGVDgVIzp1RgYtJhYxf1UOAERQfAQ7O2dnOkZIXF4lWAVhBix2aBUjXSAlamkdXj44aQ8GbBMHIXoCcWRMHyBHNxYNTWR8CgVeQSADR04qWSsGfkBGOUEnIwp+f0UfKCc7DSpacz5tVC19SXJqcgZ7fyg9ZX9MFTkQb1BeMTF+YComG30xNSNARydHZhZKdAI8CkhUL15fRFMxbCkJCQYgKTpFbhkzEWoyXQ9BHSItbnQqZQ0FQzs4ORBELQVZRkt2HRIMHyZMWw0mRnh/B0MPDmBKaV4eOgchDCAzQQxHYl0PfkwFUCJqQQNDSwE2KDQTd1kNYAY3PjpIGnkVZSIiW0hiLSkqKHIDL3tJDhgEBU8XIxUtK1h+VTQIWSkOHUwaWWk0TmJFAHtcVhYmHXVPCAJ9VjF4IwJtMRBaVFFbSkIidn8tSQV5XWIIDAUMG3kBLEZ8BVsLJ2NjRx5XdghZUlk7GGkQYyRKXjEHcltNC30OADEEBkJzbl03W1B5aTA2VEMOUFBzeXh7XTFHBQZOZQZKRw54MjIbc3AeTDIlaEVTFn08aWUHHkhfDHwYKG0OJhUzLz1CNnFuJA46fGACRnREOykZUQMMK2EETUwBG18WWHwhNDwJZws3BW9JSEMLMCRLfAZSd0F1diAOfWoKBXw8Fh9YNUFtdww6DGwpVm1obFQKZBNNBSoFRmYFNy8/CmwNK1YZGjteaRNLMjhiVFFmQzQNEC9SD1gCRSUYPHR6cGJFF1pvJxVGTkhfSRFfeVQZXi0TXV0+HwVodAonNxBYfwIAFBAIOjs7ERxrLn8AIhQvPyZ2BVMYeW9HewsgAOM/YPJ/T5YEAAAAAAAHfRRRAABCXq5jfM4BAEIBCgAAAAABeAkAAGQAAAACAAAAAAAAAAABAAAAAAAAAAABCgAAAAABdgMGBgYKAAAAAAFwBgoAAAAAAXoGBrzjznYBAAEAQA4OxODRudknq82hTdXEhEqBy/0q8MNelEMpWDkjvrXz5juUyjloVQhIdAjZP05j4wnLn8A7txhPjs8JFfFeYQY="
    )
    val json = Json.parse(Base64.decode("ewogICJzZW5kZXJQdWJsaWNLZXkiIDogIjI0ZmE0QnREcVJlWVE3ejZhUlcxUzRtVEQ5NWFjdGE2eTU1SFAzcnVzNHljIiwKICAicXVhbnRpdHkiIDogNjM5NjQ1MDUzMzExNTg5MzQsCiAgImZlZSIgOiAxMjU2Mzc3MTMsCiAgImRlc2NyaXB0aW9uIiA6ICJcdTAwMEUyXHUwMDFEPzIqO0FiemJoKXVPXHUwMDFCf1A+fydcdTAwMEI/XHUwMDFENm9cdTAwMTZRRm0lVnBqb29cdTAwMUJCVFZyXHUwMDE3XGIyXHUwMDBFci9aUFhoQk40TiQ0bHFcdTAwMTlHRmFYQkw7XHUwMDFFXHRZZSlcdTAwMDFlUDBpXHUwMDFFXHUwMDA0PE5ncDMxcD03bVx1MDAwMlBcdTAwMUZPXHUwMDBCXHUwMDE0dFx1MDAwNVx1MDAwN0hDTFx1MDAxNFx1MDAxOFBALlx1MDAxNF9cdTAwMTVpfSpTdnxnXHRJRVx1MDAxQVx1MDAxQXlcXFx0LkU6XHUwMDBFSFdlLFo2XHUwMDBFUVx1MDAwMFx1MDAxNFxiXHUwMDExTHQ4XHUwMDA0JCZUXHUwMDBGZyhcdTAwMDVFdVs/cShcdTAwMDN3alx1MDAwQndGXHUwMDE0XHUwMDBCIzxcdTAwMTIjXHUwMDFDR0FRRXJZM0ZRbEsqXHUwMDEyXHUwMDFEXWA+XHUwMDE2aERcdTAwMTJGVDhcdTAwMTUjOnVGXHUwMDA2LSZcdTAwMTYxf1VcdTAwMEVcdTAwMDBEUHxcdTAwMDQ7O2dnOkZIXFxeJVhcdTAwMDVhXHUwMDA2LHZoXHUwMDE1I10gJWppXHUwMDFEXj44aVx1MDAwRlx1MDAwNmxcdTAwMTNcdTAwMDchelx1MDAwMnFkTFx1MDAxRiBHN1x1MDAxNlxyTWR8XG5cdTAwMDVeQSBcdTAwMDNHTipZK1x1MDAwNn5ARjlBJyNcbn5/RVx1MDAxRignO1xyKlpzPm1ULX1JcmpyXHUwMDA2e38oPWV/TFx1MDAxNTlcdTAwMTBvUF4xMX5gKiZcdTAwMUJ9MTUjQEcnR2ZcdTAwMTZKdFx1MDAwMjxcbkhUL15fRFMxbClcdFx0XHUwMDA2ICk6RW5cdTAwMTkzXHUwMDExajJdXHUwMDBGQVx1MDAxRFwiLW50KmVcclx1MDAwNUM7ODlcdTAwMTBELVx1MDAwNVlGS3ZcdTAwMURcdTAwMTJcZlx1MDAxRiZMW1xyJkZ4f1x1MDAwN0NcdTAwMEZcdTAwMEVgSmleXHUwMDFFOlx1MDAwNyFcZiAzQVxmR2JdXHUwMDBGfkxcdTAwMDVQXCJqQVx1MDAwM0NLXHUwMDAxNig0XHUwMDEzd1lccmBcdTAwMDY3PjpIXHUwMDFBeVx1MDAxNWVcIlwiW0hiLSkqKHJcdTAwMDMve0lcdTAwMEVcdTAwMThcdTAwMDRcdTAwMDVPXHUwMDE3I1x1MDAxNS0rWH5VNFxiWSlcdTAwMEVcdTAwMURMXHUwMDFBWWk0TmJFXHUwMDAwe1xcVlx1MDAxNiZcdTAwMUR1T1xiXHUwMDAyfVYxeCNcdTAwMDJtMVx1MDAxMFpUUVtKQlwidn8tSVx1MDAwNXldYlxiXGZcdTAwMDVcZlx1MDAxQnlcdTAwMDEsRnxcdTAwMDVbXHUwMDBCJ2NjR1x1MDAxRVd2XGJZUlk7XHUwMDE4aVx1MDAxMGMkSl4xXHUwMDA3cltNXHUwMDBCfVx1MDAwRVx1MDAwMDFcdTAwMDRcdTAwMDZCc25dN1tQeWkwNlRDXHUwMDBFUFBzeXh7XTFHXHUwMDA1XHUwMDA2TmVcdTAwMDZKR1x1MDAwRXgyMlx1MDAxQnNwXHUwMDFFTDIlaEVTXHUwMDE2fTxpZVx1MDAwN1x1MDAxRUhfXGZ8XHUwMDE4KG1cdTAwMEUmXHUwMDE1My89QjZxbiRcdTAwMEU6fGBcdTAwMDJGdEQ7KVx1MDAxOVFcdTAwMDNcZithXHUwMDA0TUxcdTAwMDFcdTAwMUJfXHUwMDE2WHwhNDxcdGdcdTAwMEI3XHUwMDA1b0lIQ1x1MDAwQjAkS3xcdTAwMDZSd0F1diBcdTAwMEV9alxuXHUwMDA1fDxcdTAwMTZcdTAwMUZYNUFtd1xmOlxmbClWbWhsVFxuZFx1MDAxM01cdTAwMDUqXHUwMDA1RmZcdTAwMDU3Lz9cbmxccitWXHUwMDE5XHUwMDFBO15pXHUwMDEzSzI4YlRRZkM0XHJcdTAwMTAvUlx1MDAwRlhcdTAwMDJFJVx1MDAxODx0enBiRVx1MDAxN1pvJ1x1MDAxNUZOSF9JXHUwMDExX3lUXHUwMDE5Xi1cdTAwMTNdXT5cdTAwMUZcdTAwMDVodFxuJzdcdTAwMTBYf1x1MDAwMlx1MDAwMFx1MDAxNFx1MDAxMFxiOjs7XHUwMDExXHUwMDFDay5/XHUwMDAwXCJcdTAwMTQvPyZ2XHUwMDA1U1x1MDAxOHlvR3tcdTAwMEIgIiwKICAidHlwZSIgOiAzLAogICJ2ZXJzaW9uIiA6IDIsCiAgInJlaXNzdWFibGUiIDogZmFsc2UsCiAgInNjcmlwdCIgOiAiYmFzZTY0OkFRb0FBQUFBQVhnSkFBQmtBQUFBQWdBQUFBQUFBQUFBQVFBQUFBQUFBQUFBQVFvQUFBQUFBWFlEQmdZR0NnQUFBQUFCY0FZS0FBQUFBQUY2QmdhODQ4NTIiLAogICJzZW5kZXIiIDogIjNOQm1RUjR3cVlyNFZZbzVrQ3JiWlpIR1hrcHo2aTNvRDFXIiwKICAiZmVlQXNzZXRJZCIgOiBudWxsLAogICJjaGFpbklkIiA6IDg0LAogICJwcm9vZnMiIDogWyAiSEpVcXZSNFRXRXJ6blcyZVZZa2FVUE1oaHJ3RlZOMkJBQTU2N1BiMWNubmVncHRRb05qaEJqbXlYWkZRVHpvckFUV0RxSGFSRW9oTGNydmhZcUNHVHhaIiBdLAogICJhc3NldElkIiA6ICJEaGtGeFFITHE5NHhvNmRkOFlFclhkb2hrUG8yVXRwUDg2bmdlS1hyYWV6OCIsCiAgImRlY2ltYWxzIiA6IDQsCiAgIm5hbWUiIDogIkJHXHUwMDBCXFxBIiwKICAiaWQiIDogIkRoa0Z4UUhMcTk0eG82ZGQ4WUVyWGRvaGtQbzJVdHBQODZuZ2VLWHJhZXo4IiwKICAidGltZXN0YW1wIiA6IDcyOTc0NDIwMTE0NjM4Cn0="))

    val tx = IssueTransaction.serializer.parseBytes(bytes).get
    tx.json() shouldBe json
    assert(crypto.verify(tx.signature, tx.bodyBytes(), tx.sender), "signature should be valid")
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                       "type": 3,
                       "id": "2ykNAo5JrvNCcL8PtCmc9pTcNtKUy2PjJkrFdRvTfUf4",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "feeAssetId": null,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g"
                       ],
                       "version": 2,
                       "assetId": "2ykNAo5JrvNCcL8PtCmc9pTcNtKUy2PjJkrFdRvTfUf4",
                       "chainId": 84,
                       "name": "Gigacoin",
                       "quantity": 10000000000,
                       "reissuable": true,
                       "decimals": 8,
                       "description": "Gigacoin",
                       "script":null
                       }
    """)

    val tx = IssueTransaction.create(TxVersion.V2, PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "Gigacoin".getBytes("UTF-8"),
        "Gigacoin".getBytes("UTF-8"),
        10000000000L,
        8,
        true,
        None,
        100000000,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("Contract script on asset isn't allowed") {
    val contract = {
      val script =
        s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE CONTRACT #-}
          |
          |@Verifier(txx)
          |func verify() = {
          |    true
          |}
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val ctx = {
      utils.functionCosts(V3)
      Monoid
        .combineAll(
          Seq(
            PureContext.build(Global, V3).withEnvironment[Environment],
            CryptoContext.build(Global, V3).withEnvironment[Environment],
            WavesContext.build(
              DirectiveSet(V3, Account, Expression).explicitGet()
            )
          ))
    }

    val script = ContractScript(V3, compiler.ContractCompiler(ctx.compilerContext, contract, V3).explicitGet())

    val tx = IssueTransaction.create(TxVersion.V2, PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        "Gigacoin".getBytes("UTF-8"),
        "Gigacoin".getBytes("UTF-8"),
        10000000000L,
        8,
        true,
        script.toOption,
        100000000,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
      )

    tx shouldBe 'left
  }
}
