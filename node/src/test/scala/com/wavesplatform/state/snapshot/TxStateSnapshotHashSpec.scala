package com.wavesplatform.state.snapshot

import com.google.common.primitives.Ints
import com.google.protobuf.ByteString.copyFrom as bs
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.crypto.fastHash
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.snapshot.{TransactionStatus, TransactionStateSnapshot as TSS}
import com.wavesplatform.protobuf.transaction.DataEntry
import com.wavesplatform.protobuf.{Amount, PBSnapshots}
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.bouncycastle.util.encoders.Hex

class TxStateSnapshotHashSpec extends PropSpec {
  private def hashInt(i: Int) = bs(fastHash(Ints.toByteArray(i)))

  val stateHash         = new StateHashBuilder
  private val signer101 = TxHelpers.signer(101)
  private val signer102 = TxHelpers.signer(102)
  private val signer103 = TxHelpers.signer(103)

  private val address1 = signer101.toAddress
  private val address2 = signer102.toAddress
  private val address3 = signer103.toAddress

  private val assetId1 = hashInt(0xaa22aa44)
  private val assetId2 = hashInt(0xbb22aa44)

  private val leaseId  = hashInt(0x11aaef22)
  private val orderId1 = hashInt(0xee23ef22)
  private val orderId2 = hashInt(0xbb77ef29)

  private val testScript = bs(TestCompiler(V6).compileExpression("true").bytes().arr)

  private val wavesBalances = TSS(balances =
    Seq(
      TSS.Balance(bs(address1.bytes), Some(Amount(amount = 10.waves))),
      TSS.Balance(bs(address2.bytes), Some(Amount(amount = 20.waves)))
    )
  )

  private val assetBalances = TSS(balances =
    Seq(
      TSS.Balance(bs(address1.bytes), Some(Amount(assetId1, 10_000))),
      TSS.Balance(bs(address2.bytes), Some(Amount(assetId2, 20_000)))
    )
  )

  private val dataEntries = TSS(accountData =
    Seq(
      TSS.AccountData(
        bs(address1.bytes),
        Seq(
          DataEntry("foo", DataEntry.Value.Empty),
          DataEntry("bar", DataEntry.Value.StringValue("StringValue")),
          DataEntry("baz", DataEntry.Value.BinaryValue(bs(address1.bytes)))
        )
      ),
      TSS.AccountData(
        bs(address2.bytes),
        Seq(
          DataEntry("foo", DataEntry.Value.IntValue(1200)),
          DataEntry("bar", DataEntry.Value.BoolValue(true))
        )
      )
    )
  )

  private val accountScript = TSS(accountScripts =
    Some(
      TSS.AccountScript(
        bs(signer101.publicKey.arr),
        testScript,
        250
      )
    )
  )

  private val assetScript = TSS(assetScripts = Some(TSS.AssetScript(assetId2, testScript)))

  private val newLease = TSS(
    leaseBalances = Seq(
      TSS.LeaseBalance(bs(address1.bytes), out = 45.waves),
      TSS.LeaseBalance(bs(address2.bytes), in = 55.waves)
    ),
    newLeases = Seq(
      TSS.NewLease(leaseId,  bs(signer101.publicKey.arr), bs(address2.bytes), 25.waves)
    )
  )

  private val cancelledLease = TSS(
    leaseBalances = Seq(
      TSS.LeaseBalance(bs(address3.bytes), out = 20.waves), TSS.LeaseBalance(bs(TxHelpers.address(104).bytes), in = 0.waves)),
    cancelledLeases = Seq(
      TSS.CancelledLease(leaseId)
    )
  )

  private val sponsorship = TSS(
    sponsorships = Seq(TSS.Sponsorship(assetId2, 5500))
  )

  private val alias = TSS(
    aliases = Some(TSS.Alias(bs(address2.bytes), "wavesevo"))
  )

  private val volumeAndFee = TSS(
    orderFills = Seq(
      TSS.OrderFill(orderId1, 10.waves, 2000),
      TSS.OrderFill(orderId2, 10.waves, 2000)
    )
  )

  private val newAsset = TSS(
    assetStatics = Seq(
      TSS.NewAsset(assetId1, hashInt(0x88aadd55), nft = true),
      TSS.NewAsset(assetId2, hashInt(0x88aadd55), decimals = 8)
    ),
    assetVolumes = Seq(
      TSS.AssetVolume(assetId2, true, bs((BigInt(Long.MaxValue) * 10).toByteArray)),
      TSS.AssetVolume(assetId1, false, bs(BigInt(1).toByteArray))
    ),
    assetNamesAndDescriptions = Seq()
  )

  private val reissuedAsset = TSS(
    assetVolumes = Seq(
      TSS.AssetVolume(hashInt(0x23aadd55), false, bs((BigInt(10000000_00L)).toByteArray)),
    )
  )
  private val renamedAsset = TSS(
    assetNamesAndDescriptions = Seq(
      TSS.AssetNameAndDescription(
        assetId2, "newname", "some fancy description"
      )
    )
  )
  private val failedTransaction = TSS(
    balances = Seq(
      TSS.Balance(bs(address2.bytes), Some(Amount(amount = 25.995.waves)))
    ),
    transactionStatus = TransactionStatus.FAILED
  )
  private val elidedTransaction = TSS(
    transactionStatus = TransactionStatus.ELIDED
  )

  private val all = TSS(
    assetBalances.balances ++ wavesBalances.balances,
    newLease.leaseBalances ++ cancelledLease.leaseBalances,
    newLease.newLeases,
    cancelledLease.cancelledLeases,
    newAsset.assetStatics,
    newAsset.assetVolumes ++ reissuedAsset.assetVolumes,
    newAsset.assetNamesAndDescriptions ++ renamedAsset.assetNamesAndDescriptions,
    newAsset.assetScripts,
    alias.aliases,
    volumeAndFee.orderFills,
    accountScript.accountScripts,
    dataEntries.accountData,
    sponsorship.sponsorships,
    failedTransaction.transactionStatus
  )

  private val testData = Table(
    ("clue", "state snapshot", "base64 bytes", "tx id", "previous state hash", "expected result"),
    (
      "waves balances",
      wavesBalances,
      "CiQKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgYQgJTr3AMKJAoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSBhCAqNa5Bw==",
      ByteStr.empty,
      "",
      "954bf440a83542e528fe1e650471033e42d97c5896cc571aec39fccc912d7db0"
    ),
    (
      "asset balances",
      assetBalances,
      "CkMKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EiUKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEJBOCkQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEiYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEKCcAQ==",
      ByteStr.empty,
      "954bf440a83542e528fe1e650471033e42d97c5896cc571aec39fccc912d7db0",
      "534e27c3a787536e18faf844ff217a8f14e5323dfcd3cc5b9ab3a8e261f60cf7"
    ),
    (
      "data entries",
      dataEntries,
      "YloKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgUKA2ZvbxISCgNiYXJqC1N0cmluZ1ZhbHVlEiEKA2JhemIaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7ViLwoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCAoDZm9vULAJEgcKA2JhclgB",
      ByteStr.empty,
      "534e27c3a787536e18faf844ff217a8f14e5323dfcd3cc5b9ab3a8e261f60cf7",
      "b1440780a268eeaf9f6bb285a97ee35582cb84382576e84432b2e61b86d64581"
    ),
    (
      "account script",
      accountScript,
      "Wi4KIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoEgcGAQaw0U/PGPoB",
      ByteStr.empty,
      "b1440780a268eeaf9f6bb285a97ee35582cb84382576e84432b2e61b86d64581",
      "ca42620b03b437e025bec14152c3f7d8ff65b8fb1062b5013363186484176cb7"
    ),
    (
      "asset script",
      assetScript,
      "QisKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEgcGAQaw0U/P",
      ByteStr.empty,
      "ca42620b03b437e025bec14152c3f7d8ff65b8fb1062b5013363186484176cb7",
      "4e9cbb5349a31d2954d57f67d2fc5cf73dd1ce90b508299cf5f92b1b45ca668f"
    ),
    (
      "new lease",
      newLease,
      "EiIKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1GICa4uEQEiIKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEICuzb4UGmYKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqNEiBQx1mvQnelVPDNtCyxR82wU73hEX9bBDsNfUlEuNBgaBoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoggPKLqAk=",
      ByteStr.empty,
      "4e9cbb5349a31d2954d57f67d2fc5cf73dd1ce90b508299cf5f92b1b45ca668f",
      "8615df0268bcc76e851a9925e07f212a875a1bd047b0cccbc4ad3d842895f16e"
    ),
    (
      "cancelled lease",
      cancelledLease,
      "EiIKGgFUMCPLqLW81X2Atgaj2KwF9QkaJq47Cev9GICo1rkHEhwKGgFUYSJd8vzI9rq7GdIuDy65JMc8zi497E98IiIKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqN",
      ByteStr.empty,
      "8615df0268bcc76e851a9925e07f212a875a1bd047b0cccbc4ad3d842895f16e",
      "3bb24694ea57c1d6b2eec2c549d5ba591853bd7f21959027d2793d5c0846cc8d"
    ),
    (
      "sponsorship",
      sponsorship,
      "aiUKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEPwq",
      ByteStr.empty,
      "3bb24694ea57c1d6b2eec2c549d5ba591853bd7f21959027d2793d5c0846cc8d",
      "4fd2ceeb81d4d9c7ebad4391fbd938cfc40564088d2cc71801d308d56eca9b75"
    ),
    (
      "alias",
      alias,
      "SiYKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgh3YXZlc2V2bw==",
      ByteStr.empty,
      "4fd2ceeb81d4d9c7ebad4391fbd938cfc40564088d2cc71801d308d56eca9b75",
      "0f02911227a9835c1248822f4e500213c4fc4c05a83a5d27680f67d1d1f6a8ee"
    ),
    (
      "order fill",
      volumeAndFee,
      "UisKIMkknO8yHpMUT/XKkkdlrbYCG0Dt+qvVgphfgtRbyRDMEICU69wDGNAPUisKIJZ9YwvJObbWItHAD2zhbaFOTFx2zQ4p0Xbo81GXHKeEEICU69wDGNAP",
      ByteStr.empty,
      "0f02911227a9835c1248822f4e500213c4fc4c05a83a5d27680f67d1d1f6a8ee",
      "4d0d2c893b435d1bbc3464c59ceda196961b94a81cfb9bb2c50fe03c06f23d00"
    ),
    (
      "new asset",
      newAsset,
      "KkYKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEiDcYGFqY9MotHTpDpskoycN/Mt62bZfPxIC4fpU0ZTBniABKkYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEiDcYGFqY9MotHTpDpskoycN/Mt62bZfPxIC4fpU0ZTBnhgIMi8KIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEAEaCQT/////////9jIlCiBeZp+CCmfQiG2HR4zwQ6KseFzHr1XcMc4QdTlL01CiWxoBAQ==",
      ByteStr.empty,
      "4d0d2c893b435d1bbc3464c59ceda196961b94a81cfb9bb2c50fe03c06f23d00",
      "e2baa6d7e863fc1f5f6cec326b01e577c4509e927f3b13ed7818af9075be82c3"
    ),
    (
      "reissued asset",
      reissuedAsset,
      "MigKIDhvjT3TTlJ+v4Ni205vcYc1m9WWgnQPFovjmJI1H62yGgQ7msoA",
      ByteStr.empty,
      "e2baa6d7e863fc1f5f6cec326b01e577c4509e927f3b13ed7818af9075be82c3",
      "a161ea70fa027f6127763cdb946606e3d65445915ec26c369e0ff28b37bee8cd"
    ),
    (
      "renamed asset",
      renamedAsset,
      "OkMKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEgduZXduYW1lGhZzb21lIGZhbmN5IGRlc2NyaXB0aW9u",
      ByteStr.empty,
      "a161ea70fa027f6127763cdb946606e3d65445915ec26c369e0ff28b37bee8cd",
      "7a43e1fb599e8a921ecb2a83b4b871bd46db4569bf5c4f6c9225479191450a58"
    ),
    (
      "failed transaction",
      failedTransaction,
      "CiQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgYQ4PHE1wlwAQ==",
      ByteStr(fastHash(Ints.toByteArray(0xaabbef20))),
      "7a43e1fb599e8a921ecb2a83b4b871bd46db4569bf5c4f6c9225479191450a58",
      "dfa190a84d59edda03428c93c4b1be4c50f12adf3c11528ef0bdd8db1edaf49b"
    ),
    (
      "elided transaction",
      elidedTransaction,
      "cAI=",
      ByteStr(fastHash(Ints.toByteArray(0xaabbef40))),
      "dfa190a84d59edda03428c93c4b1be4c50f12adf3c11528ef0bdd8db1edaf49b",
      "002f4f7f3741668c10a8ba92b4b183680fd6659bafd36037be6f9a636510b128"
    ),
    (
      "all together",
      all,
      "CkMKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EiUKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEJBOCkQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEiYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEKCcAQokChoBVGD9UO8g3kVxIH37nIi+fBwviiLHCtiPtRIGEICU69wDCiQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgYQgKjWuQcSIgoaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7UYgJri4RASIgoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoQgK7NvhQSIgoaAVQwI8uotbzVfYC2BqPYrAX1CRomrjsJ6/0YgKjWuQcSHAoaAVRhIl3y/Mj2ursZ0i4PLrkkxzzOLj3sT3waZgoguIIzLIWCBbxl3Ysa38C0yvtZan6R9ZvOU33eldmrOo0SIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoGhoBVELFyWNz9Q/YE1BgTxwU8qbJJWra/RmwKiCA8ouoCSIiCiC4gjMshYIFvGXdixrfwLTK+1lqfpH1m85Tfd6V2as6jSpGCiBeZp+CCmfQiG2HR4zwQ6KseFzHr1XcMc4QdTlL01CiWxIg3GBhamPTKLR06Q6bJKMnDfzLetm2Xz8SAuH6VNGUwZ4gASpGCiB4ncARI9U2D3CCr9S0ari/6LUWC7+1JsBD5fx4Ok+JThIg3GBhamPTKLR06Q6bJKMnDfzLetm2Xz8SAuH6VNGUwZ4YCDIvCiB4ncARI9U2D3CCr9S0ari/6LUWC7+1JsBD5fx4Ok+JThABGgkE//////////YyJQogXmafggpn0Ihth0eM8EOirHhcx69V3DHOEHU5S9NQolsaAQEyKAogOG+NPdNOUn6/g2LbTm9xhzWb1ZaCdA8Wi+OYkjUfrbIaBDuaygA6QwogeJ3AESPVNg9wgq/UtGq4v+i1Fgu/tSbAQ+X8eDpPiU4SB25ld25hbWUaFnNvbWUgZmFuY3kgZGVzY3JpcHRpb25KJgoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCHdhdmVzZXZvUisKIMkknO8yHpMUT/XKkkdlrbYCG0Dt+qvVgphfgtRbyRDMEICU69wDGNAPUisKIJZ9YwvJObbWItHAD2zhbaFOTFx2zQ4p0Xbo81GXHKeEEICU69wDGNAPWi4KIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoEgcGAQaw0U/PGPoBYloKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgUKA2ZvbxISCgNiYXJqC1N0cmluZ1ZhbHVlEiEKA2JhemIaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7ViLwoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCAoDZm9vULAJEgcKA2JhclgBaiUKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEPwqcAE=",
      ByteStr(fastHash(Ints.toByteArray(0xaabbef50))),
      "002f4f7f3741668c10a8ba92b4b183680fd6659bafd36037be6f9a636510b128",
      "a65304008a49f4ae10cd4af0e61c5d59ba048f0766846d9239d0b28275a0184b"
    )
  )

  property("correctly create transaction state snapshot hash from snapshot") {
    forAll(testData) { case (clue, pbSnapshot, b64str, txId, prev, expectedResult) =>
      withClue(clue) {
        TSS.parseFrom(Base64.decode(b64str)) shouldEqual pbSnapshot

        val (snapshot, meta) = PBSnapshots.fromProtobuf(pbSnapshot, txId, 10)
        val raw = Hex.toHexString(
          TxStateSnapshotHashBuilder
            .createHashFromSnapshot(snapshot, Some(TxStateSnapshotHashBuilder.TxStatusInfo(txId, meta)))
            .createHash(ByteStr.decodeBase64(prev).get)
            .arr
        )
        PBSnapshots.toProtobuf(snapshot, meta) shouldEqual pbSnapshot
        raw shouldEqual expectedResult
      }
    }
  }
}
