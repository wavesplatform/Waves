package com.wavesplatform.database


final case class KeyTag(prefix: Short, tag: String)

object KeyTags {
  private[this] var counter = 0.toShort
  private[this] var allTags = Vector.empty[KeyTag]

  private def nextTag(tag: String): KeyTag = {
    val t = KeyTag(counter, tag)
    allTags = allTags :+ t
    counter = (counter + 1).toShort
    t
  }

  nextTag("version")
  nextTag("height")
  nextTag("score")
  nextTag("height-of")
  nextTag("waves-balance-history")
  nextTag("waves-balance")
  nextTag("asset-balance-history")
  nextTag("asset-balance")
  nextTag("asset-details-history")
  nextTag("asset-details")
  nextTag("lease-balance-history")
  nextTag("lease-balance")
  nextTag("lease-status-history")
  nextTag("lease-status")
  nextTag("filled-volume-and-fee-history")
  nextTag("filled-volume-and-fee")
  nextTag("changed-addresses")
  nextTag("address-id-of-alias")
  nextTag("last-address-id")
  nextTag("address-id")
  nextTag("id-to-address")
  nextTag("address-script-history")
  nextTag("address-script")
  nextTag("approved-features")
  nextTag("activated-features")
  nextTag("data-key-chunk-count")
  nextTag("data-key-chunk")
  nextTag("data-history")
  nextTag("data")
  nextTag("sponsorship-history")
  nextTag("sponsorship")
  nextTag("carry-fee")
  nextTag("asset-script-history")
  nextTag("asset-script")
  nextTag("safe-rollback-height")
  nextTag("changed-data-keys")
  nextTag("block-info-at-height")
  nextTag("nth-transaction-info-at-height")
  nextTag("address-transaction-seq-nr")
  nextTag("address-transaction-height-type-and-nums")
  nextTag("transaction-height-and-nums-by-id")
  nextTag("block-transactions-fee")
  nextTag("invoke-script-result")
  nextTag("block-reward")
  nextTag("waves-amount")
  nextTag("hit-source")
  nextTag("disabled-aliases")
  nextTag("asset-static-info")
  nextTag("nft-count")
  nextTag("nft-possession")
}
