package com.wavesplatform.api.common

import org.scalatest.{FreeSpec, Matchers}

class CommonAccountApiSpec extends FreeSpec with Matchers {
  "Waves distribution" - {
    "height is taken into account" in pending
    "balances only present in diff are not ignored" in pending
    "ignores zero balances" in pending
    "diff is ignored when height is less than blockchain height" in pending
    "diff is applied when height matches blockchain height" in pending
  }

  "Asset distribution" - {
    "works for NFT" in pending
    "balances only present in diff are not ignored" in pending
    "ignores zero balances" in pending
    "returns balances only for requested asset" in pending
    "height is taken into account" in pending
    "diff is ignored when height is less than blockchain height" in pending
    "diff is applied when height matches blockchain height" in pending
  }

  "Active leases for address" - {
    "does not return leases which were cancelled in diff" in pending
    "includes new leases from diff" in pending
  }

  "Portfolio" - {
    "includes NFT balances when ReducedNFTFee feature is inactive" in pending
    "excludes NFT balances when ReducedNFTFee feature is active" - {
      "from diff" in pending
      "from leveldb" in pending
    }
  }

  "NFT list" - {
    "does not include NFTs which were spent in diff" in pending
    "includes NFTs which were received in diff" in pending
  }
}
