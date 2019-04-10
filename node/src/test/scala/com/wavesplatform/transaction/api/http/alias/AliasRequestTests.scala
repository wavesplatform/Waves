package com.wavesplatform.transaction.api.http.alias

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import com.wavesplatform.api.http.alias.{CreateAliasV1Request, SignedCreateAliasV1Request}

class AliasRequestTests extends FunSuite with Matchers {
  test("CreateAliasRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
           "fee": 10000000,
           "alias": "ALIAS"
        }
      """

    val req = Json.parse(json).validate[CreateAliasV1Request].get

    req shouldBe CreateAliasV1Request("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7", "ALIAS", 10000000)
  }

  test("SignedCreateAliasRequest") {
    val json =
      """
         {
           "senderPublicKey": "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
           "fee": 100000,
           "alias": "ALIAS",
           "timestamp": 1488807184731,
           "signature": "3aB6cL1osRNopWyqBYpJQCVCXNLibkwM58dvK85PaTK5sLV4voMhe5E8zEARM6YDHnQP5YE3WX8mxdFp3ciGwVfy"
          }
       """

    val req = Json.parse(json).validate[SignedCreateAliasV1Request].get

    req shouldBe SignedCreateAliasV1Request(
      "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
      100000,
      "ALIAS",
      1488807184731L,
      "3aB6cL1osRNopWyqBYpJQCVCXNLibkwM58dvK85PaTK5sLV4voMhe5E8zEARM6YDHnQP5YE3WX8mxdFp3ciGwVfy"
    )
  }
}
