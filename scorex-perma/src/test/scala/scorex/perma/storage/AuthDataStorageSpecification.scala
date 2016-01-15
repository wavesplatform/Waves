package scorex.perma.storage

import java.io.File

import org.scalacheck.Arbitrary
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.ads.merkle.AuthDataBlock


class AuthDataStorageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val treeDirName = "/tmp/scorex-test/test/AuthDataStorageSpecification/"
  val treeDir = new File(treeDirName)
  treeDir.mkdirs()

  val keyVal = for {
    key: Long <- Arbitrary.arbitrary[Long]
    value <- Arbitrary.arbitrary[String]
  } yield (key, AuthDataBlock(value.getBytes, Seq()))


  property("set value and get it") {
    lazy val storage = new AuthDataStorage(treeDirName + "/test_db")

    forAll(keyVal) { x =>
      val key = x._1
      val value = x._2
      storage.set(key, value)

      assert(storage.get(key).get.data sameElements value.data)
    }
    storage.close()
  }
}