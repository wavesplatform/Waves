package scorex.crypto.ads.merkle

import java.io.File

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import scorex.crypto.ads.merkle.TreeStorage.Key
import scorex.crypto.hash.CryptographicHash.Digest

import scala.reflect.io.Path
import scala.util.Try


class MerkleTreeStorageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with BeforeAndAfterAll {

  val treeDirName = "/tmp/scorex-msg-tests/test/MapDBStorageSpecification/"
  val treeDir = new File(treeDirName)
  treeDir.mkdirs()

  override protected def afterAll(): Unit = {
    val path: Path = Path ("/tmp/scorex-msg-tests")
    Try(path.deleteRecursively())
  }

  val dbFile = new File(treeDirName + "/db.file")
  val maxLevel = 50
  dbFile.delete()

  val keyVal = for {
    level: Int <- Gen.choose(1, maxLevel)
    key: Long <- Arbitrary.arbitrary[Long]
    value <- Arbitrary.arbitrary[String]
  } yield ((level, math.abs(key)), value.getBytes)


  property("set value and get it") {
    lazy val dirName = treeDirName + "/test_db"
    new File(dirName).mkdirs()
    lazy val storage = new TreeStorage(dirName, maxLevel)

    forAll(keyVal) { case(key: Key, value: Digest) =>
      whenever(key._1 >= 0.toLong && key._2 >= 0.toLong) {
        storage.set(key, value)
        assert(storage.get(key).get sameElements value)
      }
    }
    storage.close()
  }
}