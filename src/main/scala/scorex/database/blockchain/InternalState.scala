package scorex.database.blockchain

import java.io.File

import org.mapdb.{Serializer, DBMaker}

import scala.util.Try


// Store current balances only, and balances changes within effective balance depth.
// Store transactions for selected accounts only.

// Make design ready for pruning!
// Make possibility of easy switching underlying storage implementation(e.g. from MapDb to Riak)

class InternalState {
  private val database = DBMaker.newFileDB(new File(s"/tmp/state"))
    .closeOnJvmShutdown()
    .cacheSize(2048)
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  database.rollback() //initial rollback

  private val balances = database.createHashMap("balances").makeOrGet[String, Long]()


//  Try(database.createAtomicVar(SEED, seed, Serializer.BYTE_ARRAY)).getOrElse(database.getAtomicVar(SEED).set(seed))


}
