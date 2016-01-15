package scorex

import org.scalatest.Suites
import scorex.perma.consensus.PermaConsensusBlockFieldSpecification
import scorex.perma.storage.AuthDataStorageSpecification

class PermaTestSuite extends Suites(
  new AuthDataStorageSpecification,
  new PermaConsensusBlockFieldSpecification
)
