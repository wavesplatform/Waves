---
purpose: Implementation notes for the BlockchainUpdates stream extension (StateUpdate wiring)
---

# BlockchainUpdates stream

## Adding a field to `events.StateUpdate`

`events.StateUpdate` (`grpc-server/.../events/events.scala`) mirrors only a subset of `StateSnapshot`; anything it doesn't name is silently dropped from the event stream. Adding a field means touching all of these at once: the domain case class (plus `reverse`, or a ROLLBACK event reports the wrong direction), `fromPB`/`toPB`, the `Monoid[StateUpdate]` merge, `atomic`, and **`isEmpty`**. `serde`'s `Some(blockStateUpdate).filterNot(_.isEmpty)` (`events/protobuf/serde/package.scala`) drops the whole key-block/microblock update when `isEmpty` says so, so a field missing from `isEmpty` makes a block carrying only that field emit nothing at all.

The protobuf side lives in the external `com.wavesplatform:protobuf-schemas` artifact (`project/Dependencies.scala`), not in this repo, so a new wire field needs a schemas release first.
