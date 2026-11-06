---
purpose: Implementation notes for block and microblock mining (Miner.scala, MicroBlockMinerImpl), including known races
---

# Mining

## Key block forged on a stale parent (fixed: forging runs on the appender)

The miner used to orphan its own newest microblock, even as the only miner with `min-micro-block-age = 0`. `MinerImpl.forgeBlock` ran on `minerScheduler` and took its parent from `bestLastBlockInfo`; the append was a separate task on the appender. A microblock appended in between (packed before the key block was forged, appended before the key block) was then discarded by the key block (`Discarded microblocks: List(...)` in the node log). Its transactions went back to the UTX pool and were mined again, but for about 200ms they were gone from the state. Seen in node-it (`IssueReissueBurnAssetSuite`, run 41668443292): a client saw a transfer in the liquid block, the next key block orphaned it, and a follow-up transaction failed with `negative waves balance`.

Now `generateBlockTask` forges and appends in one appender task (`forgeAndAppend`, using the synchronous `BlockAppender.appendBlock`). The appender is a single thread (`Application.scala`, `singleThread("appender")`), so the key block sees every microblock queued ahead of it, and a microblock packed meanwhile reaches the appender after the key block and is rejected: its transactions never left the UTX pool. Pinned by `LastMicroBlockSuite` ("key block is forged and appended within appender tasks ...").

Rules that keep this correct:

- Keep forge and append in one appender runnable. Any async boundary between them (`executeOn`, `Task.shift`, a separate `BlockAppender(...)` task) reopens the window.
- The task is cancelable until it starts: `scheduleMining` replaces `scheduledAttempts`, and a replaced attempt must not forge (otherwise a second wallet account forges on top of the block the first one just appended). Appending our own key block also cancels the running attempt, through `BlockchainUpdaterImpl` calling `scheduleMining`, so everything that must follow a successful append (broadcast, `startMicroBlockMining`) runs synchronously inside the task, never in a continuation.
- Do not fix ordering problems by cancelling `microBlockAttempt` before forging. A key-block attempt can give up (`PermanentFailure`, a failed append) without rescheduling anything; the node recovers only because each microblock append calls `scheduleMining(cancelMicroBlockMining = false)` (`BlockchainUpdaterImpl.processMicroBlock`). Without microblocks nothing would schedule mining again.

Checked and unchanged by the move:

- `BlockFromFuture` retry: `generateBlockTask(account, None)` was already continued on `appenderScheduler` (the attempt runs there and the old append task shifted back to it); only the forge moved. A `TemporaryFailure` retries the same way, now from the appender.
- `waitBlockAppendedTask` runs before the sleep and the forge, so where forging runs does not affect it.
- `BlockChallenger` builds and appends challenging blocks through its own path (`BlockAppender.apply`), untouched here; our key block always has `challengedHeader = None`.
- Finalization voting and endorsements: a key block carries `finalizationVoting = None` (votes go into microblocks, read from `EndorsementStorage` by `MicroBlockMinerImpl`), and `blockEndorser.vote` still runs inside `BlockAppender.appendBlock` after a successful append, as before.

Costs and behaviour to know:

- The appender now also computes consensus data, the light-node state hash and the signature of our key block, delaying peer appends by that much (not measured; under NG a key block has no transactions). Before NG activation it also packs UTX transactions on the appender, bounded by `micro-block-interval`.
- An attempt fires as soon as its delay passes, so if our newest microblocks are younger than `min-micro-block-age` it builds on an older liquid block (`bestReference`). If it then gives up there (a `PermanentFailure` or a failed append), for example because the account may only generate on the newer state, `forgeAndAppend` retries after `min-micro-block-age`, when those microblocks are eligible. Before this, nothing retried until the next microblock or block, and a quiet UTX left the node without a block (the old code did the same in real time; TestScheduler stepping hid it). Giving up on the tip itself stays final until the next `scheduleMining`, so a permanently ineligible account does not spin. Pinned by `MultipleAccountsMinerWithFinalitySuite` ("it tries again after new microblock of Account 1") and by the no-retry checks in `MinerWithFinalitySuite` ("Mining doesn't work").
- The microblock the previous sequence was building is dropped quietly: our key block's append cancels `microBlockAttempt` before that microblock is built or its append runs, and its transactions are mined by the new sequence. Pinned by `LastMicroBlockSuite` ("stale microblock of the previous sequence is dropped quietly ...").

Driving `MinerImpl` with `TestScheduler`: a key-block attempt now completes in a single `appenderScheduler.tickNext` (sleep, forge, append), and the miner scheduler only runs microblock mining.
