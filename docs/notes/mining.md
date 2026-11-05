---
purpose: Implementation notes for block and microblock mining (Miner.scala, MicroBlockMinerImpl), including known races
---

# Mining

## Key block forged on a stale parent

The miner can orphan its own newest microblock, even when it is the only miner and `min-micro-block-age = 0`. Key-block forging (`MinerImpl.forgeBlock`) and microblock mining (`MicroBlockMinerImpl.generateOneMicroBlockTask`) are separate tasks on `minerScheduler`, and nothing orders them. `forgeBlock` takes its parent from `bestLastBlockInfo` when it runs; if the microblock task has already packed its transactions but not yet built and appended the microblock, the key block references the tip before that microblock. Both blocks then go through the appender: the microblock first, the key block about 20ms later, and appending the key block discards the microblock (`Discarded microblocks: List(...)` in the node log). Its transactions return to the UTX priority pool and are mined again in the next microblock, so the final state is right, but for about 200ms they are gone from the state. `min-micro-block-age` does not help: it only skips microblocks that already exist and are too young.

Seen in node-it (`IssueReissueBurnAssetSuite`, run 41668443292): the test saw a funding transfer in the liquid block through `/transactions/info`, the next key block orphaned that microblock, and the set-script transaction sent right after was rejected with `negative waves balance`. Suites that wait for the next block after every transaction never notice; the window is only reachable by a client that reacts to the liquid block, which is what real clients do.

Do not fix it by cancelling `microBlockAttempt` before forging. A key-block attempt can give up (`PermanentFailure`, a failed append in `appendTask`) without rescheduling anything, and today the node recovers only because each microblock append calls `scheduleMining(cancelMicroBlockMining = false)` (`BlockchainUpdaterImpl.processMicroBlock`), which schedules a new key-block attempt. With microblocks cancelled as well, nothing would schedule mining again: a single-miner network stalls, and on mainnet the current generator stops filling the liquid block until another miner's block arrives.

### Option: choose the parent on the appender thread (needs further investigation)

The appender is a single thread (`Application.scala`, `singleThread("appender")`) and microblock appends are queued on it in order. If the key block took its parent, and was built, inside the appender task rather than on `minerScheduler`, it would see every microblock queued ahead of it. A microblock packed but not yet queued would reach the appender after the key block, be rejected because its reference is no longer the tip, and lose nothing: transactions leave the UTX pool only when a microblock is appended. Microblock mining keeps running, so the rescheduling path above stays intact. A cheaper variant keeps building on `minerScheduler` and only re-checks in the appender task that the chosen parent is still the liquid tip, rebuilding the block if not.

Not done yet, because it changes mining on mainnet and these questions are open:

- Cost on the appender thread: under NG a key block has no transactions, but it still computes consensus data, the light-node state hash (`packTransactionsForKeyBlock`) and the signature, all of which would delay block and microblock appends from peers.
- Whether the rejected microblock is handled quietly: the microblock miner treats an append error as `MicroBlockAppendError`; check that this does not stop or back off the microblock sequence for the new key block.
- Interaction with block challenging (`BlockChallenger`), finalization voting and endorsements, which also read the parent when the block is built.
- `BlockFromFuture` retries and `generateBlockTask`'s `waitBlockAppendedTask`, which assume forging happens on `minerScheduler`.
- A node-tests case that reproduces the orphaned microblock deterministically (pack a microblock, forge a key block, then append both) is needed before and after the change.
