#!/bin/bash

cd /tmp
source ./bitcoin-bash-tools/bitcoin.sh
export NEWSEED=`newBitcoinKey | grep address | cut -c 27- | head -1`
sed -i.bak "s/mysecretpass/${SEED:-$NEWSEED}/g" waves-testnet-0.1.3/waves-testnet.json
cat waves-testnet-0.1.3/waves-testnet.json
waves -v /tmp/waves-testnet-0.1.3/waves-testnet.json
