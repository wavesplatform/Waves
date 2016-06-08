
**Please, put your own walletSeed into waves-testnet.json. It must be random Base58 string.**

**A part of HTTP API must be closed by apiKeyHash in config. Use /utils/hash/secure http api to get hash for your api key.**

[How to configure Waves node](https://github.com/wavesplatform/Waves/wiki/How-to-configure-Waves-node)

# You need to install Java SDK
## Ubuntu

Instructions here https://github.com/wavesplatform/Waves (SBT not necessary)


# Install Waves

`sudo dpkg -i <waves .deb package>`

# How to run

`waves waves-testnet.json`

After that open [http://127.0.0.1:6869](http://127.0.0.1:6869) in your browser. REST Api must be available.

# Create new Address

Use [http://127.0.0.1:6869/#!/addresses/create](http://127.0.0.1:6869/#!/addresses/create) and press 'Try it out'

# Get coins for Testnet

Post your address to slack and ask somebody

# Send new Payment

Use [http://127.0.0.1:6869/#!/payment/payment](http://127.0.0.1:6869/#!/payment/payment), fill the body with
correct sender address and recipient address

# Check your balance

Use [http://127.0.0.1:6869/#!/addresses/balance](http://127.0.0.1:6869/#!/addresses/balance)

