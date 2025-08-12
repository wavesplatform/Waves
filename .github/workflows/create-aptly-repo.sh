#!/usr/bin/env bash
set -e

echo "$GPG_PRIVATE_KEY" > private-key.asc
gpg --batch --import-options import-show --import private-key.asc
rm -f private-key.asc

all_deb_packages=$(curl -s -H "Authorization: Bearer $GITHUB_TOKEN" \
  https://api.github.com/repos/wavesplatform/waves/releases |\
  jq --raw-output '.[].assets[]|select(.name | test("\\.deb$"))|select(now - (.created_at|fromdate) < 1209600)|[.name,.browser_download_url,.digest]|@csv' |\
  tr -d \")

mkdir -p packages

for deb_package in ${all_deb_packages} ; do
  IFS="," read -r deb_file_name deb_url digest <<< "$deb_package"
  wget -nv -O "packages/$deb_file_name" $deb_url
  if [[ "$digest" != "null" ]] ; then
    echo "$(echo $digest | cut -d: -f2) packages/$deb_file_name" | sha256sum --check --status - && echo CHECKSUM OK
  fi
done

aptly repo create main
aptly repo add main packages/
aptly publish repo -batch -architectures="arm64,amd64,all" -distribution=stable -gpg-key=$GPG_KEY_ID -passphrase=$GPG_PASSPHRASE main

gpg --armor --export $GPG_KEY_ID > /home/runner/.aptly/public/pubkey.txt

rm -rf .gnupg

cat > /home/runner/.aptly/public/index.html <<EOF
<html>
<head>
<title>Waves Platform APT Repository</title>
</head>
<body>
<h1>Waves Platform APT Repository</h1>
To add the repository, run the following:
<pre>
echo "deb [signed-by=/etc/apt/keyrings/wavesplatform.asc] https://wavesplatform.github.io/Waves stable main" | sudo tee /etc/apt/sources.list.d/wavesplatform.list
# For releases older than Debian 12 and Ubuntu 22.04, create the directory first:
sudo mkdir -p /etc/apt/keyrings; sudo chmod 755 /etc/apt/keyrings
sudo wget -O /etc/apt/keyrings/wavesplatform.asc https://wavesplatform.github.io/Waves/pubkey.txt
sudo apt-get update
</pre>
</body>
</html>
EOF
