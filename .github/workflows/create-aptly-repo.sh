#!/usr/bin/env bash
set -e

all_deb_packages=$(curl -s -H "Authorization: Bearer $GITHUB_TOKEN" \
  https://api.github.com/repos/wavesplatform/waves/releases |\
  jq --raw-output '.[].assets[]|select(.name | test("\\.deb$"))|select(now - (.created_at|fromdate) < 31536000)|[.name,.browser_download_url,.digest]|@csv' |\
  tr -d \")

mkdir -p packages

for deb_package in ${all_deb_packages} ; do
  IFS="," read -r deb_file_name deb_url digest <<< "$deb_package"
  wget -v -O "packages/$deb_file_name" $deb_url
  if [[ "$digest" != "null" ]] ; then
    echo "$(echo $digest | cut -d: -f2) packages/$deb_file_name" | sha256sum --check --status - && echo CHECKSUM OK
  fi
done

echo $GPG_PRIVATE_KEY > private-key.asc
gpg --batch --import-options import-show --import private-key.asc
rm -f private-key.asc

aptly repo create main
aptly repo add main packages/
aptly publish repo -architectures=all -distribution=jammy -gpg-key=$GPG_KEY_ID -passphrase=$GPG_PASSPHRASE main

gpg --batch --yes --delete-secret-and-public-key $GPG_KEY_ID

cat >> .aptly/public/index.tml <<EOF
<html>
<head>
<title>Waves Platform APT Repository</title>
</head>
<body>
<h1>Waves Platform APT Repository</h1>
</body>
</html>
EOF
