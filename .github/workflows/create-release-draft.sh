#!/bin/bash
set -e

sbt -Dproject.version=${RELEASE_VERSION} --batch 'buildPackages $RELEASE_NETWORKS'

assets=$(find . \( -path ./docker -o -path ./node/target/universal \) -prune  -o \( -name '*.deb' -o -name '*all*.jar' -o -name '*.tgz' \) -print)
sha256sums=$(sha256sum $assets | sed 's|\..*/||')

lowercase_networks=$( echo $RELEASE_NETWORKS| tr '[:upper:]' '[:lower:]')
release_heading=""
if [[ "$lowercase_networks" == *"mainnet"* ]]; then
  release_heading="Mainnet"
fi

if [[ "$lowercase_networks" == *"testnet"* ]]; then
  if [[ -n $release_heading ]] ; then
    release_heading="$release_heading + "
  fi
  release_heading="${release_heading}Testnet"
fi

if [[ "$lowercase_networks" == *"stagenet"* ]]; then
  if [[ -n $release_heading ]] ; then
    release_heading="$release_heading + "
  fi
  release_heading="${release_heading}Stagenet"
fi

release_text=$(cat << EOF
# In this release

## Update notes

## SHA256 Checksums
\`\`\`
$sha256sums
\`\`\`
EOF
)

release_body=$(cat << EOF
{
  "tag_name":"v${RELEASE_VERSION}",
  "target_commitish":"${RELEASE_BRANCH}",
  "name":"Version ${RELEASE_VERSION} ($release_heading)",
  "draft":true,
  "prerelease":false,
  "generate_release_notes":false
}
EOF
)

release_id=$(echo $release_body |\
  jq --arg text "$release_text" '.body = $text' |\
  curl -L \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    https://api.github.com/repos/${GITHUB_REPO}/releases \
    -d @- |\
 jq .id)

for asset in $assets; do
  curl -L \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer ${GITHUB_TOKEN}" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    -H "Content-Type: application/octet-stream" \
    "https://uploads.github.com/repos/${GITHUB_REPO}/releases/${release_id}/assets?name=$(basename -- "$asset")" \
    --data-binary "@${asset}"
done
