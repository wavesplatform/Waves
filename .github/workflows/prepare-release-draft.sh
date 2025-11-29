#!/bin/bash
set -e

sbt -Dproject.version=${RELEASE_VERSION} --batch "buildReleaseArtifacts $RELEASE_NETWORKS"

assets=$(find . \( -name '*.deb' -o -name '*all*.jar' -o -name '*.tgz' \) -print)

if [ -n "$GITHUB_OUTPUT" ]; then
  echo "subject-path<<EOF" >> $GITHUB_OUTPUT
  echo "$assets" >> $GITHUB_OUTPUT
  echo "EOF" >> $GITHUB_OUTPUT
fi

lowercase_networks=$( echo $RELEASE_NETWORKS| tr '[:upper:]' '[:lower:]')
release_heading=""
prerelease="true"
if [[ "$lowercase_networks" == *"mainnet"* ]]; then
  release_heading="Mainnet"
  prerelease="false"
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

EOF
)

release_body=$(cat << EOF
{
  "tag_name":"v${RELEASE_VERSION}",
  "target_commitish":"${RELEASE_BRANCH}",
  "name":"Version ${RELEASE_VERSION} ($release_heading)",
  "draft":true,
  "prerelease":${prerelease},
  "generate_release_notes":false
}
EOF
)

curl_headers=(
  -L
  --retry 5
  --retry-all-errors
  -H 'Accept: application/vnd.github+json'
  -H 'X-GitHub-Api-Version: 2022-11-28'
  -H "Authorization: Bearer ${GITHUB_TOKEN}"
)

release_resp=$(echo $release_body |\
  jq --arg text "$release_text" '.body = $text' |\
  curl "${curl_headers[@]}" -H 'Content-type: application/json' https://api.github.com/repos/${GITHUB_REPO}/releases -d @-)

release_id=$(echo $release_resp | jq .id)

if [[ "$release_id" == "null" ]] ; then
  echo "::error title=Error::Release draft NOT created"
  exit 1
else
  echo "::notice::Created release draft with id ${release_id}"
fi

for asset in $assets; do
  curl "${curl_headers[@]}" -H "Content-Type: application/octet-stream" \
    "https://uploads.github.com/repos/${GITHUB_REPO}/releases/${release_id}/assets?name=$(basename -- "$asset")" \
    --data-binary "@${asset}"
  echo "::notice::Uploaded ${asset}"
done
