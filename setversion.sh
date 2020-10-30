#!/bin/bash
version_regex='v([0-9]+)\.([0-9]+)\.?([0-9]*)-([0-9]+)-g([0-9|a-z]+)'
version_msg_regex='([0-9]+)\.([0-9]+)\.?([0-9]*)'
git_string=$(git describe --tags --long)

if [[ $1 =~ $version_msg_regex ]]; then
  major_version="${BASH_REMATCH[1]}"
  minor_version="${BASH_REMATCH[2]}"
  patch_version="${BASH_REMATCH[3]}"
  version_string=$major_version.$minor_version.$patch_version
elif [[ $git_string =~ $version_regex ]]; then
  major_version="${BASH_REMATCH[1]}"
  minor_version="${BASH_REMATCH[2]}"
  patch_version="${BASH_REMATCH[3]}"
  commits="${BASH_REMATCH[4]}"
  postfix="-SNAPSHOT"

  if [[ $1 == "minor" ]]; then
    minor_version=$((minor_version + 1))
    patch_version=0
    shift
  elif [[ $1 == "major" ]]; then
    major_version=$((major_version + 1))
    minor_version=0
    patch_version=0
    shift
  elif [[ $1 == "patch" || $1 == "bugfix" || $1 == "fix" ]]; then
    patch_version=$((patch_version + 1))
    shift
  elif [[ $1 == "reset" || ($commits -eq 0 && $1 != "snapshot") ]]; then
    postfix=""
  else
    patch_version=$((patch_version + 1))
  fi

  if [[ $1 == "release" ]]; then
    postfix=""
    shift
  elif [[ $1 != "" && $1 != "snapshot" ]]; then
    postfix="-$1-SNAPSHOT"
  fi
  version_string=$major_version.$minor_version.$patch_version$postfix
else
  echo "Error: git describe did not output a valid version string. Unable to update version" >&2
  exit 1
fi

mvn_ver_regex="<version>([0-9]+)\.([0-9]+)\.?([0-9]*)(-SNAPSHOT)?<\/version>"
orig_ver_regex="([0-9]+)\.([0-9]+)\.?([0-9]*)(-SNAPSHOT)?"
if [ "$(uname)" == "Darwin" ]; then
  SED="gsed -i -E -e" # brew install gnu-sed
else
  SED="sed -i -E -e"
fi

# Just outputs versionw
echo $version_string
