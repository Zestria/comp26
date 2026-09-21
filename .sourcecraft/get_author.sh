set -ex

curl --header "Authorization: Bearer $SOURCECRAFT_TOKEN" --url "https://api.sourcecraft.tech/pulls/id:$SOURCECRAFT_PR_ID"
