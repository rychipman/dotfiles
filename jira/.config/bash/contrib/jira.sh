# jira-cli configuration for MongoDB Jira
# https://github.com/ankitpokhrel/jira-cli

export JIRA_AUTH_TYPE=bearer

# Fetch token from 1Password - use from direnv .envrc files:
#   export JIRA_API_TOKEN="$(jira-token)"
jira-token() {
    op read 'op://MongoDB/rf4im2yhazpcompidzgfvluwtm/credential'
}

# Wrapper fetches token from 1Password on demand
jira() {
    JIRA_API_TOKEN="$(jira-token)" command jira "$@"
}
