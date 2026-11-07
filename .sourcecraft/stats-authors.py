#!/usr/bin/env python3
"""Read team usernames as JSON; output a Git --author ERE, or no filter."""

import json
import re
import sys
from pathlib import Path


def author_pattern(members, identities):
    if not isinstance(members, list) or not all(isinstance(m, str) for m in members):
        raise ValueError("expected a list of team usernames")
    if not isinstance(identities, dict):
        raise ValueError("expected an object mapping usernames to email lists")
    missing = [member for member in members if not identities.get(member)]
    if not members or missing:
        return None
    emails = set()
    for member in members:
        addresses = identities[member]
        if not isinstance(addresses, list) or not all(
            isinstance(email, str) and "@" in email
            and not any(char.isspace() or char in "<>" for char in email)
            for email in addresses
        ):
            raise ValueError(f"invalid Git email list for {member}")
        emails.update(addresses)
    # Git author identities contain <email>; delimiters prevent partial matches.
    return "<(" + "|".join(re.escape(email) for email in sorted(emails)) + ")>"


def main():
    path = Path(__file__).with_name("git-author-emails.json")
    identities = json.loads(path.read_text()) if path.exists() else {}
    pattern = author_pattern(json.load(sys.stdin), identities)
    if pattern is None:
        print("stats: Git emails are not configured for the whole team; "
              "using the original unfiltered report", file=sys.stderr)
    else:
        print(pattern)


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError) as error:
        sys.exit(f"stats: {error}")
