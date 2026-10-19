#!/usr/bin/env python3
"""
parses .rdjsonl linter report and posts comments to pull request with linter's remarks as inline comments
uses ZANUDA_COMMENT_MARKER to locate old comments and delete them
"""

import json
import sys
import os
import urllib.request

TOKEN             = os.environ["SOURCECRAFT_TOKEN"]
PR_ID             = os.environ["SOURCECRAFT_PR_ID"]
DUNE_PROJECT_ROOT = os.environ["DUNE_PROJECT_ROOT"]
BASE              = "https://api.sourcecraft.tech"

_debug = True

if _debug:
    print(f"[debug] PR_ID: {PR_ID}")
    print(f"[debug] BASE: {BASE}")
    print(f"[debug] DUNE_PROJECT_ROOT: {DUNE_PROJECT_ROOT}")

ZANUDA_COMMENT_MARKER = "<!-- zanuda-inline -->"
rdjsonl_path = sys.argv[1]

def dbg(title, text=""):
    if not _debug:
        return
    print(f"\n┌─ {title}")
    if text:
        print(f"│")
        for line in str(text).splitlines():
            print(f"│  {line}")
        print(f"│")
    print(f"└{'─' * (len(title) + 2)}")
    sys.stdout.flush()

def api(method, url, payload=None):
    req = urllib.request.Request(
        url,
        data=json.dumps(payload).encode() if payload else None,
        headers={"Authorization": f"Bearer {TOKEN}", "Content-Type": "application/json"},
        method=method,
    )
    resp = urllib.request.urlopen(req).read()
    return json.loads(resp) if resp else None

# --- delete old zanuda inline comments ---
comments     = api("GET", f"{BASE}/pulls/id:{PR_ID}/comments")
all_comments = comments["pull_request_comments"]
old          = [c for c in all_comments if ZANUDA_COMMENT_MARKER in c["body"]]
dbg(f"delete: begin — {len(old)} comments to delete")
for i, c in enumerate(old):
    dbg(f"[{i}] deleting id={c['id']}", c['body'])
    rq = f"{BASE}/pull_comments/id:{c['id']}"
    if _debug:
        print(f"[debug] request: DELETE {rq}")
    api("DELETE", rq)
dbg(f"delete: done — deleted {len(old)} comments")

# --- parse lints ---
lints = []
seen  = set()
with open(rdjsonl_path) as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        l   = json.loads(line)

        if l['location']['path'] == "_none_":
            dbg(f"Line skipped, because location is _none_", line)
            continue


        dbg(f"message: {l['message']}")
        dbg(f"location: {l['location']}")

        key = (l["location"]["path"], l["location"]["range"]["start"]["line"], l["message"])
        if key not in seen:
            seen.add(key)
            lints.append(l)

dbg(f"parse: begin — {rdjsonl_path}")
for i, l in enumerate(lints):
    dbg(
        f"[{i}] {l['location']['path']}:{l['location']['range']['start']['line']}",
        l['message']
    )
dbg(f"parse: done — {len(lints)} lints")

# --- post new inline comments ---
dbg(f"post: begin — {len(lints)} comments to post")
for i, lint in enumerate(lints):
    path    = lint["location"]["path"]
    # path    = os.path.join(DUNE_PROJECT_ROOT, lint["location"]["path"])
    line_no = int(lint["location"]["range"]["start"]["line"])
    message = lint["message"].strip()
    code    = lint.get("code", {})

    body = ZANUDA_COMMENT_MARKER + "\n" + message
    # if code.get("url"):
    #     doc_name = code.get('value', 'docs')
    #     body += f"\n\nDocumentation: [{doc_name}]({code['url']})"

    payload = {
        "body": body,
        "publish": True,
        "anchor": {
            "path": path,
            "position": {
                "from": str(line_no),
                "to":   str(line_no),
                "side": "source",
            },
        },
    }
    request = f"{BASE}/pulls/id:{PR_ID}/comments"
    if _debug:
        print(f"[debug] request: POST {request}")
        print(f"[debug] payload:  {payload}")
    dbg(f"[{i}] posting comment → {path}:{line_no}", body)
    api("POST", request, payload)
dbg(f"post: done — posted {len(lints)} comments")
