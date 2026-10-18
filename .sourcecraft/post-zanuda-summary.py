#!/usr/bin/env python3

"""
parses .rdjsonl linter report and posts comments to pull request with linter's remarks as one summary comment
uses ZANUDA_COMMENT_MARKER to locate old comment
uses fingerprints wrapped into ZANUDA_STATE_MARKER and ZANUDA_STATE_END to define if each remark is new/fixed/unchanged (compared to the previous run)
if there is no old comment or there are no changes does not adds info about changes to summary
"""

import json
import sys
import os
import hashlib
import urllib.request
from collections import Counter


TOKEN             = os.environ["SOURCECRAFT_TOKEN"]
PR_ID             = os.environ["SOURCECRAFT_PR_ID"]
DUNE_PROJECT_ROOT = os.environ["DUNE_PROJECT_ROOT"]

BASE                  = "https://api.sourcecraft.tech"
ZANUDA_COMMENT_MARKER = "<!-- zanuda-summary -->"
ZANUDA_STATE_MARKER   = "<!-- zanuda-state "
ZANUDA_STATE_END      = " -->"

def api(method, url, payload=None):
    req = urllib.request.Request(
        url,
        data=json.dumps(payload).encode() if payload else None,
        headers={"Authorization": f"Bearer {TOKEN}", "Content-Type": "application/json"},
        method=method,
    )
    resp = urllib.request.urlopen(req).read()
    return json.loads(resp) if resp else None

def fetch_comments():
    comments     = api("GET", f"{BASE}/pulls/id:{PR_ID}/comments")
    all_comments = comments["pull_request_comments"]
    zanuda       = [c for c in all_comments if ZANUDA_COMMENT_MARKER in c["body"]]
    dbg(f"all comments: {len(all_comments)}")
    dbg(f"zanuda summary comments: {len(zanuda)}", "\n".join(
        f"[{i}] id={c['id']}" for i, c in enumerate(zanuda)
    ))
    return zanuda

def delete_comments(comments):
    for c in comments:
        dbg(f"deleting comment id={c['id']}")
        try:
            api("DELETE", f"{BASE}/pull_comments/id:{c['id']}")
        except Exception as e:
            dbg(f"warning: failed to delete id={c['id']}", str(e))

def post_comment(body):
    dbg("posting new comment", body)
    api("POST", f"{BASE}/pulls/id:{PR_ID}/comments", {"body": body, "publish": True})

# ---------- logging ----------

def dbg(title, text=""):
    print(f"\n┌─ {title}")
    if text:
        print("│")
        for line in str(text).splitlines():
            print(f"│  {line}")
        print("│")
    print(f"└{'─' * (len(title) + 2)}")

# ---------- state ----------

def fingerprint(lint):
    key = (
        f"{lint['location']['path']}"
        f":{lint['location']['range']['start']['line']}"
        f":{lint['message']}"
    )
    return hashlib.sha1(key.encode()).hexdigest()[:12]

def extract_state(comment_body):
    start = comment_body.find(ZANUDA_STATE_MARKER)
    if start == -1:
        return None
    start += len(ZANUDA_STATE_MARKER)
    end = comment_body.find(ZANUDA_STATE_END, start)
    if end == -1:
        return None
    try:
        result = json.loads(comment_body[start:end])
        return result if isinstance(result, dict) else None
    except (json.JSONDecodeError, ValueError):
        return None

def encode_state(fingerprints):
    blob = json.dumps({"fingerprints": sorted(fingerprints)}, separators=(",", ":"))
    return f"{ZANUDA_STATE_MARKER}{blob}{ZANUDA_STATE_END}"

def load_prev_fingerprints(zanuda_comments):
    for c in reversed(zanuda_comments):
        state = extract_state(c["body"])
        if state is None:
            dbg(f"comment id={c['id']}: no valid state blob, skipping")
            continue
        fps = state.get("fingerprints")
        if not isinstance(fps, list):
            dbg(f"comment id={c['id']}: fingerprints missing or wrong type, skipping")
            continue
        dbg(f"comment id={c['id']}: state extracted", json.dumps(state, indent=2))
        return fps
    dbg("previous state: none (first run or no valid state found)")
    return None

# ---------- markdown ----------

def parse_lints(rdjsonl_path):
    lints = []
    with open(rdjsonl_path, "r", encoding="utf-8") as f:
        for raw in f:
            raw = raw.strip()
            if not raw:
                continue
            try:
                lint = json.loads(raw)
                if DUNE_PROJECT_ROOT:
                    lint["location"]["path"] = os.path.join(
                        DUNE_PROJECT_ROOT, lint["location"]["path"]
                    )
                lints.append(lint)
            except json.JSONDecodeError:
                dbg("parse warning: skipped malformed line", raw)
    return lints

def collect_rule_info(lints):
    rule_info = {}
    for lint in lints:
        code = lint.get("code") or {}
        name = code.get("value") or None
        url  = code.get("url")  or None
        key  = name or "\x00"
        if key not in rule_info:
            rule_info[key] = {"name": name, "url": url, "count": 0}
        rule_info[key]["count"] += 1
    return rule_info

def stat_table(total, num_files, num_rules, current_fps, prev_fingerprints):
    diff_row = ""
    if prev_fingerprints is not None:
        prev_fps        = set(prev_fingerprints)
        new_count       = len(current_fps - prev_fps)
        fixed_count     = len(prev_fps - current_fps)
        unchanged_count = len(current_fps & prev_fps)
        if new_count + fixed_count > 0:
            diff_row = (
                "<tr>\n"
                f'  <td align="center"><b>{new_count}</b><br><sub>new</sub></td>\n'
                f'  <td align="center"><b>{fixed_count}</b><br><sub>fixed</sub></td>\n'
                f'  <td align="center"><b>{unchanged_count}</b><br><sub>unchanged</sub></td>\n'
                "</tr>\n"
            )
    return (
        "<table>\n"
        "<tr>\n"
        f'  <td align="center" width="200"><b>{total}</b><br><sub>total issues</sub></td>\n'
        f'  <td align="center" width="200"><b>{num_files}</b><br><sub>files affected</sub></td>\n'
        f'  <td align="center" width="200"><b>{num_rules}</b><br><sub>unique rules</sub></td>\n'
        "</tr>\n"
        + diff_row +
        "</table>"
    )

def severity_table(sev_counter):
    severity_order = {"ERROR": 0, "WARNING": 1, "INFO": 2}
    rows = ["#### By severity", "", "| Severity | Count |", "|:---------|------:|"]
    for sev, count in sorted(sev_counter.items(), key=lambda x: (severity_order.get(x[0], 99), x[0])):
        rows.append(f"| `{sev}` | {count} |")
    return "\n".join(rows)

def rules_table(rule_info):
    rows = ["#### By rule", "", "| Rule | Docs | Count |", "|:-----|:----:|------:|"]
    for key, info in sorted(rule_info.items(), key=lambda x: -x[1]["count"]):
        if info["name"]:
            rule_cell = f"`{info['name']}`"
            docs_cell = f"[docs]({info['url']})" if info["url"] else "—"
        else:
            rule_cell = "*(no rule)*"
            docs_cell = "—"
        rows.append(f"| {rule_cell} | {docs_cell} | {info['count']} |")
    return "\n".join(rows)

def files_table(file_counter):
    rows = ["#### By file", "", "| File | Issues |", "|:-----|-------:|"]
    for path, count in file_counter.most_common():
        rows.append(f"| `{path}` | {count} |")
    return "\n".join(rows)

# ---------- summary ----------

def build_summary_body(rdjsonl_path, prev_fingerprints=None):
    lints = parse_lints(rdjsonl_path)
    parts = [ZANUDA_COMMENT_MARKER]

    if not lints:
        parts += ["### Zanuda found no issues", "", encode_state(set())]
        return "\n".join(parts)

    current_fps  = {fingerprint(l) for l in lints}
    file_counter = Counter(lint["location"]["path"] for lint in lints)
    sev_counter  = Counter(lint.get("severity", "INFO") for lint in lints)
    rule_info    = collect_rule_info(lints)

    parts += [
        "### Zanuda analysis", "",
        stat_table(len(lints), len(file_counter), len(rule_info), current_fps, prev_fingerprints), "",
        severity_table(sev_counter), "",
        rules_table(rule_info), "",
        files_table(file_counter), "",
        "*Inline comments with details are posted on each affected line.*", "",
        encode_state(current_fps),
    ]
    return "\n".join(parts)

# ---------- main ----------

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: post-zanuda-summary.py <rdjsonl_file>", file=sys.stderr)
        sys.exit(1)

    zanuda            = fetch_comments()
    prev_fingerprints = load_prev_fingerprints(zanuda)
    body              = build_summary_body(sys.argv[1], prev_fingerprints)

    delete_comments(zanuda)
    post_comment(body)
