"""Run: python3 -B -m unittest discover -s .sourcecraft -p 'test_stats_*.py'."""

import importlib.util
import json
import subprocess
import tempfile
import unittest
from pathlib import Path


spec = importlib.util.spec_from_file_location(
    "stats_authors", Path(__file__).with_name("stats-authors.py")
)
stats = importlib.util.module_from_spec(spec)
spec.loader.exec_module(stats)


class StatsAuthorsTests(unittest.TestCase):
    def test_missing_email_keeps_original_report(self):
        self.assertIsNone(stats.author_pattern(["alice", "bob"], {
            "alice": ["alice@example.org"],
        }))
        self.assertIsNone(stats.author_pattern(["unconfigured_team"], {}))
        self.assertIsNone(stats.author_pattern([], {}))

    def test_malformed_email_list_is_an_error(self):
        for addresses in ("alice@example.org", ["invalid"], ["a@example.org\n"]):
            with self.assertRaises(ValueError):
                stats.author_pattern(["alice"], {"alice": addresses})

    def test_git_filters_exact_emails_and_supports_multiple_addresses(self):
        pattern = stats.author_pattern(["alice"], {
            "alice": ["alice@example.org", "alice+work@example.org"],
            "other_team": ["other@example.org"],
        })
        with tempfile.TemporaryDirectory(prefix="stats-authors-") as directory:
            def git(*args):
                return subprocess.check_output(
                    ["git", "-C", directory, *args], text=True
                )

            git("init", "-q")
            for email in ("alice@example.org", "ALICE+work@example.org",
                          "other@example.org", "fake-alice@example.org",
                          "alice@exampleXorg"):
                git("-c", "user.name=Test", "-c", f"user.email={email}",
                    "-c", "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null",
                    "commit", "--quiet", "--allow-empty", "-m", email)
            selected = git("log", "--format=%ae", "--regexp-ignore-case",
                           "--extended-regexp", f"--author={pattern}").splitlines()
            self.assertCountEqual(selected, ["alice@example.org", "ALICE+work@example.org"])

    def test_declared_team_configuration(self):
        identities = json.loads(Path(__file__).with_name("git-author-emails.json").read_text())
        self.assertIsNotNone(stats.author_pattern(
            ["bogbanrvd-gmail-com", "shamashaehmitov", "garry2311"], identities
        ))


if __name__ == "__main__":
    unittest.main()
