#!/usr/bin/env python3
"""Add native lazy loading to every content image (and iframe) in _site.

Images dominate the deployed payload (hundreds of PNG figures across the
site). Browser-native ``loading="lazy"`` defers offscreen fetches until the
user scrolls near them, cutting initial page load dramatically, and
``decoding="async"`` keeps rendering responsive while images decode.
No JavaScript, no layout shift (Quarto emits width/height on its figures).

Structural images (sidebar logo etc.) are excluded: any <img> inside
<header>, <nav>, or whose src looks like the site logo keeps default
eager loading because it is above the fold on every page.
"""
import os
import re
import sys
from html.parser import HTMLParser

SITE = sys.argv[1] if len(sys.argv) > 1 else "_site"
LOGO_RE = re.compile(r"Bizard_logo|logo[-_]?footer", re.I)


class LazyInjector(HTMLParser):
    """Track header/nav depth; record char positions of tags to patch."""

    def __init__(self):
        super().__init__(convert_charrefs=False)
        self.skip_depth = 0
        self.insert_at = []  # (char_offset, attribute_text)

    def handle_starttag(self, tag, attrs):
        if tag in ("header", "nav"):
            self.skip_depth += 1
        attr_map = dict(attrs)
        if tag == "img" and self.skip_depth == 0:
            if "loading" not in attr_map and not LOGO_RE.search(attr_map.get("src", "")):
                self._record(' decoding="async" loading="lazy"')
        elif tag == "iframe" and self.skip_depth == 0:
            if "loading" not in attr_map:
                self._record(' loading="lazy"')

    def _record(self, text):
        line, col = self.getpos()
        self.insert_at.append((self._line_offsets[line - 1] + col, text))

    def handle_endtag(self, tag):
        if tag in ("header", "nav") and self.skip_depth > 0:
            self.skip_depth -= 1

    def prepare(self, html):
        # Byte offset of the start of each line, for getpos() translation.
        self._line_offsets = [0]
        for idx, ch in enumerate(html):
            if ch == "\n":
                self._line_offsets.append(idx + 1)


def process(path):
    with open(path, encoding="utf-8") as fh:
        html = fh.read()
    parser = LazyInjector()
    parser.prepare(html)
    try:
        parser.feed(html)
        parser.close()
    except Exception:
        return 0  # never corrupt a page we cannot parse
    if not parser.insert_at:
        return 0
    for offset, text in sorted(parser.insert_at, reverse=True):
        end = html.index(">", offset)  # close of this start tag
        if html[end - 1] == "/":  # keep XHTML-style self-closing valid
            html = html[:end - 1] + text + html[end - 1:]
        else:
            html = html[:end] + text + html[end:]
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(html)
    return len(parser.insert_at)


total = 0
pages = 0
for dirpath, _dirnames, filenames in os.walk(SITE):
    for fname in filenames:
        if fname.endswith(".html"):
            n = process(os.path.join(dirpath, fname))
            if n:
                pages += 1
                total += n

print(f"Added lazy loading to {total} image(s)/iframe(s) across {pages} page(s)")
