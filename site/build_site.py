#!/usr/bin/env python3
"""Build the MACpaca website into _site/.

Reads site/parks.yml, scans quartos/ for rendered Quarto reports (.html),
and writes _site/index.html plus copies of the reports, park photos and
workflow infographic. Only reports that exist become links; reports listed
under `planned:` that don't exist yet are shown as "In preparation".

Run locally from the repo root with:  python site/build_site.py
Requires: pyyaml, pillow
"""
from __future__ import annotations

import datetime as dt
import html
import re
import shutil
import sys
from pathlib import Path
from urllib.parse import quote

import yaml
from PIL import Image

ROOT = Path(__file__).resolve().parent.parent
SITE_SRC = ROOT / "site"
QUARTOS = ROOT / "quartos"
OUT = ROOT / "_site"

REPORT_RE = re.compile(r"Appendix\s+([A-Z]\d*)-q-(.+)$", re.IGNORECASE)
OTHER_NETWORK = {"id": "other", "name": "Other sites"}


def esc(text) -> str:
    return html.escape(str(text), quote=True)


def url_path(*parts: str) -> str:
    return "/".join(quote(p) for p in parts)


def prettify(name: str) -> str:
    words = re.sub(r"[-_]+", " ", name).strip()
    return words[:1].upper() + words[1:]


def code_sort_key(report: dict):
    m = re.match(r"([A-Z]*)(\d*)", report["code"].upper())
    letters, num = (m.group(1), m.group(2)) if m else ("", "")
    return (letters == "", letters, int(num) if num else 0, report["title"])


def find_reports(folder: str, suffix: str, titles: dict) -> list[dict]:
    """All .html files directly inside quartos/<folder> (sub-folders are ignored)."""
    path = QUARTOS / folder
    if not path.is_dir():
        return []
    reports = []
    for f in sorted(path.glob("*.html")):
        stem = f.stem
        m = REPORT_RE.search(stem)
        if stem in titles:
            code, title = (m.group(1).upper() if m else ""), titles[stem]
        elif m:
            code, title = m.group(1).upper(), m.group(2).strip()
            title = title[:1].upper() + title[1:]
        else:
            code, title = "", prettify(stem)
        if suffix:
            title = f"{title} {suffix}"
        reports.append({"code": code, "title": title, "href": url_path("quartos", folder, f.name), "ready": True})
    return reports


def park_folders(park: dict) -> list[tuple[str, str]]:
    out = []
    for item in park.get("folders", []):
        if isinstance(item, dict):
            out.append((item["path"], item.get("suffix", "")))
        else:
            out.append((str(item), ""))
    return out


def make_thumbnail(src: Path, dest: Path, width: int = 900) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    with Image.open(src) as im:
        im = im.convert("RGB")
        if im.width > width:
            im = im.resize((width, round(im.height * width / im.width)), Image.LANCZOS)
        im.save(dest, "JPEG", quality=80, optimize=True)


def render_card(park: dict, cfg: dict) -> str:
    title = esc(park["name"])
    sub = f'<p class="card-sub">{esc(park["subtitle"])}</p>' if park.get("subtitle") else ""

    img = park.get("image") or {}
    if park.get("thumb"):
        credit_text = f'Photo: {img["credit"]}' if img.get("credit") else "Image: Australian Marine Parks"
        caption = " · ".join(esc(x) for x in [img.get("caption"), credit_text] if x)
        credit = f'<a href="{esc(img["source"])}">{caption}</a>' if img.get("source") else caption
        alt = esc(img.get("caption") or park["name"])
        media = (f'<figure class="card-media"><img src="{park["thumb"]}" alt="{alt}" loading="lazy">'
                 f'<figcaption>{credit}</figcaption></figure>')
    else:
        media = '<div class="card-media card-media--empty" aria-hidden="true"></div>'

    syn = ""
    if park.get("syntheses"):
        chips = "".join(
            f'<a class="chip" href="{esc(cfg["synthesis_url"])}{esc(s["id"])}">{esc(s["label"])}</a>'
            for s in park["syntheses"])
        syn = f'<div class="block"><h4 class="label">Data syntheses</h4><div class="chips">{chips}</div></div>'

    rows = []
    for r in park["reports"]:
        code = f'<span class="code">{esc(r["code"])}</span>' if r["code"] else ""
        if r["ready"]:
            rows.append(f'<li><a href="{r["href"]}">{code}{esc(r["title"])}</a></li>')
        else:
            rows.append(f'<li class="todo"><span>{code}{esc(r["title"])}</span><span class="pill">In preparation</span></li>')
    reports = (f'<div class="block"><h4 class="label">Reports</h4><ul class="reports">{"".join(rows)}</ul></div>'
               if rows else "")

    code_link = ""
    if park.get("code"):
        href = f'https://github.com/{cfg["repo"]}/tree/main/{url_path(*park["code"].split("/"))}'
        code_link = (f'<a class="code-link" href="{href}"><svg aria-hidden="true" width="16" height="16" viewBox="0 0 24 24" '
                     'fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">'
                     '<polyline points="16 18 22 12 16 6"></polyline><polyline points="8 6 2 12 8 18"></polyline></svg>'
                     'Data analysis code</a>')

    return (f'<article class="card">{media}<div class="card-body"><div><h3 class="card-title">{title}</h3>{sub}</div>'
            f'{syn}{reports}{code_link}</div></article>')


def main() -> int:
    cfg = yaml.safe_load((SITE_SRC / "parks.yml").read_text(encoding="utf-8"))
    hidden = set(cfg.get("hidden_folders") or [])
    networks = list(cfg.get("networks") or [])
    parks = list(cfg.get("parks") or [])

    if OUT.exists():
        shutil.rmtree(OUT)
    OUT.mkdir()

    # Reports, matched to parks by folder
    claimed = set()
    for park in parks:
        found = []
        for folder, suffix in park_folders(park):
            claimed.add(folder)
            found += find_reports(folder, suffix, park.get("titles") or {})
        have = {r["code"] for r in found}
        for p in park.get("planned") or []:
            if str(p["code"]).upper() not in have:
                found.append({"code": str(p["code"]).upper(), "title": p["title"], "href": "", "ready": False})
        park["reports"] = sorted(found, key=code_sort_key)

    # Any new quartos/ folder with reports that isn't in parks.yml gets a basic card
    if QUARTOS.is_dir():
        for d in sorted(p for p in QUARTOS.iterdir() if p.is_dir()):
            if d.name in claimed or d.name in hidden:
                continue
            reports = find_reports(d.name, "", {})
            if reports:
                print(f"note: quartos/{d.name} is not in parks.yml - added as a new card", file=sys.stderr)
                parks.append({"name": prettify(d.name), "network": "other", "folders": [d.name],
                              "code": f"r/{d.name}" if (ROOT / "r" / d.name).is_dir() else "",
                              "reports": sorted(reports, key=code_sort_key)})

    # Park photos -> small copies for the cards
    for park in parks:
        img = park.get("image") or {}
        src = ROOT / img["file"] if img.get("file") else None
        if src and src.is_file():
            thumb = Path("assets/parks") / (src.stem + ".jpg")
            if not (OUT / thumb).exists():
                make_thumbnail(src, OUT / thumb)
            park["thumb"] = thumb.as_posix()
        elif src:
            print(f"note: photo {img['file']} not found - showing a placeholder for {park['name']}", file=sys.stderr)

    # Group by network, in the order listed in parks.yml
    if any(p.get("network") == "other" for p in parks):
        networks.append(OTHER_NETWORK)
    known = {n["id"] for n in networks}
    for p in parks:
        if p.get("network") not in known:
            p["network"] = "other"
            if OTHER_NETWORK not in networks:
                networks.append(OTHER_NETWORK)
                known.add("other")

    sections, filters = [], ['<button type="button" class="filter" data-net="all" aria-pressed="true">All networks</button>']
    for n in networks:
        members = [p for p in parks if p.get("network") == n["id"]]
        if not members:
            continue
        filters.append(f'<button type="button" class="filter" data-net="{esc(n["id"])}" aria-pressed="false">'
                       f'{esc(n["name"].replace(" Network", ""))}</button>')
        cards = "".join(render_card(p, cfg) for p in members)
        sections.append(f'<section class="network" data-net="{esc(n["id"])}"><h3 class="network-title">{esc(n["name"])}</h3>'
                        f'<div class="grid">{cards}</div></section>')

    report_count = sum(1 for p in parks for r in p["reports"] if r["ready"])

    # Workflow infographic: web-sized copy for the page, original for "full size"
    workflow = ROOT / "workflow.png"
    if workflow.is_file():
        shutil.copy2(workflow, OUT / "workflow.png")
        with Image.open(workflow) as im:
            im = im.convert("RGB")
            im.thumbnail((2000, 2000), Image.LANCZOS)
            (OUT / "assets").mkdir(exist_ok=True)
            im.save(OUT / "assets/workflow.jpg", "JPEG", quality=85, optimize=True)

    page = (SITE_SRC / "template.html").read_text(encoding="utf-8")
    for key, value in {
        "FIELD_MANUALS_URL": esc(cfg["field_manuals_url"]),
        "REPO_URL": f'https://github.com/{cfg["repo"]}',
        "PARK_COUNT": str(len(parks)),
        "REPORT_COUNT": str(report_count),
        "FILTERS": "".join(filters),
        "SECTIONS": "".join(sections),
        "BUILD_DATE": dt.date.today().strftime("%-d %B %Y") if sys.platform != "win32" else dt.date.today().strftime("%d %B %Y"),
    }.items():
        page = page.replace("{{" + key + "}}", value)
    (OUT / "index.html").write_text(page, encoding="utf-8")
    shutil.copy2(SITE_SRC / "style.css", OUT / "style.css")

    # Reports (with their _files/ and figures/ folders) keep the same URLs as before
    if QUARTOS.is_dir():
        shutil.copytree(QUARTOS, OUT / "quartos", ignore=shutil.ignore_patterns("*.qmd", "*.R", "*.rds", ".gitignore"))
    (OUT / ".nojekyll").touch()

    print(f"Built _site/: {len(parks)} parks, {report_count} reports linked")
    return 0


if __name__ == "__main__":
    sys.exit(main())
