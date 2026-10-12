from __future__ import annotations

import copy
import json
import re
import subprocess
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET


W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
DOC_REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
ET.register_namespace("w", W_NS)


def qn(tag: str) -> str:
    return f"{{{W_NS}}}{tag}"


def rel_qn(tag: str) -> str:
    return f"{{{REL_NS}}}{tag}"


def strip_matching_braces(value: str) -> str:
    value = value.strip()
    while value.startswith("{") and value.endswith("}"):
        value = value[1:-1].strip()
    return value


def inspect_quarto_project(paper_dir: Path) -> dict:
    result = subprocess.run(
        ["quarto", "inspect", str(paper_dir)],
        check=True,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    return json.loads(result.stdout)


def build_author_affiliations(metadata: dict) -> tuple[list[dict], list[str]]:
    ordered_affiliations: list[str] = []
    affiliation_indices: dict[str, int] = {}
    processed_authors: list[dict] = []

    for author in metadata.get("author", []):
        author_aff_indices: list[int] = []
        for affiliation in author.get("affiliations", []) or []:
            aff_text = str(affiliation).strip()
            if not aff_text:
                continue
            if aff_text not in affiliation_indices:
                ordered_affiliations.append(aff_text)
                affiliation_indices[aff_text] = len(ordered_affiliations)
            author_aff_indices.append(affiliation_indices[aff_text])
        processed_authors.append(
            {
                "name": str(author.get("name", "")).strip(),
                "indices": author_aff_indices,
            }
        )

    return processed_authors, ordered_affiliations


def paragraph_style(paragraph: ET.Element) -> str:
    style = paragraph.find("./w:pPr/w:pStyle", {"w": W_NS})
    return style.get(qn("val")) if style is not None else ""


def paragraph_text(paragraph: ET.Element) -> str:
    return "".join(node.text or "" for node in paragraph.findall(".//w:t", {"w": W_NS})).strip()


def run_text(run: ET.Element) -> str:
    return "".join(node.text or "" for node in run.findall(".//w:t", {"w": W_NS}))


def clear_paragraph_runs(paragraph: ET.Element) -> ET.Element:
    existing_ppr = paragraph.find("./w:pPr", {"w": W_NS})
    ppr = copy.deepcopy(existing_ppr) if existing_ppr is not None else ET.Element(qn("pPr"))
    for child in list(paragraph):
        paragraph.remove(child)
    paragraph.append(ppr)
    return ppr


def add_text_run(paragraph: ET.Element, text: str, superscript: bool = False) -> None:
    run = ET.SubElement(paragraph, qn("r"))
    if superscript:
        rpr = ET.SubElement(run, qn("rPr"))
        vert = ET.SubElement(rpr, qn("vertAlign"))
        vert.set(qn("val"), "superscript")
    text_node = ET.SubElement(run, qn("t"))
    text_node.text = text


def replace_author_paragraph(paragraph: ET.Element, author_name: str, aff_indices: list[int]) -> None:
    clear_paragraph_runs(paragraph)
    add_text_run(paragraph, author_name)
    if aff_indices:
        add_text_run(paragraph, ",".join(str(index) for index in aff_indices), superscript=True)


def make_affiliation_paragraph(aff_index: int, aff_text: str) -> ET.Element:
    paragraph = ET.Element(qn("p"))
    ppr = ET.SubElement(paragraph, qn("pPr"))
    pstyle = ET.SubElement(ppr, qn("pStyle"))
    pstyle.set(qn("val"), "Normal")
    justification = ET.SubElement(ppr, qn("jc"))
    justification.set(qn("val"), "center")
    add_text_run(paragraph, f"{aff_index}. {aff_text}")
    return paragraph


def load_bibliography_entries(bib_path: Path) -> dict[str, dict[str, str]]:
    entries: dict[str, dict[str, str]] = {}
    current_key: str | None = None
    current_fields: dict[str, str] = {}

    for raw_line in bib_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line:
            continue

        if line.startswith("@"):
            if current_key is not None:
                entries[current_key] = current_fields
            current_key = line.split("{", 1)[1].split(",", 1)[0].strip()
            current_fields = {}
            continue

        if current_key is None or "=" not in line:
            continue

        field_match = re.match(r"^([A-Za-z]+)\s*=\s*\{(.*)\},?\s*$", line)
        if field_match is None:
            continue

        field_name = field_match.group(1).lower()
        field_value = strip_matching_braces(field_match.group(2))
        current_fields[field_name] = field_value

    if current_key is not None:
        entries[current_key] = current_fields

    return entries


def extract_used_citekeys(article_text: str, bibliography_entries: dict[str, dict[str, str]]) -> list[str]:
    used_citekeys: list[str] = []

    for match in re.finditer(r"@([A-Za-z0-9_:\-]+)", article_text):
        citekey = match.group(1)
        if citekey not in bibliography_entries or citekey in used_citekeys:
            continue
        used_citekeys.append(citekey)

    return used_citekeys


def parse_author_surnames(author_field: str) -> list[str]:
    surnames: list[str] = []

    for author in author_field.split(" and "):
        cleaned_author = strip_matching_braces(author)
        if not cleaned_author:
            continue
        surname = cleaned_author.split(",", 1)[0].strip() if "," in cleaned_author else cleaned_author.strip()
        surnames.append(strip_matching_braces(surname))

    return surnames


def build_citation_variants(entry: dict[str, str]) -> dict[str, str | list[str]] | None:
    year = entry.get("year", "").strip()
    author_field = entry.get("author", "").strip()
    surnames = parse_author_surnames(author_field)
    if not year or not surnames:
        return None

    if len(surnames) == 1:
        author_variants = [surnames[0]]
    elif len(surnames) == 2:
        author_variants = [f"{surnames[0]} and {surnames[1]}", f"{surnames[0]} & {surnames[1]}"]
    else:
        author_variants = [f"{surnames[0]} et al."]

    labels = set()
    for author_variant in author_variants:
        labels.add(f"{author_variant}, {year}")
        labels.add(f"{author_variant} ({year})")
        labels.add(f"{author_variant}\u00a0({year})")

    return {
        "first_author": surnames[0],
        "year": year,
        "labels": sorted(labels, key=len, reverse=True),
    }


def make_text_run_from_template(template_run: ET.Element, text: str) -> ET.Element:
    run = ET.Element(qn("r"))
    template_rpr = template_run.find("./w:rPr", {"w": W_NS})
    if template_rpr is not None:
        run.append(copy.deepcopy(template_rpr))
    text_node = ET.SubElement(run, qn("t"))
    if text.startswith(" ") or text.endswith(" "):
        text_node.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    text_node.text = text
    return run


def ensure_run_style(run: ET.Element, style_id: str) -> None:
    run_properties = run.find("./w:rPr", {"w": W_NS})
    if run_properties is None:
        run_properties = ET.Element(qn("rPr"))
        run.insert(0, run_properties)

    run_style = run_properties.find("./w:rStyle", {"w": W_NS})
    if run_style is None:
        run_style = ET.Element(qn("rStyle"))
        run_properties.insert(0, run_style)
    run_style.set(qn("val"), style_id)


def make_internal_hyperlink(anchor: str, run: ET.Element) -> ET.Element:
    ensure_run_style(run, "Hyperlink")
    hyperlink = ET.Element(qn("hyperlink"))
    hyperlink.set(qn("anchor"), anchor)
    hyperlink.set(qn("history"), "1")
    hyperlink.append(run)
    return hyperlink


def next_bookmark_id(root: ET.Element) -> int:
    bookmark_ids = [
        int(bookmark.get(qn("id")))
        for bookmark in root.findall(".//w:bookmarkStart", {"w": W_NS})
        if (bookmark.get(qn("id")) or "").isdigit()
    ]
    return max(bookmark_ids, default=0) + 1


def ensure_paragraph_bookmark(paragraph: ET.Element, anchor: str, bookmark_id: int) -> int:
    for bookmark in paragraph.findall("./w:bookmarkStart", {"w": W_NS}):
        if bookmark.get(qn("name")) == anchor:
            return bookmark_id

    insert_at = 1 if paragraph.find("./w:pPr", {"w": W_NS}) is not None else 0
    bookmark_start = ET.Element(qn("bookmarkStart"))
    bookmark_start.set(qn("id"), str(bookmark_id))
    bookmark_start.set(qn("name"), anchor)
    paragraph.insert(insert_at, bookmark_start)

    bookmark_end = ET.Element(qn("bookmarkEnd"))
    bookmark_end.set(qn("id"), str(bookmark_id))
    paragraph.append(bookmark_end)

    return bookmark_id + 1


def match_bibliography_paragraph(paragraphs: list[ET.Element], first_author: str, year: str) -> ET.Element | None:
    pattern = re.compile(rf"^{re.escape(first_author)}.*\b{re.escape(year)}\b")

    for paragraph in paragraphs:
        if pattern.search(paragraph_text(paragraph)):
            return paragraph

    return None


def replace_run_segment_with_hyperlink(paragraph: ET.Element, run: ET.Element, match_text: str, anchor: str) -> bool:
    text = run_text(run)
    match_index = text.find(match_text)
    if match_index == -1:
        return False

    before_text = text[:match_index]
    matched_text = text[match_index : match_index + len(match_text)]
    after_text = text[match_index + len(match_text) :]
    insert_at = list(paragraph).index(run)

    new_nodes: list[ET.Element] = []
    if before_text:
        new_nodes.append(make_text_run_from_template(run, before_text))
    new_nodes.append(make_internal_hyperlink(anchor, make_text_run_from_template(run, matched_text)))
    if after_text:
        new_nodes.append(make_text_run_from_template(run, after_text))

    paragraph.remove(run)
    for offset, node in enumerate(new_nodes):
        paragraph.insert(insert_at + offset, node)

    return True


def link_citations_in_paragraph(paragraph: ET.Element, citation_targets: list[dict[str, str | list[str]]]) -> bool:
    changed = False

    while True:
        paragraph_changed = False
        for child in list(paragraph):
            if child.tag != qn("r"):
                continue

            child_text = run_text(child)
            if not child_text:
                continue

            for target in citation_targets:
                anchor = str(target["anchor"])
                for label in target["labels"]:
                    if replace_run_segment_with_hyperlink(paragraph, child, str(label), anchor):
                        changed = True
                        paragraph_changed = True
                        break
                if paragraph_changed:
                    break
            if paragraph_changed:
                break

        if not paragraph_changed:
            break

    return changed


def add_internal_citation_links(
    root: ET.Element,
    bibliography_entries: dict[str, dict[str, str]],
    used_citekeys: list[str],
) -> bool:
    bibliography_paragraphs = [
        paragraph
        for paragraph in root.findall(".//w:p", {"w": W_NS})
        if paragraph_style(paragraph) == "Bibliography"
    ]
    if not bibliography_paragraphs:
        return False

    bookmark_id = next_bookmark_id(root)
    citation_targets: list[dict[str, str | list[str]]] = []
    for cite_index, citekey in enumerate(used_citekeys, start=1):
        entry = build_citation_variants(bibliography_entries.get(citekey, {}))
        if entry is None:
            continue

        bibliography_paragraph = match_bibliography_paragraph(
            bibliography_paragraphs,
            str(entry["first_author"]),
            str(entry["year"]),
        )
        if bibliography_paragraph is None:
            continue

        anchor = f"bibref_{cite_index}"
        bookmark_id = ensure_paragraph_bookmark(bibliography_paragraph, anchor, bookmark_id)
        citation_targets.append({"anchor": anchor, "labels": entry["labels"]})

    changed = False
    for paragraph in root.findall(".//w:p", {"w": W_NS}):
        if paragraph_style(paragraph) == "Bibliography":
            continue
        changed = link_citations_in_paragraph(paragraph, citation_targets) or changed

    return changed


def normalize_citation_hyperlink_styles(root: ET.Element) -> bool:
    changed = False

    for hyperlink in root.findall(".//w:hyperlink", {"w": W_NS}):
        anchor = hyperlink.get(qn("anchor"), "")
        if not anchor.startswith("bibref_"):
            continue

        for run in hyperlink.findall("./w:r", {"w": W_NS}):
            run_properties = run.find("./w:rPr", {"w": W_NS})
            run_style = None if run_properties is None else run_properties.find("./w:rStyle", {"w": W_NS})
            if run_style is not None and run_style.get(qn("val")) == "Hyperlink":
                continue
            ensure_run_style(run, "Hyperlink")
            changed = True

    return changed


def strip_bibliography_external_links(root: ET.Element) -> bool:
    changed = False

    for paragraph in root.findall(".//w:p", {"w": W_NS}):
        if paragraph_style(paragraph) != "Bibliography":
            continue

        insert_at = 0
        for child in list(paragraph):
            if child.tag == qn("hyperlink"):
                for nested_child in list(child):
                    paragraph.insert(insert_at, nested_child)
                    insert_at += 1
                paragraph.remove(child)
                changed = True
                continue

            insert_at += 1

        for run in paragraph.findall("./w:r", {"w": W_NS}):
            run_properties = run.find("./w:rPr", {"w": W_NS})
            if run_properties is None:
                continue
            run_style = run_properties.find("./w:rStyle", {"w": W_NS})
            if run_style is None or run_style.get(qn("val")) != "Hyperlink":
                continue
            run_properties.remove(run_style)
            if len(run_properties) == 0:
                run.remove(run_properties)
            changed = True

    return changed


def remove_unused_hyperlink_relationships(files: dict[str, bytes]) -> None:
    rels_path = "word/_rels/document.xml.rels"
    if rels_path not in files or "word/document.xml" not in files:
        return

    rels_root = ET.fromstring(files[rels_path])
    document_root = ET.fromstring(files["word/document.xml"])
    rid_attr = f"{{{DOC_REL_NS}}}id"
    used_rids = {
        hyperlink.get(rid_attr)
        for hyperlink in document_root.findall(".//w:hyperlink", {"w": W_NS})
        if hyperlink.get(rid_attr)
    }

    changed = False
    for relationship in list(rels_root):
        if not relationship.tag.endswith("Relationship"):
            continue
        if not relationship.get("Type", "").endswith("/hyperlink"):
            continue
        if relationship.get("Id") in used_rids:
            continue
        rels_root.remove(relationship)
        changed = True

    if changed:
        files[rels_path] = ET.tostring(rels_root, encoding="utf-8", xml_declaration=True)


def patch_docx(
    docx_path: Path,
    authors: list[dict],
    affiliations: list[str],
    bibliography_entries: dict[str, dict[str, str]],
    used_citekeys: list[str],
) -> bool:
    if not docx_path.exists():
        return False

    with zipfile.ZipFile(docx_path, "r") as archive:
        files = {name: archive.read(name) for name in archive.namelist()}

    root = ET.fromstring(files["word/document.xml"])
    body = root.find("./w:body", {"w": W_NS})
    if body is None:
        return False

    body_children = list(body)
    author_positions = [idx for idx, child in enumerate(body_children) if child.tag == qn("p") and paragraph_style(child) == "Author"]
    if author_positions and authors and affiliations:
        first_author_idx = author_positions[0]
        date_idx = next(
            (
                idx
                for idx, child in enumerate(body_children[first_author_idx + len(author_positions) :], start=first_author_idx + len(author_positions))
                if child.tag == qn("p") and paragraph_style(child) == "Date"
            ),
            None,
        )

        if date_idx is not None:
            for paragraph, author in zip((body_children[idx] for idx in author_positions), authors):
                replace_author_paragraph(paragraph, author["name"], author["indices"])

            existing_between = body_children[author_positions[-1] + 1 : date_idx]
            affiliation_texts = {f"{idx}. {text}" for idx, text in enumerate(affiliations, start=1)}
            for node in existing_between:
                if node.tag != qn("p"):
                    continue
                if paragraph_text(node) in affiliation_texts:
                    body.remove(node)

            insert_at = list(body).index(body_children[author_positions[-1]]) + 1
            for aff_idx, aff_text in enumerate(affiliations, start=1):
                paragraph = make_affiliation_paragraph(aff_idx, aff_text)
                body.insert(insert_at, paragraph)
                insert_at += 1

    strip_bibliography_external_links(root)
    add_internal_citation_links(root, bibliography_entries, used_citekeys)
    normalize_citation_hyperlink_styles(root)
    files["word/document.xml"] = ET.tostring(root, encoding="utf-8", xml_declaration=True)
    remove_unused_hyperlink_relationships(files)

    with zipfile.ZipFile(docx_path, "w") as archive:
        for name, content in files.items():
            archive.writestr(name, content)

    return True


def main() -> None:
    paper_dir = Path(__file__).resolve().parent
    project_info = inspect_quarto_project(paper_dir)
    project_config = project_info["config"]
    article_name = project_config["manuscript"]["article"]
    article_path = (paper_dir / article_name).resolve()
    metadata = project_info["fileInformation"][article_name]["metadata"]
    authors, affiliations = build_author_affiliations(metadata)
    bibliography_path = paper_dir / project_config["bibliography"]
    bibliography_entries = load_bibliography_entries(bibliography_path)
    used_citekeys = extract_used_citekeys(article_path.read_text(encoding="utf-8"), bibliography_entries)

    output_dir = (paper_dir / project_config["project"]["output-dir"]).resolve()
    output_basename = project_config["format"]["docx"].get("output-file", Path(article_name).stem)
    source_docx_path = paper_dir / f"{output_basename}.docx"
    final_docx_path = output_dir / f"{output_basename}.docx"

    patch_docx(source_docx_path, authors, affiliations, bibliography_entries, used_citekeys)
    patch_docx(final_docx_path, authors, affiliations, bibliography_entries, used_citekeys)


if __name__ == "__main__":
    main()
