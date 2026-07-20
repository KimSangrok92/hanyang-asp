"""Create the fully synthetic ASP Toolkit example workbook.

The script uses only the Python standard library so the generated XLSX can be
reproduced without Excel or third-party Python packages.
"""

from __future__ import annotations

import argparse
import calendar
import random
import zipfile
from datetime import date, datetime, time, timedelta, timezone
from pathlib import Path
from xml.etree import ElementTree as ET


SEED = 20260719
GENERATED_AT = datetime(2026, 7, 19, tzinfo=timezone.utc)
XLSX_NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
PKG_REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
CORE_NS = "http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
DC_NS = "http://purl.org/dc/elements/1.1/"
DCTERMS_NS = "http://purl.org/dc/terms/"
XSI_NS = "http://www.w3.org/2001/XMLSchema-instance"
APP_NS = "http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
VT_NS = "http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"

ET.register_namespace("", XLSX_NS)
ET.register_namespace("r", REL_NS)
ET.register_namespace("cp", CORE_NS)
ET.register_namespace("dc", DC_NS)
ET.register_namespace("dcterms", DCTERMS_NS)
ET.register_namespace("xsi", XSI_NS)
ET.register_namespace("vt", VT_NS)


def qname(namespace: str, tag: str) -> str:
    return f"{{{namespace}}}{tag}"


def month_start(year: int, month: int) -> date:
    return date(year, month, 1)


def column_name(index: int) -> str:
    value = index
    chars: list[str] = []
    while value:
        value, remainder = divmod(value - 1, 26)
        chars.append(chr(65 + remainder))
    return "".join(reversed(chars))


def xml_bytes(element: ET.Element) -> bytes:
    return ET.tostring(element, encoding="utf-8", xml_declaration=True)


def build_data() -> dict[str, list[list[object]]]:
    rng = random.Random(SEED)

    profiles = [
        {
            "department": "Infectious Diseases",
            "c1": "Bloodstream infection",
            "c2": "Primary bacteremia",
            "diagnosis": "Bacteremia",
        },
        {
            "department": "Pulmonology",
            "c1": "Respiratory infection",
            "c2": "Community-acquired pneumonia",
            "diagnosis": "Pneumonia, unspecified organism",
        },
        {
            "department": "Gastroenterology",
            "c1": "Intra-abdominal infection",
            "c2": "Biliary tract infection",
            "diagnosis": "Acute cholangitis",
        },
        {
            "department": "Pediatrics",
            "c1": "Respiratory infection",
            "c2": "Pediatric pneumonia",
            "diagnosis": "Pneumonia, unspecified organism",
        },
        {
            "department": "Emergency Medicine",
            "c1": "Urinary tract infection",
            "c2": "Complicated urinary tract infection",
            "diagnosis": "Acute pyelonephritis",
        },
        {
            "department": "General Surgery",
            "c1": "Skin and soft tissue infection",
            "c2": "Cellulitis",
            "diagnosis": "Cellulitis of lower limb",
        },
    ]

    drugs = [
        {
            "ingredient": "Cefazolin",
            "atc": "J01DB04",
            "form": "Injection",
            "route": "P",
            "dose": 1.0,
            "unit": "G",
            "unit_g": 1.0,
            "ddd": 3.0,
            "ddd_unit": "G",
            "class": "1세대, 2세대 세팔로스포린 계통 항생제",
            "hours": [6, 14, 22],
        },
        {
            "ingredient": "Ceftriaxone",
            "atc": "J01DD04",
            "form": "Injection",
            "route": "P",
            "dose": 2.0,
            "unit": "G",
            "unit_g": 1.0,
            "ddd": 2.0,
            "ddd_unit": "G",
            "class": "3세대, 4세대 세팔로스포린 계통 항생제",
            "hours": [9],
        },
        {
            "ingredient": "Piperacillin/tazobactam",
            "atc": "J01CR05",
            "form": "Injection",
            "route": "P",
            "dose": 4.5,
            "unit": "G",
            "unit_g": 1.0,
            "ddd": 14.0,
            "ddd_unit": "G",
            "class": "항녹농균 효과를 가진 페니실린 계통 항생제",
            "hours": [6, 14, 22],
        },
        {
            "ingredient": "Cefepime",
            "atc": "J01DE01",
            "form": "Injection",
            "route": "P",
            "dose": 2.0,
            "unit": "G",
            "unit_g": 1.0,
            "ddd": 4.0,
            "ddd_unit": "G",
            "class": "3세대, 4세대 세팔로스포린 계통 항생제",
            "hours": [9, 21],
        },
        {
            "ingredient": "Meropenem",
            "atc": "J01DH02",
            "form": "Injection",
            "route": "P",
            "dose": 1.0,
            "unit": "G",
            "unit_g": 1.0,
            "ddd": 3.0,
            "ddd_unit": "G",
            "class": "카바페넴 계통 항생제",
            "hours": [6, 14, 22],
        },
        {
            "ingredient": "Vancomycin",
            "atc": "J01XA01",
            "form": "Injection",
            "route": "P",
            "dose": 1.0,
            "unit": "G",
            "unit_g": 1.0,
            "ddd": 2.0,
            "ddd_unit": "G",
            "class": "글리코펩타이드 계통 항생제",
            "hours": [9, 21],
        },
        {
            "ingredient": "Levofloxacin",
            "atc": "J01MA12",
            "form": "Tablet",
            "route": "O",
            "dose": 1.0,
            "unit": "TAB",
            "unit_g": 0.5,
            "ddd": 0.5,
            "ddd_unit": "G",
            "class": "퀴놀론 계통 항생제",
            "hours": [9],
        },
    ]

    patient_headers = [
        "MDRP_NO",
        "PTNO",
        "ADMTIME",
        "DISCHTIME",
        "sex",
        "ADM_DEPARTMENT",
        "adm_type",
        "inhospital_expire",
        "age",
    ]
    diagnosis_headers = [
        "MDRP_NO",
        "PTNO",
        "DIAG_DATE",
        "ICD_NAME",
        "Classification1",
        "Classification2",
    ]
    medication_order_headers = [
        "MDRP_NO",
        "PTNO",
        "성분명",
        "경로",
        "일회량",
        "단위",
        "투여일시",
        "age",
    ]
    antibiotic_headers = [
        "MDRP_NO",
        "PTNO",
        "ATC_Code",
        "성분명",
        "제형",
        "경로",
        "일회량",
        "단위",
        "단위환산함량",
        "DDD",
        "DDD단위",
        "투여일시",
        "class",
        "age",
    ]

    patient_rows: list[list[object]] = [patient_headers]
    diagnosis_rows: list[list[object]] = [diagnosis_headers]
    medication_order_rows: list[list[object]] = [medication_order_headers]
    antibiotic_rows: list[list[object]] = [antibiotic_headers]
    encounters: list[dict[str, object]] = []
    adult_ages = [18, 24, 31, 39, 47, 55, 63, 71, 78, 84]
    pediatric_ages = [3, 5, 7, 9, 11, 13, 14]
    los_pattern = [4, 7, 12, 18, 26, 38, 55, 9, 15, 31]

    encounter_number = 0
    for month_index in range(12):
        current_month = month_start(2025, month_index + 1)
        days_in_month = calendar.monthrange(current_month.year, current_month.month)[1]
        admissions_this_month = 9 if month_index < 4 else 8
        for slot in range(admissions_this_month):
            profile = profiles[(slot + month_index) % len(profiles)]
            encounter_number += 1
            encounter_id = f"SYN-E{encounter_number:04d}"
            patient_number = encounter_number
            patient_id = f"SYN-P{patient_number:04d}"
            is_pediatric = profile["department"] == "Pediatrics"
            age = rng.choice(pediatric_ages if is_pediatric else adult_ages)
            sex = "M" if patient_number % 2 else "F"
            day_value = 2 + ((month_index * 3 + slot * 4) % min(22, days_in_month - 7))
            admission_date = date(current_month.year, current_month.month, day_value)
            length_of_stay = los_pattern[(encounter_number + month_index) % len(los_pattern)]
            discharge_date = admission_date + timedelta(days=length_of_stay)
            admission_type = "E" if encounter_number % 4 == 0 else "I"
            expired = "Y" if encounter_number in {23, 59} else "N"

            patient_rows.append(
                [
                    encounter_id,
                    patient_id,
                    f"{admission_date.isoformat()} 10:00:00",
                    f"{discharge_date.isoformat()} 11:00:00",
                    sex,
                    profile["department"],
                    admission_type,
                    expired,
                    age,
                ]
            )

            diagnosis_rows.append(
                [
                    encounter_id,
                    patient_id,
                    (admission_date + timedelta(days=1)).isoformat(),
                    profile["diagnosis"],
                    profile["c1"],
                    profile["c2"],
                ]
            )

            encounters.append(
                {
                    "encounter_id": encounter_id,
                    "patient_id": patient_id,
                    "admission_date": admission_date,
                    "length_of_stay": length_of_stay,
                    "age": age,
                    "pediatric": is_pediatric,
                    "slot": slot,
                    "month_index": month_index,
                }
            )

    for encounter_index, encounter in enumerate(encounters):
        if encounter["pediatric"]:
            drug = drugs[encounter_index % 2]
        else:
            primary_index = (encounter["month_index"] + encounter["slot"]) % len(drugs)
            drug = drugs[primary_index]

        administration_date = encounter["admission_date"] + timedelta(
            days=encounter_index % min(int(encounter["length_of_stay"]), 3)
        )
        dose = drug["dose"]
        if encounter["pediatric"] and drug["ingredient"] == "Cefazolin":
            dose = 0.5
        if encounter["pediatric"] and drug["ingredient"] == "Ceftriaxone":
            dose = 1.0
        administered_at = datetime.combine(
            administration_date, time(hour=int(drug["hours"][0]), minute=0)
        )

        medication_order_rows.append(
            [
                encounter["encounter_id"],
                encounter["patient_id"],
                drug["ingredient"],
                drug["route"],
                dose,
                drug["unit"],
                administered_at.strftime("%Y-%m-%d %H:%M:%S"),
                encounter["age"],
            ]
        )
        antibiotic_rows.append(
            [
                encounter["encounter_id"],
                encounter["patient_id"],
                drug["atc"],
                drug["ingredient"],
                drug["form"],
                drug["route"],
                dose,
                drug["unit"],
                drug["unit_g"],
                drug["ddd"],
                drug["ddd_unit"],
                administered_at.strftime("%Y-%m-%d %H:%M:%S"),
                drug["class"],
                encounter["age"],
            ]
        )

    patient_days_values = [
        4320,
        4080,
        4460,
        4510,
        4690,
        4750,
        4920,
        4870,
        4630,
        4710,
        4580,
        4490,
    ]
    patient_days_rows: list[list[object]] = [["month", "patient_days"]]
    for month_index, value in enumerate(patient_days_values, start=1):
        patient_days_rows.append([f"2025-{month_index:02d}", value])

    return {
        "Sheet1": diagnosis_rows,
        "Sheet2": medication_order_rows,
        "Sheet3": patient_rows,
        "Sheet4": antibiotic_rows,
        "Sheet5": patient_days_rows,
    }


def build_worksheet(rows: list[list[object]]) -> bytes:
    worksheet = ET.Element(qname(XLSX_NS, "worksheet"))
    max_columns = max(len(row) for row in rows)
    ET.SubElement(
        worksheet,
        qname(XLSX_NS, "dimension"),
        {"ref": f"A1:{column_name(max_columns)}{len(rows)}"},
    )

    sheet_views = ET.SubElement(worksheet, qname(XLSX_NS, "sheetViews"))
    sheet_view = ET.SubElement(
        sheet_views, qname(XLSX_NS, "sheetView"), {"workbookViewId": "0"}
    )
    ET.SubElement(
        sheet_view,
        qname(XLSX_NS, "pane"),
        {
            "ySplit": "1",
            "topLeftCell": "A2",
            "activePane": "bottomLeft",
            "state": "frozen",
        },
    )
    ET.SubElement(worksheet, qname(XLSX_NS, "sheetFormatPr"), {"defaultRowHeight": "15"})

    columns = ET.SubElement(worksheet, qname(XLSX_NS, "cols"))
    for index in range(1, max_columns + 1):
        values = [str(row[index - 1]) for row in rows if len(row) >= index and row[index - 1] is not None]
        width = min(max(max((len(value) for value in values), default=8) + 2, 11), 36)
        ET.SubElement(
            columns,
            qname(XLSX_NS, "col"),
            {
                "min": str(index),
                "max": str(index),
                "width": str(width),
                "customWidth": "1",
            },
        )

    sheet_data = ET.SubElement(worksheet, qname(XLSX_NS, "sheetData"))
    for row_index, row_values in enumerate(rows, start=1):
        row_element = ET.SubElement(
            sheet_data,
            qname(XLSX_NS, "row"),
            {"r": str(row_index), "ht": "22" if row_index == 1 else "18", "customHeight": "1"},
        )
        for column_index, value in enumerate(row_values, start=1):
            if value is None:
                continue
            reference = f"{column_name(column_index)}{row_index}"
            attributes = {"r": reference}
            if row_index == 1:
                attributes["s"] = "1"
            elif column_index == len(row_values) and str(value) == "YES":
                attributes["s"] = "2"

            if isinstance(value, (int, float)) and not isinstance(value, bool):
                cell = ET.SubElement(row_element, qname(XLSX_NS, "c"), attributes)
                ET.SubElement(cell, qname(XLSX_NS, "v")).text = str(value)
            else:
                attributes["t"] = "inlineStr"
                cell = ET.SubElement(row_element, qname(XLSX_NS, "c"), attributes)
                inline_string = ET.SubElement(cell, qname(XLSX_NS, "is"))
                text_element = ET.SubElement(inline_string, qname(XLSX_NS, "t"))
                text_element.text = str(value)

    ET.SubElement(
        worksheet,
        qname(XLSX_NS, "autoFilter"),
        {"ref": f"A1:{column_name(max_columns)}{len(rows)}"},
    )
    ET.SubElement(
        worksheet,
        qname(XLSX_NS, "pageMargins"),
        {
            "left": "0.7",
            "right": "0.7",
            "top": "0.75",
            "bottom": "0.75",
            "header": "0.3",
            "footer": "0.3",
        },
    )
    return xml_bytes(worksheet)


def build_workbook(sheet_names: list[str]) -> bytes:
    workbook = ET.Element(qname(XLSX_NS, "workbook"))
    book_views = ET.SubElement(workbook, qname(XLSX_NS, "bookViews"))
    ET.SubElement(
        book_views,
        qname(XLSX_NS, "workbookView"),
        {"xWindow": "0", "yWindow": "0", "windowWidth": "24000", "windowHeight": "12000"},
    )
    sheets = ET.SubElement(workbook, qname(XLSX_NS, "sheets"))
    for index, name in enumerate(sheet_names, start=1):
        ET.SubElement(
            sheets,
            qname(XLSX_NS, "sheet"),
            {
                "name": name,
                "sheetId": str(index),
                qname(REL_NS, "id"): f"rId{index}",
            },
        )
    ET.SubElement(workbook, qname(XLSX_NS, "calcPr"), {"calcId": "191029"})
    return xml_bytes(workbook)


def build_workbook_relationships(sheet_count: int) -> bytes:
    relationships = ET.Element(qname(PKG_REL_NS, "Relationships"))
    for index in range(1, sheet_count + 1):
        ET.SubElement(
            relationships,
            qname(PKG_REL_NS, "Relationship"),
            {
                "Id": f"rId{index}",
                "Type": "http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet",
                "Target": f"worksheets/sheet{index}.xml",
            },
        )
    ET.SubElement(
        relationships,
        qname(PKG_REL_NS, "Relationship"),
        {
            "Id": f"rId{sheet_count + 1}",
            "Type": "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles",
            "Target": "styles.xml",
        },
    )
    return xml_bytes(relationships)


def build_styles() -> bytes:
    style_sheet = ET.Element(qname(XLSX_NS, "styleSheet"))

    fonts = ET.SubElement(style_sheet, qname(XLSX_NS, "fonts"), {"count": "3"})
    default_font = ET.SubElement(fonts, qname(XLSX_NS, "font"))
    ET.SubElement(default_font, qname(XLSX_NS, "sz"), {"val": "10"})
    ET.SubElement(default_font, qname(XLSX_NS, "name"), {"val": "Aptos"})
    ET.SubElement(default_font, qname(XLSX_NS, "family"), {"val": "2"})

    header_font = ET.SubElement(fonts, qname(XLSX_NS, "font"))
    ET.SubElement(header_font, qname(XLSX_NS, "b"))
    ET.SubElement(header_font, qname(XLSX_NS, "color"), {"rgb": "FFFFFFFF"})
    ET.SubElement(header_font, qname(XLSX_NS, "sz"), {"val": "10"})
    ET.SubElement(header_font, qname(XLSX_NS, "name"), {"val": "Aptos"})

    marker_font = ET.SubElement(fonts, qname(XLSX_NS, "font"))
    ET.SubElement(marker_font, qname(XLSX_NS, "b"))
    ET.SubElement(marker_font, qname(XLSX_NS, "color"), {"rgb": "FF156F67"})
    ET.SubElement(marker_font, qname(XLSX_NS, "sz"), {"val": "10"})
    ET.SubElement(marker_font, qname(XLSX_NS, "name"), {"val": "Aptos"})

    fills = ET.SubElement(style_sheet, qname(XLSX_NS, "fills"), {"count": "4"})
    fill_none = ET.SubElement(fills, qname(XLSX_NS, "fill"))
    ET.SubElement(fill_none, qname(XLSX_NS, "patternFill"), {"patternType": "none"})
    fill_gray = ET.SubElement(fills, qname(XLSX_NS, "fill"))
    ET.SubElement(fill_gray, qname(XLSX_NS, "patternFill"), {"patternType": "gray125"})
    fill_header = ET.SubElement(fills, qname(XLSX_NS, "fill"))
    header_pattern = ET.SubElement(fill_header, qname(XLSX_NS, "patternFill"), {"patternType": "solid"})
    ET.SubElement(header_pattern, qname(XLSX_NS, "fgColor"), {"rgb": "FF167C80"})
    ET.SubElement(header_pattern, qname(XLSX_NS, "bgColor"), {"indexed": "64"})
    fill_marker = ET.SubElement(fills, qname(XLSX_NS, "fill"))
    marker_pattern = ET.SubElement(fill_marker, qname(XLSX_NS, "patternFill"), {"patternType": "solid"})
    ET.SubElement(marker_pattern, qname(XLSX_NS, "fgColor"), {"rgb": "FFE5F6F2"})
    ET.SubElement(marker_pattern, qname(XLSX_NS, "bgColor"), {"indexed": "64"})

    borders = ET.SubElement(style_sheet, qname(XLSX_NS, "borders"), {"count": "2"})
    default_border = ET.SubElement(borders, qname(XLSX_NS, "border"))
    for side in ("left", "right", "top", "bottom", "diagonal"):
        ET.SubElement(default_border, qname(XLSX_NS, side))
    thin_border = ET.SubElement(borders, qname(XLSX_NS, "border"))
    for side in ("left", "right", "top", "bottom"):
        side_element = ET.SubElement(thin_border, qname(XLSX_NS, side), {"style": "thin"})
        ET.SubElement(side_element, qname(XLSX_NS, "color"), {"rgb": "FFB9D8D2"})
    ET.SubElement(thin_border, qname(XLSX_NS, "diagonal"))

    cell_style_xfs = ET.SubElement(style_sheet, qname(XLSX_NS, "cellStyleXfs"), {"count": "1"})
    ET.SubElement(
        cell_style_xfs,
        qname(XLSX_NS, "xf"),
        {"numFmtId": "0", "fontId": "0", "fillId": "0", "borderId": "0"},
    )

    cell_xfs = ET.SubElement(style_sheet, qname(XLSX_NS, "cellXfs"), {"count": "3"})
    ET.SubElement(
        cell_xfs,
        qname(XLSX_NS, "xf"),
        {"numFmtId": "0", "fontId": "0", "fillId": "0", "borderId": "0", "xfId": "0"},
    )
    header_xf = ET.SubElement(
        cell_xfs,
        qname(XLSX_NS, "xf"),
        {
            "numFmtId": "0",
            "fontId": "1",
            "fillId": "2",
            "borderId": "1",
            "xfId": "0",
            "applyAlignment": "1",
        },
    )
    ET.SubElement(header_xf, qname(XLSX_NS, "alignment"), {"horizontal": "center", "vertical": "center"})
    ET.SubElement(
        cell_xfs,
        qname(XLSX_NS, "xf"),
        {
            "numFmtId": "0",
            "fontId": "2",
            "fillId": "3",
            "borderId": "0",
            "xfId": "0",
        },
    )

    cell_styles = ET.SubElement(style_sheet, qname(XLSX_NS, "cellStyles"), {"count": "1"})
    ET.SubElement(
        cell_styles,
        qname(XLSX_NS, "cellStyle"),
        {"name": "Normal", "xfId": "0", "builtinId": "0"},
    )
    ET.SubElement(style_sheet, qname(XLSX_NS, "dxfs"), {"count": "0"})
    ET.SubElement(
        style_sheet,
        qname(XLSX_NS, "tableStyles"),
        {"count": "0", "defaultTableStyle": "TableStyleMedium2", "defaultPivotStyle": "PivotStyleLight16"},
    )
    return xml_bytes(style_sheet)


def build_content_types(sheet_count: int) -> bytes:
    namespace = "http://schemas.openxmlformats.org/package/2006/content-types"
    types = ET.Element(qname(namespace, "Types"))
    ET.SubElement(types, qname(namespace, "Default"), {"Extension": "rels", "ContentType": "application/vnd.openxmlformats-package.relationships+xml"})
    ET.SubElement(types, qname(namespace, "Default"), {"Extension": "xml", "ContentType": "application/xml"})
    ET.SubElement(types, qname(namespace, "Override"), {"PartName": "/xl/workbook.xml", "ContentType": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"})
    ET.SubElement(types, qname(namespace, "Override"), {"PartName": "/xl/styles.xml", "ContentType": "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"})
    for index in range(1, sheet_count + 1):
        ET.SubElement(types, qname(namespace, "Override"), {"PartName": f"/xl/worksheets/sheet{index}.xml", "ContentType": "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"})
    ET.SubElement(types, qname(namespace, "Override"), {"PartName": "/docProps/core.xml", "ContentType": "application/vnd.openxmlformats-package.core-properties+xml"})
    ET.SubElement(types, qname(namespace, "Override"), {"PartName": "/docProps/app.xml", "ContentType": "application/vnd.openxmlformats-officedocument.extended-properties+xml"})
    return xml_bytes(types)


def build_root_relationships() -> bytes:
    relationships = ET.Element(qname(PKG_REL_NS, "Relationships"))
    ET.SubElement(relationships, qname(PKG_REL_NS, "Relationship"), {"Id": "rId1", "Type": "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument", "Target": "xl/workbook.xml"})
    ET.SubElement(relationships, qname(PKG_REL_NS, "Relationship"), {"Id": "rId2", "Type": "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties", "Target": "docProps/core.xml"})
    ET.SubElement(relationships, qname(PKG_REL_NS, "Relationship"), {"Id": "rId3", "Type": "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties", "Target": "docProps/app.xml"})
    return xml_bytes(relationships)


def build_core_properties() -> bytes:
    properties = ET.Element(qname(CORE_NS, "coreProperties"))
    ET.SubElement(properties, qname(DC_NS, "creator")).text = "ASP Toolkit"
    ET.SubElement(properties, qname(CORE_NS, "lastModifiedBy")).text = "ASP Toolkit"
    ET.SubElement(properties, qname(DC_NS, "title")).text = "ASP Toolkit Synthetic Example"
    ET.SubElement(properties, qname(DC_NS, "subject")).text = "Synthetic training data"
    ET.SubElement(properties, qname(DC_NS, "description")).text = "Fictional data for ASP Toolkit demonstration only"
    created = ET.SubElement(properties, qname(DCTERMS_NS, "created"), {qname(XSI_NS, "type"): "dcterms:W3CDTF"})
    created.text = GENERATED_AT.isoformat().replace("+00:00", "Z")
    modified = ET.SubElement(properties, qname(DCTERMS_NS, "modified"), {qname(XSI_NS, "type"): "dcterms:W3CDTF"})
    modified.text = GENERATED_AT.isoformat().replace("+00:00", "Z")
    return xml_bytes(properties)


def build_app_properties(sheet_names: list[str]) -> bytes:
    properties = ET.Element(qname(APP_NS, "Properties"))
    ET.SubElement(properties, qname(APP_NS, "Application")).text = "ASP Toolkit"
    heading_pairs = ET.SubElement(properties, qname(APP_NS, "HeadingPairs"))
    pair_vector = ET.SubElement(heading_pairs, qname(VT_NS, "vector"), {"size": "2", "baseType": "variant"})
    variant_one = ET.SubElement(pair_vector, qname(VT_NS, "variant"))
    ET.SubElement(variant_one, qname(VT_NS, "lpstr")).text = "Worksheets"
    variant_two = ET.SubElement(pair_vector, qname(VT_NS, "variant"))
    ET.SubElement(variant_two, qname(VT_NS, "i4")).text = str(len(sheet_names))
    titles = ET.SubElement(properties, qname(APP_NS, "TitlesOfParts"))
    title_vector = ET.SubElement(titles, qname(VT_NS, "vector"), {"size": str(len(sheet_names)), "baseType": "lpstr"})
    for name in sheet_names:
        ET.SubElement(title_vector, qname(VT_NS, "lpstr")).text = name
    ET.SubElement(properties, qname(APP_NS, "AppVersion")).text = "1.0"
    return xml_bytes(properties)


def write_zip_entry(archive: zipfile.ZipFile, name: str, content: bytes) -> None:
    info = zipfile.ZipInfo(name, date_time=(2026, 7, 19, 0, 0, 0))
    info.compress_type = zipfile.ZIP_DEFLATED
    info.external_attr = 0o600 << 16
    archive.writestr(info, content)


def create_workbook(output_path: Path) -> dict[str, int]:
    sheets = build_data()
    sheet_names = list(sheets)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with zipfile.ZipFile(output_path, "w") as archive:
        write_zip_entry(archive, "[Content_Types].xml", build_content_types(len(sheet_names)))
        write_zip_entry(archive, "_rels/.rels", build_root_relationships())
        write_zip_entry(archive, "docProps/core.xml", build_core_properties())
        write_zip_entry(archive, "docProps/app.xml", build_app_properties(sheet_names))
        write_zip_entry(archive, "xl/workbook.xml", build_workbook(sheet_names))
        write_zip_entry(archive, "xl/_rels/workbook.xml.rels", build_workbook_relationships(len(sheet_names)))
        write_zip_entry(archive, "xl/styles.xml", build_styles())
        for index, rows in enumerate(sheets.values(), start=1):
            write_zip_entry(archive, f"xl/worksheets/sheet{index}.xml", build_worksheet(rows))

    return {name: len(rows) - 1 for name, rows in sheets.items()}


def main() -> None:
    default_output = Path(__file__).resolve().parents[1] / "examples" / "ASP_Toolkit_Example.xlsx"
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=default_output)
    args = parser.parse_args()

    counts = create_workbook(args.output.resolve())
    print(f"Created: {args.output.resolve()}")
    for name, count in counts.items():
        print(f"{name}: {count} data rows")


if __name__ == "__main__":
    main()
