"""Local end-to-end tests using synthetic data in English and German."""
import csv
import os
import re
import socket
import subprocess
import tempfile
import time
from datetime import datetime
from pathlib import Path
from urllib.request import ProxyHandler, build_opener

from playwright.sync_api import sync_playwright, expect

ROOT = Path(__file__).resolve().parents[1]


def run_browser(url, directory):
    errors = []
    with sync_playwright() as p:
        browser = p.chromium.launch(
            executable_path=os.environ.get("CHROMIUM_PATH") or None,
            headless=True, args=["--no-sandbox", "--disable-dev-shm-usage"])
        page = browser.new_page(viewport={"width": 1440, "height": 1000}, accept_downloads=True)
        page.on("pageerror", lambda exc: errors.append(str(exc)))
        page.goto(url, wait_until="networkidle")

        export_names = {}

        def download(name, control="batch_download", label="Results"):
            with page.expect_download() as info:
                page.locator(f"#{control}").click()
            suggested = info.value.suggested_filename
            assert re.fullmatch(rf"OGRS3_{label}_\d{{4}}-\d{{2}}-\d{{2}}_\d{{2}}-\d{{2}}-\d{{2}}_UTC\.csv", suggested), suggested
            destination = directory / name
            info.value.save_as(destination)
            export_names[name] = suggested
            if control != "example_download":
                with destination.open(encoding="utf-8-sig", newline="") as file:
                    row = next(csv.DictReader(file))
                calculated = datetime.strptime(row["Calculated_at_UTC"], "%Y-%m-%dT%H:%M:%SZ")
                assert suggested == f"OGRS3_{label}_{calculated:%Y-%m-%d_%H-%M-%S}_UTC.csv"
            return destination

        def check_brand_spacing():
            brand = page.locator(".navbar-brand").bounding_box()
            first_tab = page.locator('a[data-value="single"]').bounding_box()
            assert first_tab["x"] - (brand["x"] + brand["width"]) >= 24
            assert page.evaluate("document.documentElement.scrollWidth <= innerWidth + 1")

        def check_mapping_controls():
            # A selected value must not be followed by a clipped empty-choice
            # prompt. Long selected labels must fit inside their controls.
            page.wait_for_function("""() => Array.from(document.querySelectorAll(
                '#column_mapping select, #category_mapping select')).every(el => {
                const c = el.selectize;
                return c && (c.items.length ? !c.$control_input.attr('placeholder') :
                    c.$control_input.attr('placeholder') === c.settings.placeholder);
            })""")
            clipped = page.locator("#column_mapping select, #category_mapping select").evaluate_all("""els => els.flatMap(el => {
                const c = el.selectize;
                const bounds = c.$control[0].getBoundingClientRect();
                return Array.from(c.$control[0].querySelectorAll('.item')).filter(item => {
                    const r = item.getBoundingClientRect();
                    return item.scrollWidth > item.clientWidth + 1 || r.left < bounds.left ||
                        r.right > bounds.right - 20 || r.bottom > bounds.bottom;
                }).map(item => ({id:el.id, text:item.textContent}));
            })""")
            assert not clipped, clipped
            assert page.evaluate("document.documentElement.scrollWidth <= innerWidth + 1")

        expect(page.locator("html")).to_have_attribute("lang", "en")
        expect(page.locator("#language")).to_have_value("en")
        expect(page.get_by_role("heading", name="Estimate the risk of general reoffending.")).to_be_visible()
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("click Calculate result")
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("CSV file")
        expect(page.locator('a[data-value="methods"]')).to_have_text("Methods")
        expect(page.locator('a[data-value="about"]')).to_have_text("About the app")
        expect(page.locator(".tab-pane.active")).not_to_contain_text("specific prediction of violent or sexual")
        check_brand_spacing()
        expect(page.locator("#prob_one")).to_have_count(0)
        page.locator("#single_example").click()
        expect(page.locator("#example_notice")).to_contain_text("Example loaded")
        page.locator("#single_calculate").click()
        expect(page.locator("#prob_one")).to_have_text("12.2 %")
        expect(page.locator("#prob_two")).to_have_text("22.1 %")
        expect(page.locator("#copas_value")).to_have_text("-2.303")
        expect(page.locator("#single_results .csv-export-note")).to_be_visible()
        expect(page.locator("#single_results .csv-export-note")).to_contain_text("UTF-8, commas as field separators and a decimal point")
        single_english = download("single-en.csv", "single_download", "Single_case").read_bytes()
        with (directory / "single-en.csv").open(encoding="utf-8-sig", newline="") as file:
            single_row = next(csv.DictReader(file))
        assert single_row["Item1"] == "m"
        assert abs(float(single_row["Prob_one_year"]) - .12161553504714573) < 1e-12
        page.screenshot(path=str(directory / "single-case-english.png"), full_page=True)

        def select(id_, value):
            page.wait_for_function("""([id, value]) => {
                const el = document.getElementById(id);
                if (!el || el.tagName !== 'SELECT') return false;
                return el.selectize ? value === '' || Object.hasOwn(el.selectize.options, value) :
                    Array.from(el.options).some(option => option.value === value);
            }""", arg=[id_, value])
            page.evaluate("""([id, value]) => {
                const el = document.getElementById(id);
                if (!el || el.tagName !== 'SELECT') throw new Error('Expected a select element: ' + id);
                if (el.selectize) el.selectize.setValue(value);
                else { el.value = value; el.dispatchEvent(new Event('change', {bubbles:true})); }
            }""", [id_, value])

        def language(value):
            page.locator("#language").select_option(value)
            expect(page.locator("html")).to_have_attribute("lang", value)

        language("de")
        expect(page.locator("#prob_one")).to_have_text("12,2 %")
        expect(page.locator("#prob_two")).to_have_text("22,1 %")
        expect(page.locator("#copas_value")).to_have_text("-2,303")
        expect(page.get_by_role("heading", name="Allgemeines Rückfallrisiko einschätzen.")).to_be_visible()
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("unter den Eingabefeldern")
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("CSV-Datei")
        expect(page.locator('label[for="single_age_last"]')).to_have_text("Item 2 · Alter bei der letzten Verurteilung")
        expect(page.locator("#single_sex + .selectize-control")).to_contain_text("Männlich")
        expect(page.locator("#single_download")).to_be_visible()
        expect(page.locator("#single_results .csv-export-note")).to_contain_text("UTF-8, Kommas als Trennzeichen und einen Dezimalpunkt")
        assert download("single-de.csv", "single_download", "Single_case").read_bytes() == single_english
        assert export_names["single-en.csv"] == export_names["single-de.csv"]
        expect(page.locator('a[data-value="explore"]')).to_have_text("Ausprobieren")
        expect(page.locator('a[data-value="methods"]')).to_have_text("Methodische Hinweise")
        expect(page.locator('a[data-value="about"]')).to_have_text("Über die App")
        check_brand_spacing()
        page.screenshot(path=str(directory / "single-case-german.png"), full_page=True)
        language("en")
        expect(page.locator("#prob_one")).to_have_text("12.2 %")
        expect(page.locator("#single_sex + .selectize-control")).to_contain_text("Male")
        page.locator("#single_previous").fill("-1")
        page.locator("#single_previous").press("Tab")
        expect(page.locator("#prob_one")).to_have_count(0)
        expect(page.locator("#single_validation")).to_contain_text("must not be negative")
        language("de")
        expect(page.locator("#single_validation")).to_contain_text("nicht negativ")
        expect(page.locator("#single_download")).to_have_count(0)
        language("en")
        print("PASS: bilingual single-case results, CSV guidance, timestamped downloads and validation", flush=True)

        page.locator('a[data-value="batch"]').click()
        expect(page.locator(".tab-pane.active .eyebrow")).to_have_text("DATASET CALCULATION")
        page.locator("#batch_file").set_input_files(ROOT / "examples/example-data.csv")
        expect(page.locator("#column_1")).to_have_count(1)
        expect(page.locator("#batch_validation_summary")).to_contain_text("3 scoreable rows", timeout=30000)
        check_mapping_controls()
        select("column_1", "")
        column_prompt = page.locator("#column_1 + .selectize-control input")
        expect(column_prompt).to_have_attribute("placeholder", "Please select")
        language("de")
        expect(column_prompt).to_have_attribute("placeholder", "Bitte auswählen")
        expect(page.locator(".tab-pane.active .eyebrow")).to_have_text("BERECHNUNG FÜR DATENSÄTZE")
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("Zuordnung der Geschlechts- und Deliktkategorien")
        select("column_1", "Item1")
        expect(page.locator("#batch_validation_summary")).to_contain_text("3 berechenbare Zeilen", timeout=30000)
        check_mapping_controls()
        language("en")
        sex_control = page.locator('#category_mapping select[id^="map_sex_"]').first
        sex_id = sex_control.get_attribute("id")
        sex_value = sex_control.input_value()
        select(sex_id, "")
        category_prompt = page.locator(f"#{sex_id} + .selectize-control input")
        expect(category_prompt).to_have_attribute("placeholder", "Not mapped")
        language("de")
        expect(category_prompt).to_have_attribute("placeholder", "Nicht zugeordnet")
        select(sex_id, sex_value)
        expect(page.locator("#batch_validation_summary")).to_contain_text("3 berechenbare Zeilen", timeout=30000)
        check_mapping_controls()
        page.screenshot(path=str(directory / "dataset-mappings-german.png"), full_page=True)
        language("en")
        select("batch_id", "Case_ID")
        page.locator("#batch_calculate").click()
        expect(page.locator("#batch_download")).to_be_visible()
        expect(page.locator("#batch_status")).to_contain_text("example-data.csv")
        expect(page.locator("#batch_plot img")).to_be_visible(timeout=30000)

        exported = download("export.csv")
        with exported.open(encoding="utf-8-sig", newline="") as file:
            rows = list(csv.DictReader(file))
        assert len(rows) == 3
        assert rows[0]["Case_ID"] == "Example_01"
        assert rows[0]["Status"] == "calculated"
        assert abs(float(rows[0]["Prob_one_year"]) - .12161553504714573) < 1e-12
        example = download("example.csv", "example_download", "Example_data")
        assert example.read_bytes() == (ROOT / "examples/example-data.csv").read_bytes()
        expect(page.locator(".tab-pane.active .csv-export-note")).to_contain_text("Invalid rows include a status")
        print("PASS: upload, mapping, plot, export schema and timestamped result/example downloads", flush=True)

        # Deliberately override a suggested mapping before switching languages.
        mapping_label = page.locator("#category_mapping label").filter(has_text=re.compile(r"^\s*Value:\s*Violence\s*$"))
        mapping_id = mapping_label.locator("xpath=..").locator("select").get_attribute("id")
        select(mapping_id, "Robbery")
        expect(page.locator("#batch_status")).to_contain_text("out of date")
        page.locator("#batch_calculate").click()
        expect(page.locator("#batch_download")).to_be_visible()
        before = download("custom-en.csv").read_bytes()
        language("de")
        expect(page.locator("#batch_validation_summary")).to_contain_text("3 berechenbare Zeilen")
        expect(page.locator("#batch_download")).to_be_visible()
        assert page.evaluate("id => document.getElementById(id).selectize.getValue()", mapping_id) == "Robbery"
        expect(page.locator(f"#{mapping_id} + .selectize-control")).to_contain_text("Raub")
        expect(page.locator("#category_mapping [data-i18n-raw]").filter(has_text=re.compile("^Violence$"))).to_have_text("Violence")
        check_mapping_controls()
        expect(page.locator("#batch_preview")).to_contain_text("Male")
        assert download("custom-de.csv").read_bytes() == before
        assert export_names["custom-en.csv"] == export_names["custom-de.csv"]
        expect(page.locator(".tab-pane.active .csv-export-note")).to_contain_text("Ungültige Zeilen enthalten Status")
        language("en")
        expect(page.locator(f"#{mapping_id} + .selectize-control")).to_contain_text("Robbery")
        expect(page.locator("#batch_download")).to_be_visible()
        print("PASS: language switching preserves custom mappings, raw values and byte-identical exports", flush=True)

        replacement = directory / "new.csv"
        replacement.write_text("Case_ID,Item1,Item2,Item3,Item4,Item5,Item6\nNew,Weiblich,35,35,3,18,Raub\n", encoding="utf-8")
        page.locator("#batch_file").set_input_files(replacement)
        expect(page.locator("#batch_status")).to_contain_text("out of date", timeout=15000)
        expect(page.locator("#batch_download")).to_have_count(0)
        expect(page.locator("#batch_validation_summary")).to_contain_text("1 scoreable row", timeout=30000)
        page.locator("#batch_calculate").click()
        expect(page.locator("#batch_download")).to_be_visible()
        expect(page.locator("#batch_result_preview")).to_contain_text("7.0 %")
        select("batch_decimal", ",")
        expect(page.locator("#batch_status")).to_contain_text("out of date")
        expect(page.locator("#batch_download")).to_have_count(0)
        print("PASS: German input values in English UI and stale-download protection", flush=True)

        long_column = "Principal_current_offence_column_" * 5
        long_file = directory / "long-column.csv"
        with long_file.open("w", encoding="utf-8", newline="") as file:
            writer = csv.writer(file)
            writer.writerow(["Item1", "Item2", "Item3", "Item4", "Item5", long_column])
            writer.writerow(["Male", 35, 35, 2, 20, "Taking & driving away and related offences"])
        page.locator("#batch_file").set_input_files(long_file)
        expect(page.locator("#import_message")).to_contain_text("1 data row", timeout=15000)
        select("column_6", long_column)
        expect(page.locator("#batch_validation_summary")).to_contain_text("1 scoreable row", timeout=30000)
        # Wait for this upload's category, since the previous file also had one row.
        offence_label = page.locator("#category_mapping label").filter(has_text="Taking & driving away and related offences")
        expect(offence_label).to_have_count(1)
        offence_control = offence_label.locator("xpath=..").locator("select")
        offence_id = offence_control.get_attribute("id")
        for lang in ("en", "de"):
            language(lang)
            select(offence_id, "Taking_Driving" if lang == "en" else "Public_order")
            for width in (1440, 1024, 390):
                page.set_viewport_size({"width": width, "height": 1000})
                check_mapping_controls()
            page.screenshot(path=str(directory / f"long-mapping-mobile-{lang}.png"), full_page=True)
            page.set_viewport_size({"width": 1440, "height": 1000})
        language("en")
        print("PASS: translated prompts only for empty fields; long mapping labels fit on desktop and mobile", flush=True)

        page.locator('a[data-value="explore"]').click()
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("use Single case or Dataset")
        expect(page.locator("#explore_plot img")).to_be_visible(timeout=30000)
        select("explore_outcome", "Prob_two_years")
        expect(page.locator("#explore_plot img")).to_be_visible()
        language("de")
        expect(page.locator(".tab-pane.active .intro-copy")).to_contain_text("Reiter „Einzelfall“ oder „Datensatz“")
        expect(page.locator("#explore_status")).to_contain_text("zulässige Modellkombinationen")
        expect(page.locator("#explore_plot img")).to_have_attribute("alt", re.compile("OGRS-3-Modellschätzungen"))
        page.locator('input[name="explore_sexes"][value="m"]').uncheck()
        expect(page.locator("#explore_status")).to_contain_text("mindestens ein Geschlecht")
        language("en")
        expect(page.locator("#explore_status")).to_contain_text("at least one sex")
        page.locator('input[name="explore_sexes"][value="f"]').check()
        expect(page.locator("#explore_plot img")).to_be_visible(timeout=30000)
        page.locator('a[data-value="methods"]').click()
        expect(page.get_by_role("heading", name="OGRS 3: instrument and calculation")).to_be_visible()
        expect(page.locator(".tab-pane.active")).to_contain_text("should not be used as the sole source of information")
        limitation = "No specific prediction of sexual or violent reoffending, or of the severity of a future offence."
        expect(page.get_by_text(limitation, exact=True)).to_have_count(1)
        expect(page.get_by_text(limitation, exact=True)).to_be_visible()
        expect(page.locator(".tab-pane.active")).not_to_contain_text("Code and contact")
        page.locator("#offence_search").fill("§ 242 StGB")
        expect(page.locator("#offence_lookup")).to_contain_text("Theft")
        expect(page.locator("#offence_lookup")).to_contain_text("Diebstahl")
        language("de")
        expect(page.locator("#offence_lookup")).to_contain_text("Vorschrift")
        expect(page.get_by_role("heading", name="OGRS 3: Instrument und Berechnung")).to_be_visible()
        expect(page.locator(".tab-pane.active")).to_contain_text("nicht als alleinige Informationsquelle")
        page.screenshot(path=str(directory / "methods-german.png"), full_page=True)
        page.locator('a[data-value="about"]').click()
        expect(page.get_by_role("heading", name="Über diese App")).to_be_visible()
        expect(page.locator(".tab-pane.active")).to_contain_text("Erweiterungs- und Ergänzungsvorschläge")
        expect(page.locator(".tab-pane.active")).not_to_contain_text("Keine spezifische Prognose")
        expect(page.get_by_role("link", name="VRAG-R-Rechner", exact=True)).to_have_attribute("href", "https://mertenneumann.shinyapps.io/VRAGR_Calculator/")
        expect(page.get_by_role("link", name="GitHub Issues", exact=True)).to_have_attribute("href", "https://github.com/MertenNeumann/OGRS_Calculator/issues")
        expect(page.get_by_role("heading", name="Datenverarbeitung")).to_be_visible()
        expect(page.get_by_role("link", name="Posit-Dokumentation zu Sicherheit und Compliance")).to_be_visible()
        page.screenshot(path=str(directory / "about-german.png"), full_page=True)
        language("en")
        expect(page.get_by_role("heading", name="About this app")).to_be_visible()
        expect(page.locator(".tab-pane.active")).to_contain_text("feature requests and suggestions for additions")
        expect(page.get_by_role("link", name="VRAG-R calculator", exact=True)).to_be_visible()
        page.screenshot(path=str(directory / "about-english.png"), full_page=True)

        for width in (1920, 1280, 1024, 768):
            page.set_viewport_size({"width": width, "height": 1000})
            assert page.evaluate("document.documentElement.scrollWidth <= innerWidth + 1"), width
        page.set_viewport_size({"width": 1440, "height": 1000})
        assert page.locator(".shiny-output-error:not(.shiny-output-error-validation)").count() == 0
        print("PASS: plots, coding reference, separate bilingual Methods/About tabs and responsive layout", flush=True)

        mobile = browser.new_page(viewport={"width": 390, "height": 844}, is_mobile=True, has_touch=True)
        mobile.on("pageerror", lambda exc: errors.append(str(exc)))
        mobile.goto(url, wait_until="networkidle")
        expect(mobile.locator("html")).to_have_attribute("lang", "en")
        mobile.locator("#single_example").click()
        expect(mobile.locator("#example_notice")).to_contain_text("Example loaded")
        mobile.locator("#single_calculate").click()
        expect(mobile.locator("#prob_one")).to_have_text("12.2 %")
        assert mobile.evaluate("document.documentElement.scrollWidth <= innerWidth + 1")
        mobile.screenshot(path=str(directory / "single-case-mobile-english.png"), full_page=True)
        mobile.locator("button.navbar-toggle, button.navbar-toggler").click()
        expect(mobile.locator("#language")).to_be_visible()
        expect(mobile.locator(".navbar-collapse")).not_to_have_class(re.compile(r"\bcollapsing\b"))
        mobile.locator("#language").select_option("de")
        expect(mobile.locator("html")).to_have_attribute("lang", "de")
        expect(mobile.locator("#prob_one")).to_have_text("12,2 %")
        expect(mobile.locator('a[data-value="explore"]')).to_have_text("Ausprobieren")
        expect(mobile.locator('a[data-value="about"]')).to_be_visible()
        mobile.locator("button.navbar-toggle, button.navbar-toggler").click()
        expect(mobile.locator("#language")).not_to_be_visible()
        assert mobile.evaluate("document.documentElement.scrollWidth <= innerWidth + 1")
        mobile.screenshot(path=str(directory / "single-case-mobile-german.png"), full_page=True)
        assert not errors, errors
        browser.close()
        print("PASS: mobile language dropdown and independent session defaults; no JavaScript errors", flush=True)


def main():
    with tempfile.TemporaryDirectory(prefix="ogrs-browser-") as temporary:
        directory = Path(temporary)
        with socket.socket() as sock:
            sock.bind(("127.0.0.1", 0))
            port = sock.getsockname()[1]
        rscript = os.environ.get("OGRS_RSCRIPT", "Rscript")
        log = (directory / "server.log").open("w+")
        process = subprocess.Popen([rscript, "-e", f'shiny::runApp(".", host="127.0.0.1", port={port}L, launch.browser=FALSE)'],
                                   cwd=ROOT, stdout=log, stderr=subprocess.STDOUT)
        url = f"http://127.0.0.1:{port}/"
        opener = build_opener(ProxyHandler({}))  # Only the local server started by this test.
        try:
            for _ in range(120):
                if process.poll() is not None:
                    raise RuntimeError("The Shiny server exited before becoming ready")
                try:
                    with opener.open(url, timeout=1) as response:
                        if response.status == 200:
                            break
                except OSError:
                    time.sleep(.25)
            else:
                raise RuntimeError("The Shiny server did not become ready in time")
            run_browser(url, directory)
            screenshots = os.environ.get("OGRS_SCREENSHOTS")
            if screenshots:
                import shutil
                output = Path(screenshots)
                output.mkdir(parents=True, exist_ok=True)
                for path in directory.glob("*.png"):
                    shutil.copy(path, output / path.name)
        except Exception:
            log.flush();log.seek(0)
            print(log.read())
            raise
        finally:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill();process.wait()
            log.close()
    print("ALL BROWSER CHECKS PASSED")


if __name__ == "__main__":
    main()
