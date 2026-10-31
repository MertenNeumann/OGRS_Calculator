"""Local end-to-end tests using synthetic data in English and German."""
import csv
import os
import re
import socket
import subprocess
import tempfile
import time
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
        expect(page.locator("html")).to_have_attribute("lang", "en")
        expect(page.locator("#language")).to_have_value("en")
        expect(page.get_by_role("heading", name="Estimate the risk of general reoffending.")).to_be_visible()
        expect(page.locator("#prob_one")).to_have_count(0)
        page.locator("#single_example").click()
        expect(page.locator("#example_notice")).to_contain_text("Example loaded")
        page.locator("#single_calculate").click()
        expect(page.locator("#prob_one")).to_have_text("12.2 %")
        expect(page.locator("#prob_two")).to_have_text("22.1 %")
        expect(page.locator("#copas_value")).to_have_text("-2.303")
        page.screenshot(path=str(directory / "single-case-english.png"), full_page=True)

        def select(id_, value):
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
        expect(page.locator('label[for="single_age_last"]')).to_have_text("Item 2 · Alter bei der letzten Verurteilung")
        expect(page.locator("#single_sex + .selectize-control")).to_contain_text("Männlich")
        expect(page.locator("#single_download")).to_be_visible()
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
        print("PASS: English default; bilingual single-case results and validation without input loss", flush=True)

        page.locator('a[data-value="batch"]').click()
        page.locator("#batch_file").set_input_files(ROOT / "examples/example-data.csv")
        expect(page.locator("#column_1")).to_have_count(1)
        expect(page.locator("#batch_validation_summary")).to_contain_text("3 scoreable rows", timeout=30000)
        select("batch_id", "Case_ID")
        page.locator("#batch_calculate").click()
        expect(page.locator("#batch_download")).to_be_visible()
        expect(page.locator("#batch_status")).to_contain_text("example-data.csv")
        expect(page.locator("#batch_plot img")).to_be_visible(timeout=30000)

        def download(name):
            with page.expect_download() as info:
                page.locator("#batch_download").click()
            destination = directory / name
            info.value.save_as(destination)
            return destination

        exported = download("export.csv")
        with exported.open(encoding="utf-8-sig", newline="") as file:
            rows = list(csv.DictReader(file))
        assert len(rows) == 3
        assert rows[0]["Case_ID"] == "Example_01"
        assert rows[0]["Status"] == "calculated"
        assert abs(float(rows[0]["Prob_one_year"]) - .12161553504714573) < 1e-12
        print("PASS: real upload, mapping, plot and English export schema", flush=True)

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
        expect(page.locator("#batch_preview")).to_contain_text("Male")
        assert download("custom-de.csv").read_bytes() == before
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

        page.locator('a[data-value="explore"]').click()
        expect(page.locator("#explore_plot img")).to_be_visible(timeout=30000)
        select("explore_outcome", "Prob_two_years")
        expect(page.locator("#explore_plot img")).to_be_visible()
        language("de")
        expect(page.locator("#explore_status")).to_contain_text("zulässige Modellkombinationen")
        expect(page.locator("#explore_plot img")).to_have_attribute("alt", re.compile("OGRS-3-Modellschätzungen"))
        page.locator('input[name="explore_sexes"][value="m"]').uncheck()
        expect(page.locator("#explore_status")).to_contain_text("mindestens ein Geschlecht")
        language("en")
        expect(page.locator("#explore_status")).to_contain_text("at least one sex")
        page.locator('input[name="explore_sexes"][value="f"]').check()
        expect(page.locator("#explore_plot img")).to_be_visible(timeout=30000)
        page.locator('a[data-value="methods"]').click()
        page.locator("#offence_search").fill("§ 242 StGB")
        expect(page.locator("#offence_lookup")).to_contain_text("Theft")
        expect(page.locator("#offence_lookup")).to_contain_text("Diebstahl")
        language("de")
        expect(page.locator("#offence_lookup")).to_contain_text("Vorschrift")
        assert page.locator(".shiny-output-error:not(.shiny-output-error-validation)").count() == 0
        print("PASS: bilingual plots, accessibility text, empty selections and coding reference", flush=True)

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
