from pathlib import Path
from subprocess import Popen, PIPE

from badges import *

if __name__ == "__main__":
    from tempfile import TemporaryFile
    from re import search, finditer, match, IGNORECASE
    import sys

    args = sys.argv[1:]
    try:
        project = args[0]
    except:
        raise ValueError("Expected at least two arguments: '<repo>'")

    primary = "9cf"
    if len(args) >= 2:
        primary = args[1]
    
    project_badges: list[tuple[str, str, Parameters]] = [
        (
            "license",
            f"github/license/tired-fox/{project}.svg",
            {"style": "flat-square", "color": primary}
        ),
        (
            "maintained",
            f"badge/maintained-yes-{primary}.svg",
            {"style": "flat-square"}
        ),
        (
            "built_with_love",
            "badge/Built_With-❤-D15D27",
            {"style": "for-the-badge", "labelColor": "E26D25"}
        ),
    ]

    def _get_test_links() -> list[tuple[Name, Url]]:
        passed, failed = 0, 0
        with TemporaryFile() as file:
            data = Popen(f'cargo test --all-features -v', stdout=file, stderr=PIPE)
            data.wait()
            file.seek(0)
            output = file.read().decode()

        for line in output.split("\n"):
            if (result := search(r"test result: [A-Za-z]+\. (\d+) passed; (\d+) failed", line)) is not None:
                result: tuple[int, int] = result.groups()
                p, f = result

                passed += int(p)
                failed += int(f)

        total = passed + failed
        test_link = sheild_io_link(
            Create.badge(
                "tests",
                f"{passed}/{total}",
                Color.percentage(passed / total if total > 0 and passed > 0 else 0),
            ),
            {
                "style": "flat-square",
                "logo": "testcafe",
                "logoColor": "white",
            },
        )

        return [("tests", test_link)]


    def _get_version_link() -> list[tuple[Name, Url]]:
        try:
            from tomllib import loads
        except:
            from toml import loads

        cargo_toml = Path("cargo.toml")
        version = "0.1.0"
        if cargo_toml.exists():
            with cargo_toml.open("r", encoding="utf-8") as file:
                data = loads(file.read())
                version = data.get("package", {"version": "0.1.0"}).get("version", "0.1.0")
        version_link = sheild_io_link(
            Create.badge(
                "version",
                version,
                primary,
            ),
            {
                "style": "flat-square",
                "logo": "aiohttp",
                "logoColor": "white",
            },
        )

        return [("version", version_link)]

    badges = Badges(_get_test_links, _get_version_link)

    for badge in project_badges:
        badges.badge(*badge)

    badges.collect("assets/badges/")
    header_badges = f"""\
<!-- Header Badges -->

<div align="center">
  
<img src="assets/badges/version.svg" alt="Version"/>
<a href="https://github.com/Tired-Fox/{project}/releases" alt="Release"><img src="https://img.shields.io/github/v/release/tired-fox/{project}.svg?style=flat-square&color=9cf"/></a>
<a href="https://github.com/Tired-Fox/{project}/blob/main/LICENSE" alt="License"><img src="assets/badges/license.svg"/></a>
<br>
<img src="assets/badges/maintained.svg" alt="Maintained"/>
<img src="assets/badges/tests.svg" alt="Tests"/>
  
</div>

<!-- End Header -->\
"""
    footer_badges = """\
<!-- Footer Badges --!>

<br>
<div align="center">
  <img src="assets/badges/made_with_rust.svg" alt="Made with rust"/>
  <img src="assets/badges/built_with_love.svg" alt="Built with love"/>
</div>

<!-- End Footer -->\
"""
    print("Copying badge: made_with_rust")
    Path("assets/badges/made_with_rust.svg").write_text(PRESETS["made_with_rust"])

    readme = [path for path in Path("").glob("*.md") if path.as_posix().lower() == "readme.md"]
    readme = readme[0] if len(readme) > 0 else None

    if readme is not None:
        lines = readme.read_text().split("\n")
        idx = 0
        while idx < len(lines):
            if match(r"\s*<!--\s*Header\s*Badges\s*-->\s*", lines[idx], IGNORECASE) is not None:
                entry = idx
                end = idx
                while idx < len(lines):
                    if match(r"\s*<!--\s*End\s*Header\s*-->\s*", lines[idx], IGNORECASE) is not None: 
                        end = idx
                        break
                    idx += 1
                else:
                    end = entry
                lines[entry:end+1] = header_badges.split("\n")
                break
            idx += 1

        idx = 0
        while idx < len(lines):
            if match(r"\s*<!--\s*Footer\s*Badges\s*-->\s*", lines[idx], IGNORECASE) is not None:
                entry = idx
                end = idx
                while idx < len(lines):
                    if match(r"\s*<!--\s*End\s*Footer\s*-->\s*", lines[idx], IGNORECASE) is not None: 
                        end = idx
                        break
                    idx += 1
                else:
                    end = entry
                lines[entry:end+1] = footer_badges.split("\n")
                break
            idx += 1

        readme.write_text("\n".join(lines))
        print("\x1b[1mUpdated README.md\x1b[22m")
    else:
        print(f"\x1b[1mHeader Badges:\x1b[22m\n{header_badges}")
        print(f"\x1b[1mFooter Badges:\x1b[22m\n{footer_badges}")
