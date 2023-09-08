if __name__ == "__main__":
    import sys
    import os
    from pathlib import Path

    args = sys.argv[1:]
    if len(args) < 1:
        raise ValueError("Expected one arguments: '<repo>'")

    repo = args[0]

    if not Path("cargo.toml").exists():
        os.system("cargo init")

    Path("Makefile").write_text(f"""\
lint:
	cargo clippy
fix:
	cargo clippy --fix

ex:
	cargo run --example $(EX) -- $(ARGS)

test:
	cargo test --all-features --color=auto -v

test-doc:
	cargo test --doc --all-features --color=auto -v

badges:
	python scripts/make_badges.py {repo}
"""
)
    # pInit.parent.mkdir(exist_ok=True, parents=True)
    Path("README.md").write_text(f"""\
# {repo} 

<!-- Header Badges -->
<!-- End Header -->

<!-- Footer Badges -->
<!-- End Footer -->
""")

    os.system("make badges")
