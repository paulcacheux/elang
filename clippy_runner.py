import subprocess

proc = subprocess.Popen(["cargo", "clippy"], stderr=subprocess.PIPE)
output = proc.stderr.read().decode("utf-8")

groups = [group for group in output.split("\n\n") if "parser.rs" not in group]

print("\n-----------------\n".join(groups[:20]))
