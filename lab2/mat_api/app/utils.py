import subprocess


def start_haskel_mat(file_path: str = "app/output/automaton.txt"):
    process = subprocess.Popen(
        ['./mat_linux'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    file_path = "app/output/automaton.txt"
    process.stdin.write(f"{file_path}\n")
    process.stdin.flush()
    return process

start_haskel_mat()
