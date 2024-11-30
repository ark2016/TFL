import os
import subprocess


def start_haskel_mat(file_path: str = "app/output/automaton.txt", option: int = 1):
    # Абсолютный путь до текущего файла
    current_dir = os.path.dirname(os.path.abspath(__file__))
    file_path_haskell = os.path.join(current_dir, 'mat_linux')
    process = subprocess.Popen(
        ['./app/mat_linux'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
        bufsize=1
    )
    file_path = "app/output/automaton.txt"

    process.stdin.write(f"{file_path}\n")
    process.stdin.flush()

    process.stdin.write(f"{option}\n")
    process.stdin.flush()

    return process

# start_haskel_mat()
