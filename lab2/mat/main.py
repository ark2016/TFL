from automaton import *
import subprocess


def interact_with_mat(automaton):
    try:
        # Start mat.exe process
        process = subprocess.Popen(
            ['mat.exe'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
            bufsize=1
        )

        file_path = input("Enter file path (ex. automaton.txt):  ")
        process.stdin.write(f"{file_path}\n")
        process.stdin.flush()

        while True:
            print("\nSelect option:")
            print("1 - Check string inclusion")
            print("2 - Check automaton equivalence")
            print("3 - Visualize automaton")
            print("4 - Exit")

            option = input("Enter option: ")

            process.stdin.write(f"{option}\n")
            process.stdin.flush()

            if option == "4":
                print("Exiting program")
                break
            elif option == "1":
                test_string = input("Enter string to check: ")
                process.stdin.write(f"{test_string}\n")
                process.stdin.flush()
                result = process.stdout.readline().strip()
                print(result)
                print(f"Result: {'Accepted' if result == '1' else 'Not accepted'}")
            elif option == "2":
                eq_table = input("Enter equivalence table: ")
                process.stdin.write(f"{eq_table}\n")
                process.stdin.flush()
                result = process.stdout.readline().strip()
                print(result)
                print(f"Result: {'Equivalent' if result == '1' else 'Not equivalent'}")
            elif option == "3":
                result = process.stdout.readline().strip()
                visualize_dfa(automaton,"lab2")
                print("Visualization completed")
            else:
                result = process.stdout.readline().strip()
                print(f"Error: {result}")

        process.wait()

    except Exception as e:
        print(f"Error: {e}")
    finally:
        if 'process' in locals():
            process.terminate()


if __name__ == "__main__":
    automat_generator = AutomatGenerator("lab2")
    automat = automat_generator.gen_random_program_Automat()
    automat_dict = convert_Automat_to_dict(automat)
    save_to_file("automaton.txt", automat_dict)
    interact_with_mat(automat)