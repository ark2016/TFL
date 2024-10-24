import subprocess
import sys

class MAT:
    def __init__(self, path_to_mat):
        self.pass_code = "1"
        self.error_code = "Invalid input. Please enter a valid equivalence table."

        try:
            self.process = subprocess.Popen(
                path_to_mat,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1
            )
        except Exception as e:
            print(f"Ошибка инициализации MAT.exe: {e}") 
    
    def membership_query(self, input_str):
        if self.process is None:
            print("Ошибка: процесс MAT не инициализирован.")
            sys.exit(1)  
        
        command = f"1\n{input_str}\n"
        try:
            self.process.stdin.write(command)
            self.process.stdin.flush()
            result = self.process.stdout.readline().strip()
            if result == "1":
                return True
            elif result == "0":
                return False
            else:
                print(f"Ошибка: неожиданный ответ от MAT: {result}")
                raise ValueError(f"Некорректный ответ от MAT: {result}")
        except Exception as e:
            print(f"Ошибка при выполнении запроса: {e}") 

    def equivalence_query(self, hypothesis):
        if self.process is None:
            print("Ошибка: процесс MAT не инициализирован.")
            sys.exit(1) 
        
        command = f"2\n{str(hypothesis)}\n"
        try:
            self.process.stdin.write(command)
            self.process.stdin.flush()
            response = self.process.stdout.readline().strip()
            
            if response == self.pass_code:
                return True, None 
            else:
                if response == self.error_code: 
                    print("Ошибка: Invalid Input")
                return False, response  
        except Exception as e:
            print(f"Ошибка при выполнении запроса: {e}") 
    
    def close(self):
        if self.process is None:
            print("Ошибка: процесс MAT не инициализирован.")
            sys.exit(1)
        
        try:
            self.process.stdin.write("4\n")
            self.process.stdin.flush()
            self.process.wait()
            print("Exited MAT.exe")
        except Exception as e:
            print(f"Ошибка при закрытии MAT.exe: {e}")

def main():
    path_to_mat = r'C:\\Users\\a.korovkin\\TFL\\lab2\\mat.exe'
    mat = MAT(path_to_mat)

    strings_to_check = ["abc", "a", "eewq"]
    
    for input_str in strings_to_check:
        result = mat.membership_query(input_str)
        print(f"Результат для '{input_str}': {result}")
    
    hypothesis = [("class_name1", "state1"), ("class_name2", "state2")] 
    equivalent, counterexample = mat.equivalence_query(hypothesis)
    if equivalent:
        print("Гипотеза эквивалентна.")
    else:
        print(f"Гипотеза не эквивалентна. Контрпример: {counterexample}")

    mat.close()

if __name__ == "__main__":
    main()
