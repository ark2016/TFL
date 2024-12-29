```bash
cabal clean
```
```bash
cabal build
```

---

для тестов
```bash
cabal test
```

---

для запуска испольняемого файла 
```bash
cabal run fuzzModule
```

---

для запуска сервера
```bash
cabal run lab3-server
```
для запроса на сервер с записью в файл `res.json`

```cmd
curl.exe -X POST -H "Content-Type: application/json" -d '{\"grammarInput\": \"S -> a S\nS -> b\",\"maxDepth\": 4,\"numNegatives\": 3}' http://localhost:8080 > res.json
```

```json
{
  "grammarInput": "S -> a S\nS -> b",
  "maxDepth": 4,
  "numNegatives": 3
}
```
```cmd
curl.exe -X POST -H "Content-Type: application/json" -d '{\"grammarInput\": \"S -> A B\nA -> a\n A-> A A\nB -> b \n B-> B B\", \"maxDepth\": 6, \"numNegatives\": 5}' http://localhost:8080 > res.json
```